package io.github.aloussase.todomvc.api

import cats.{ Monad, MonadThrow }
import cats.effect._
import cats.syntax.all._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.ember.server._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpApp

import io.github.aloussase.todomvc.data.access.todo._

import io.github.aloussase.todomvc.domain.todo.services._
import io.github.aloussase.todomvc.api.serdes.TodoSerdes._
import io.github.aloussase.todomvc.api.contracts.todo._
import io.github.aloussase.todomvc.domain.todo.model._

object Main extends IOApp {
  def healthCheckApi[F[_]: Monad] = {
    val dsl = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "healthcheck" => 
        Ok()
    }
  }

  def todoApi[F[_]: Concurrent](implicit service: TodoService) = {
    val dsl = Http4sDsl[F]
    import dsl._
    
    implicit val newTodoDecoder = jsonOf[F, NewTodo]

    HttpRoutes.of[F] {
      case GET -> Root / "api" / "todos" =>
        service.listTodos[F]
          .map(todos => todos.asJson)
          .flatMap(json => Ok(json))
      
      case DELETE -> Root / "api" / "todos" / IntVar(todoId) =>
        service.deleteTodo[F](todoId)
          .flatMap(_ => NoContent())

      case req @ POST -> Root / "api" / "todos" => 
        for {
          newTodo <- req.as[NewTodo]
          result <- service.createTodo[F](newTodo.id, newTodo.content)
          resp <- result match {
            case Left(validations) =>
              BadRequest(validations.asJson)
            case Right(todo) =>
              Created(todo.asJson)
          }
        } yield resp
    }
  }

  def app[F[_]: Concurrent](implicit todoService: TodoService): HttpApp[F] = 
    ( healthCheckApi <+> todoApi
      ).orNotFound

  def run(args: List[String]): IO[ExitCode] = {
    import com.softwaremill.macwire._

    lazy val todoRepository = wire[InMemoryTodoRepository]
    implicit lazy val todoServiceImpl = wire[TodoServiceImpl]

    EmberServerBuilder
      .default[IO]
      .withHttpApp(app)
      .build
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }
}
