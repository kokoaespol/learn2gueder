package io.github.aloussase.todomvc.api.serdes

import io.circe.{ Decoder, Encoder, HCursor, Json }
import org.http4s.circe._
import cats.effect.IO
import io.circe.syntax._

import io.github.aloussase.todomvc.domain.todo.model._
import io.github.aloussase.todomvc.domain.todo.services._
import io.github.aloussase.todomvc.api.contracts.todo._

object TodoSerdes {
  implicit val decodeNewTodo: Decoder[NewTodo] = new Decoder[NewTodo] {
    final def apply(c: HCursor): Decoder.Result[NewTodo] = 
      for {
        id <- c.downField("id").as[Int]
        content <- c.downField("content").as[String]
      } yield NewTodo(id, content)
  }

  implicit val encodeTodo: Encoder[Todo] = new Encoder[Todo] {
    final def apply(todo: Todo): Json = Json.obj(
      ("id", Json.fromInt(todo.id)),
      ("content", Json.fromString(todo.content)),
      ("completed", Json.fromBoolean(todo.completed))
    )
  }

  implicit val encodeTodoValidation: Encoder[TodoValidation] = new Encoder[TodoValidation] {
    final def apply(validation: TodoValidation): Json = validation match {
      case InvalidTodoId(todoId) => Json.obj(
        ("type", Json.fromString("InvalidTodoId")),
        ("detail", Json.fromInt(todoId))
      )
      case TodoContentEmpty() => Json.obj(
        ("type", Json.fromString("TodoContentEmpty")),
      )
    }
  }

  implicit val encodeTodoServiceError: Encoder[TodoServiceError] = new Encoder[TodoServiceError] {
    final def apply(error: TodoServiceError): Json = error match {
      case FailedToCreateTodo(todo) => Json.obj(
        ("error", Json.fromString("Failed to create todo")),
        ("detail", todo.asJson)
      )
      case TodoValidationError(validations) => Json.obj(
        ("error", Json.fromString("Invalid todo data")),
        ("detail", validations.asJson)
      )
    }
  }
}
