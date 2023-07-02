package io.github.aloussase.todomvc.domain.todo.services

import cats.Monad
import cats.syntax.all._

import io.github.aloussase.todomvc.domain.todo.model._
import io.github.aloussase.todomvc.domain.todo.interfaces._

case class TodoServiceImpl(todoRepository: TodoRepository) extends TodoService {
  def createTodo[F[_]: Monad] (todoId: Int, todoContent: String) = 
    Todo.create(todoId, todoContent) match {
      case validations@Left(_) => 
        Monad[F].pure(
          validations
            .leftMap(vs => TodoValidationError(vs.toNonEmptyList.toList))
        )
      case Right(todo) =>
        todoRepository.save(todo)
          .map { success =>
            if (success)  todo.asRight
            else FailedToCreateTodo(todo).asLeft
          }
    }

  def listTodos[F[_]: Monad] = 
    todoRepository.findAll

  def deleteTodo[F[_]: Monad](todoId: Int) =
    todoRepository.delete(todoId)
}
