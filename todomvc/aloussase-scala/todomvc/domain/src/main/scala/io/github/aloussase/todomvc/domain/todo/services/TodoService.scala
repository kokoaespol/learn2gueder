package io.github.aloussase.todomvc.domain.todo.services

import cats.data._
import cats.Monad

import io.github.aloussase.todomvc.domain.todo.model._

trait TodoService {
  
  /**
   * Create a new @Todo@.
   */
  def createTodo[F[_]: Monad](todoId: Int, todoContent: String): F[Either[TodoServiceError, Todo]]

  /**
   * Delete the todo corresponding to the given @todoId@.
   */
  def deleteTodo[F[_]: Monad](todoId: Int): F[Boolean]

  /**
   * Get all @Todo@s.
   */
  def listTodos[F[_]: Monad]: F[List[Todo]]

}
