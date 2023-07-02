package io.github.aloussase.todomvc.domain.todo.interfaces

import cats.Monad

import io.github.aloussase.todomvc.domain.todo.model._

trait TodoRepository {

  /**
   * Save the provided [[Todo]] in the repository.
   */
  def save[F[_]: Monad](todo: Todo): F[Boolean]

  /**
   * Return a list of [[Todo]]s from this repository.
   */
  def findAll[F[_]: Monad]: F[List[Todo]]

  /**
   * Delete the [[Todo]] corresponding to the provided id.
   */
  def delete[F[_]: Monad](todoId: Int): F[Boolean]
  
}
