package io.github.aloussase.todomvc.data.access.todo

import cats.Monad
import cats.syntax.all._

import io.github.aloussase.todomvc.domain.todo.model._
import io.github.aloussase.todomvc.domain.todo.interfaces._

final class InMemoryTodoRepository extends TodoRepository {
  
  private var todos: Map[Int, Todo] = Map()

  override def save[F[_]: Monad](todo: Todo) = {
    todos = todos + ( todo.id -> todo )
    true.pure
  }

  override def delete[F[_]: Monad](todoId: Int): F[Boolean] = todos.get(todoId) match {
    case Some(todo) =>
      todos = todos.filter { case (id, _) => id != todoId } 
      true.pure
    case None =>
      false.pure
  }

  override def findAll[F[_]: Monad]: F[List[Todo]] =
    todos.values.toList.pure
 
}
