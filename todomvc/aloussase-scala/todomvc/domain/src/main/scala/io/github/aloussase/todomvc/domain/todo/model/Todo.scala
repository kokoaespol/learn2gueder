package io.github.aloussase.todomvc.domain.todo.model

import cats.data._
import cats.data.Validated._
import cats.syntax.all._

sealed case class Todo private(
  id: Int,
  content: String,
  completed: Boolean
)

object Todo {
  type ValidationResult[A] = ValidatedNec[TodoValidation, A]

  def create(todoId: Int, content: String, completed: Boolean = false):
    Either[NonEmptyChain[TodoValidation], Todo] =
    ( validateId(todoId)
    , validateContent(content) )
      .mapN((id, content) => Todo(id, content, completed))
      .toEither

  private def validateId(todoId: Int): ValidationResult[Int] =
    if (todoId < 0) InvalidTodoId(todoId).invalidNec
    else todoId.validNec

  private def validateContent(todoContent: String): ValidationResult[String] =
    if (todoContent.isEmpty) TodoContentEmpty().invalidNec
    else todoContent.validNec
}
