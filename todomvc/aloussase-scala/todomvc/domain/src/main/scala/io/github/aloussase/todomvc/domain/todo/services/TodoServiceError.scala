package io.github.aloussase.todomvc.domain.todo.services

import io.github.aloussase.todomvc.domain.todo.model._

sealed trait TodoServiceError
case class FailedToCreateTodo(todo: Todo) extends TodoServiceError
case class TodoValidationError(validations: List[TodoValidation]) extends TodoServiceError
