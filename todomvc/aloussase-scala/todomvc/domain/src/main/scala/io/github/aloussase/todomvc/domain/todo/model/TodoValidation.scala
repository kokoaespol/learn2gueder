package io.github.aloussase.todomvc.domain.todo.model

sealed trait TodoValidation
case class InvalidTodoId(id: Int) extends TodoValidation
case class TodoContentEmpty() extends TodoValidation
