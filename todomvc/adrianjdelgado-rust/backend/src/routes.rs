use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};
use uuid::Uuid;

use crate::models::{CreateTodo, Db, Todo, UpdateTodo};

pub async fn todos_index(State(db): State<Db>) -> Result<Json<Vec<Todo>>, StatusCode> {
    let todos = sqlx::query_as!(
        Todo,
        r#"SELECT id AS "id: Uuid", task, completed from todos"#
    )
    .fetch_all(&db)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok(Json(todos))
}

pub async fn todos_create(
    State(db): State<Db>,
    Json(input): Json<CreateTodo>,
) -> Result<(StatusCode, Json<Todo>), StatusCode> {
    let todo = Todo {
        id: Uuid::new_v4(),
        task: input.task,
        completed: false,
    };

    sqlx::query!(
        r#"INSERT into todos (id, task, completed) VALUES (?1, ?2, ?3)"#,
        todo.id,
        todo.task,
        todo.completed
    )
    .execute(&db)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok((StatusCode::CREATED, Json(todo)))
}

// TODO: use transactions (?)
pub async fn todos_update(
    Path(id): Path<Uuid>,
    State(db): State<Db>,
    Json(input): Json<UpdateTodo>,
) -> Result<Json<Todo>, StatusCode> {
    let Some(mut todo) = sqlx::query_as!(
        Todo,
        r#"SELECT id AS "id: Uuid", task, completed from todos WHERE id = ?1"#,
        id
    )
    .fetch_optional(&db)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)? else {
        return Err(StatusCode::NOT_FOUND)
    };

    if let Some(task) = input.task {
        todo.task = task;
    }

    if let Some(completed) = input.completed {
        todo.completed = completed;
    }

    sqlx::query!(
        r#"
            UPDATE todos
            SET task = ?2, completed = ?3
            WHERE id = ?1
        "#,
        id,
        todo.task,
        todo.completed
    )
    .execute(&db)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok(Json(todo))
}

pub async fn todos_delete(
    Path(id): Path<Uuid>,
    State(db): State<Db>,
) -> Result<StatusCode, StatusCode> {
    let rows_affected = sqlx::query!(r#"DELETE FROM todos WHERE id = ?1"#, id)
        .execute(&db)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
        .rows_affected();
    Ok(if rows_affected > 0 {
        StatusCode::NO_CONTENT
    } else {
        StatusCode::NOT_FOUND
    })
}
