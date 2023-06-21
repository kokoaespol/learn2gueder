use serde::{Deserialize, Serialize};
use sqlx::SqlitePool;
use uuid::Uuid;

pub type Db = SqlitePool;

#[derive(Debug, Serialize, Clone)]
pub struct Todo {
    pub id: Uuid,
    pub task: String,
    pub completed: bool,
}

#[derive(Debug, Deserialize)]
pub struct CreateTodo {
    pub task: String,
}

#[derive(Debug, Deserialize)]
pub struct UpdateTodo {
    pub task: Option<String>,
    pub completed: Option<bool>,
}
