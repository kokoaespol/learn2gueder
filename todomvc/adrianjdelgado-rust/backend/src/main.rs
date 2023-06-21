use axum::{
    error_handling::HandleErrorLayer,
    http::StatusCode,
    routing::{get, patch},
    Router,
};
use eyre::Result;
use sqlx::SqlitePool;
use std::{net::SocketAddr, time::Duration};
use tower::{BoxError, ServiceBuilder};
use tower_http::trace::TraceLayer;

use crate::routes::{todos_create, todos_delete, todos_index, todos_update};

mod models;
mod routes;
mod telemetry;

#[tokio::main]
async fn main() -> Result<()> {
    telemetry::init_subscriber();

    let pool = SqlitePool::connect("sqlite:todos.db").await?;
    sqlx::migrate!().run(&pool).await?;

    // Compose the routes
    let app = Router::new()
        .route("/todos", get(todos_index).post(todos_create))
        .route("/todos/:id", patch(todos_update).delete(todos_delete))
        // Add middleware to all routes
        .layer(
            ServiceBuilder::new()
                .layer(HandleErrorLayer::new(|error: BoxError| async move {
                    if error.is::<tower::timeout::error::Elapsed>() {
                        Ok(StatusCode::REQUEST_TIMEOUT)
                    } else {
                        Err((
                            StatusCode::INTERNAL_SERVER_ERROR,
                            format!("Unhandled internal error: {error}"),
                        ))
                    }
                }))
                .timeout(Duration::from_secs(10))
                .layer(TraceLayer::new_for_http())
                .into_inner(),
        )
        .with_state(pool);

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    tracing::debug!("listening on {}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}
