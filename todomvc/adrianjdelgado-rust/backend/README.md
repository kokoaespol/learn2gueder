# TODO App

## Setup

1. Declare the database URL

```sh
export DATABASE_URL="sqlite:todos.db"
```

2. Create the database.

```sh
sqlx db create
```

3. Run sql migrations

```sh
sqlx migrate run
```
