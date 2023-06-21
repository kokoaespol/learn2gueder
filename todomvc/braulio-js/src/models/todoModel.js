const sqlite3 = require("sqlite3").verbose();

class TodoModel {
    constructor() {
        this.db = new sqlite3.Database(":memory:");

        this.db.run(
            "CREATE TABLE IF NOT EXISTS todos (id INTEGER PRIMARY KEY AUTOINCREMENT, task TEXT)"
        );
    }

    getAllTodos(callback) {
        this.db.all("SELECT * FROM todos", (err, rows) => {
            if (err) {
                console.error(err);
                callback(err, null);
            } else {
                callback(null, rows);
            }
        });
    }

    addTodo(task, callback) {
        this.db.run(
            "INSERT INTO todos (task) VALUES (?)",
            [task],
            function (err) {
                if (err) {
                    console.error(err);
                    callback(err, null);
                } else {
                    callback(null, { id: this.lastID, task: task });
                }
            }
        );
    }
}

module.exports = TodoModel;