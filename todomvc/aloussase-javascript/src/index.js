import express from "express"
import bodyParser from "body-parser";
import sqlite3 from "sqlite3"

import {healthcheck} from "./controller/healthcheck.js";
import {todos} from "./controller/todos.js";
import {Todo} from "./model/todo.js";

const app = express();
const db = new sqlite3.Database("./todos.db");

(async function () {
    app.use(bodyParser.json());

    app.use(express.static('public'))

    app.use("/healthcheck", healthcheck());
    app.use("/todos", todos(db));

    Todo.createTable(db);

    const port = process.env.PORT || 3000;

    app.listen(port, () => {
        console.log(`server listening on port: ${port}`)
    })
})()

