const TodoModel = require("../models/todoModel");

class TodoController {
    constructor() {
        this.todoModel = new TodoModel();
    }

    getAllTodos(req, res) {
        this.todoModel.getAllTodos((err, todos) => {
            if (err) {
                res.sendStatus(500);
            } else {
                res.json(todos);
            }
        });
    }

    addTodo(req, res) {
        const task = req.body.content;

        this.todoModel.addTodo(task, (err, todo) => {
            if (err) {
                res.sendStatus(500);
            } else {
                res.json(todo);
            }
        })
    }
}

module.exports = TodoController;
