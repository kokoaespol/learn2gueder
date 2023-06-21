import {v4 as uuidv4} from 'uuid';
import {Router} from "express"

import {Todo} from '../model/todo.js';

export const todos = (db) => {
    const todosRouter = new Router();

    todosRouter.post("/", async (req, res) => {
        const todoId = uuidv4();
        const content = req.body.content;

        const todo = new Todo({
            id: todoId,
            content: content,
        });

        const ok = await todo.save(db);

        if (ok) {
            return res.status(200).json(todo);
        }

        return res.status(400).end();
    });

    todosRouter.get("/", async (_req, res) => {
        try {
            const todos = await Todo.findAll(db);
            return res.status(200).json(todos);
        } catch (err) {
            console.error(err);
            return res.status(400).end();
        }

    })

    return todosRouter;
}
