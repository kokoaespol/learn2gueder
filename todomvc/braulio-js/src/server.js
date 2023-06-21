const express = require('express');
const bodyParser = require('body-parser');
const TodoController = require('./controllers/todoController');

const app = express();
const port = process.env.PORT || 3000;

app.use(bodyParser.json());

const todoController = new TodoController();

app.get('/todos', todoController.getAllTodos.bind(todoController));
app.post('/todos', todoController.addTodo.bind(todoController));

app.listen(port, () => {
  console.log(`ToDo app listening on http://localhost:${port}`)  
});
