import TodoList from "../todolist/todolist.component.js";
import CreateTodo from "../createtodo/createtodo.component.js";

export default {
    render(props) {
        return `
            ${this.renderHtml(props)}
            ${this.renderCss(props)}
        `
    },
    renderHtml(props) {
        return `
            <h1>TodoMVC</h1>
            <todomvc-create-todo></todomvc-create-todo>
            <todomvc-todolist></todomvc-todolist>
        `
    },
    renderCss(props) {
        return ''
    }
}
