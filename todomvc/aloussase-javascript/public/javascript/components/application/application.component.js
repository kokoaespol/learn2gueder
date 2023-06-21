import Template from "./application.template.js"

export default class Application extends HTMLElement {
    connectedCallback() {
        this.innerHTML = Template.render()

        const createTodoComponent = this.querySelector("todomvc-create-todo")
        const todoListComponent = this.querySelector("todomvc-todolist")

        createTodoComponent.addEventListener("todoCreated", ({detail}) => {
            const todos = JSON.parse(localStorage.getItem("todos"))
            todos.push(detail)
            localStorage.setItem("todos", JSON.stringify(todos));
            todoListComponent.update()
        })
    }
}

customElements.define("todomvc-application", Application);
