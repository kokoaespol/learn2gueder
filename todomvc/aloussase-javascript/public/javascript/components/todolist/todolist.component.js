import Template from "./todolist.template.js"

export default class TodoList extends HTMLElement {
    connectedCallback() {
        fetch("/todos")
            .then(res => res.json())
            .then(data => {
                localStorage.setItem("todos", JSON.stringify(data))
                this.innerHTML = Template.render(data)
            })
            .catch(err => console.error(err))
    }

    update() {
        const todos = JSON.parse(localStorage.getItem("todos"))
        this.innerHTML = Template.render(todos)
    }
}

customElements.define("todomvc-todolist", TodoList)
