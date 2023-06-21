import Template from "./todo.template.js"

export default class Todo extends HTMLElement {
    connectedCallback() {
        const id = this.getAttribute("id")
        const content = this.getAttribute("content")
        const checked = this.getAttribute("checked")
        this.innerHTML = Template.render({
            id,
            content,
            checked
        })
    }
}

customElements.define("todomvc-todo", Todo)
