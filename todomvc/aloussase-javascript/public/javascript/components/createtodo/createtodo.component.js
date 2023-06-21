import Template from "./createtodo.template.js"

export default class CreateTodo extends HTMLElement {
    connectedCallback() {
        this.innerHTML = Template.render()

        const textField = this.querySelector("input[type=text]")
        const submitBtn = this.querySelector("input[type=submit]");
        submitBtn.addEventListener("click", e => {
            const newTodo = textField.value;
            if (newTodo === "") {
                alert("Por favor no ingresar un Todo vacio")
                return
            }

            fetch("/todos", {
                method: "POST",
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({content: newTodo})
            })
                .then(res => res.json())
                .then(data => {
                    const evt = new CustomEvent("todoCreated", {detail: data})
                    this.dispatchEvent(evt);
                })
        })
    }
}

customElements.define("todomvc-create-todo", CreateTodo)
