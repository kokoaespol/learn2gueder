import Todo from "../todo/todo.component.js"

export default {
    render(props) {
        return `
            ${this.renderHtml(props)}
            ${this.renderCss(props)}
        `
    },
    renderHtml(props) {
        let html = ''

        for (const todo of props) {
            html += `
                <todomvc-todo
                    id="${todo.id}"
                    content="${todo.content}"
                    checked=${todo.checked}>
                </todomvc-todo>
            `
        }

        return html;
    },
    renderCss(props) {
        return ''
    }
}
