export default {
    render(props) {
        return `
            ${this.renderHtml(props)}
            ${this.renderCss(props)}
        `
    },
    renderHtml(_props) {
        return `
            <div id="createtodo">
                <label for="createtodo-input" />Nuevo todo:</label>
                <input id="createtodo-input" type="text" placeholder="Nuevo todo..." />
                <input type="submit" value="Crear" />
            </div>
        `
    },
    renderCss(_props) {
        return ''
    }
}
