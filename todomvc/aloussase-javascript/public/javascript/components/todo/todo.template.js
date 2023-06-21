export default {
    render(props) {
        return `
            ${this.renderHtml(props)}
            ${this.renderCss(props)}
        `
    },
    renderHtml({id, content, checked}) {
        return `
            <div class="todo">
                <input id=${id} type="checkbox" ${checked != 0 && "checked"} />
                <label for=${id}>${content}</label>
            </div>
        `
    },
    renderCss(props) {return ''}
}
