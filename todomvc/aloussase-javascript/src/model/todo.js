export function Todo({ id, content }) {
    this.id = id;
    this.content = content;
    this.checked = false;
}

Todo.createTable = function (db) {
    db.run(`
        create table todos (
            id text,
            content text,
            checked integer,

            primary key (id)
        );
    `)
}

Todo.findAll = function (db) {
    return new Promise((resolve, reject) => {
        db.all(`select id, content, checked
                  from todos`,
            (err, rows) => {
                if (err !== null) {
                    reject(err);
                } else {
                    resolve(rows.map(({ id, content, checked }) => {
                        const todo = new Todo({ id, content })
                        todo.checked = checked;
                        return todo;
                    }))
                }
            })
    });
}

Todo.prototype.save = function (db) {
    return new Promise((resolve, reject) => {
        db.run(`
            insert into todos (id, content, checked)
            values (?, ?, ?)
        `,
            this.id, this.content, this.checked,
            (err) => {
                if (err === null) {
                    resolve(true);
                } else {
                    resolve(false);
                }
            }
        );
    })

}