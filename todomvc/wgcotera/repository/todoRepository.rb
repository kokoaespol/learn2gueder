class TodoRepository

    def create(request_data)
        @todo = Todo.new(request_data[:todo])
        @todo.save ? [@todo, 201] : [{error: "No se pudo cargar el Todo"}, 400]

    rescue JSON::ParserError
        [{error: "Datos de entrada invalidos"}, 400]
    end

    def update(id, request_data)
        @todo = Todo.find_by(id: id)
        return [{error: "Todo no encontrado"}, 404] unless @todo

        @todo.content = request_data[:content] if request_data.key?[:content]
        @todo.status = request_data[:status] if request_data.key?[:status]

        @todo.save ? [@todo, 200] : [{error: "No se pudo actualizar el Todo"}, 400]

    rescue JSON::ParserError
        [{error: "Datos de entrada invalidos"}, 400]
    end

    def delete(id)
        @todo = Todo.find_by(id: id)
        return [{error: "Todo no encontrado"}, 404] unless @todo

        @todo.destroy ? [{message: "Todo eliminado con exito"}, 200] : [{error: "No se pudo eliminar el Todo"}, 500]
    end

end