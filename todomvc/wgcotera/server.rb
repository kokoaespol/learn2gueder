require 'sinatra'
require 'sinatra/activerecord'
require 'sinatra/json'
require 'json'

require './models/todo'

set :database, {adapter: 'sqlite3', database: 'todos.db'}

get '/' do
    @todos = Todo.all
    json @todos
end

post '/todos' do
    begin
        request_data = JSON.parse(request.body.read)
        @todo = Todo.new(request_data["todo"])
        if @todo.save
            status 201
            json @todo
        else
            status 400
            json error: "No se pudo guardar el Todo"
        end
    rescue JSON::ParserError => e
        status 400
        json error: "Datos de entrada inválidos"
    end
end

delete '/todos/:id' do
    @todo = Todo.find_by(id: params[:id])
    if @todo
        if @todo.destroy
            status 200
            json message: "Todo eliminado con éxito"
        else
            status 500
            json error: "No se pudo eliminar el Todo"
        end
    else
        status 404
        json error: "Todo no encontrado"
    end
end

put '/todos/:id' do
    @todo = Todo.find_by(id: params[:id])
    if @todo
        request_data = JSON.parse(request.body.read)
        
        if request_data.key?("content")
            @todo.content = request_data["content"]
        end
    
        if request_data.key?("status")
            @todo.status = request_data["status"]
        end
    
        if @todo.save
            status 200
            json @todo
        else
            status 500
            json error: "No se pudo actualizar el Todo"
        end
    else
        status 404
        json error: "Todo no encontrado"
    end
end  