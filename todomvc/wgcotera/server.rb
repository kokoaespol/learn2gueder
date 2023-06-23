require 'sinatra'
require 'sinatra/activerecord'
require 'sinatra/json'
require 'json'

require './models/todo'
require_relative './repository/todoRepository'

set :database, {adapter: 'sqlite3', database: 'todos.db'}

get '/' do
    @todos = Todo.all
    json @todos
end

post '/todos' do
    request_data = JSON.parse(request.body.read, symbolize_names: true)
    result, status_code = TodoRepository.create(request_data)
    status status_code
    json result
end

put '/todos/:id' do
    request_data = JSON.parse(request.body.read, symbolize_names: true)
    result, status_code = TodoRepository.update(params[:id], request_data)
    status status_code
    json result
end

delete '/todos/:id' do
    result, status_code = TodoRepository.delete(params[:id])
    status status_code
    json result
end