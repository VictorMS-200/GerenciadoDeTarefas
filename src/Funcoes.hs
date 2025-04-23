module Funcoes(adicionarTarefa, removerTarefa, marcarConcluída, listarPorCategoria) where
import Tipos

-- Adiciona nova tarefa na lista que contem o restante da lista
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa novaTarefa listaDeTarefas = novaTarefa : listaDeTarefas

-- Remove uma tarefa da lista de tarefas, dado o identificador da tarefa
-- Se a lista estiver vazia ou o identificador não existir, retorna Nothing
removerTarefa :: Int -> [Tarefa] -> Maybe [Tarefa]
removerTarefa identificador listaDeTarefas
    | null listaDeTarefas = Nothing 
    | notElem identificador (map idTarefa listaDeTarefas) = Nothing 
    | otherwise = Just [t | t <- listaDeTarefas, idTarefa t /= identificador]

-- Marca uma tarefa como concluída, dado o identificador da tarefa
-- Se a lista estiver vazia ou o identificador não existir, retorna Nothing
marcarConcluída :: Int -> [Tarefa] -> Maybe [Tarefa]
marcarConcluída identificador listaDeTarefas 
    | null listaDeTarefas = Nothing
    | notElem identificador (map idTarefa listaDeTarefas) = Nothing 
    | otherwise = Just [if idTarefa t == identificador then t {status = Concluída} else t | t <- listaDeTarefas]

listarPorCategoria :: Categoria -> [Tarefa] -> Maybe [Tarefa]
listarPorCategoria cat listaDeTarefas 
    | null listaDeTarefas = Nothing
    | otherwise = Just [t | t <- listaDeTarefas, categoria t == cat]