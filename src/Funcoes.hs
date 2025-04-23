module Funcoes(adicionarTarefa, removerTarefa, marcarConcluída, listarPorCategoria, mostrarTarefas, listarPorPrioridade, ordenarPorPrioridade, filtrarPorStatus) where
import Tipos
import Data.List (sortBy)
import Data.Ord (comparing)

mostrarTarefas :: [Tarefa] -> IO () -- Função para mostrar as tarefas na tela
mostrarTarefas [] = return ()
mostrarTarefas (t:ts) = do
    putStrLn $ "ID: " ++ show (idTarefa t) ++ ", Descrição: " ++ descricao t ++ ", Status: " ++ show (status t) ++ ", Prioridade: " ++ show (prioridade t) ++ ", Categoria: " ++ show (categoria t) ++ ", Prazo: " ++ show (prazo t) ++ ", Tags: " ++ show (tags t)
    mostrarTarefas ts

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

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cat listaDeTarefas = [t | t <- listaDeTarefas, categoria t == cat]

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade pri listaDeTarefas = [t | t <- listaDeTarefas, prioridade t == pri]

ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade listaDeTarefas = sortBy (comparing prioridade) listaDeTarefas

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus sta listaDeTarefas = [t | t <- listaDeTarefas, status t == sta]