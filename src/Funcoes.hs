module Funcoes(adicionarTarefa, removerTarefa, marcarConcluída, 
listarPorCategoria, mostrarTarefas, listarPorPrioridade, 
ordenarPorPrioridade, filtrarPorStatus, buscarPorPalavraChave, 
verificarAtrasos, validarIdentificador, calcularDiasRestantes,
buscarTarefaPorIdentificador, salvarEmArquivo, carregarDeArquivo) where
import Tipos
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.List (isInfixOf)
import Data.Time.Calendar (diffDays, Day)


-- Função que transforma todas tarefas de uma lista em uma saida formatada
-- Exibe o ID, descrição, status, prioridade, categoria, prazo e tags de cada tarefa
mostrarTarefas :: [Tarefa] -> IO ()
mostrarTarefas listaDeTarefas = mapM_ mostrarTarefa listaDeTarefas

buscarTarefaPorIdentificador :: [Tarefa] -> Int -> Maybe Tarefa
buscarTarefaPorIdentificador listaDeTarefas identificador = case filter (\x -> idTarefa x == identificador) listaDeTarefas of
    (t:_) -> Just t
    [] -> Nothing 

mostrarTarefa :: Tarefa -> IO ()
mostrarTarefa t = putStrLn $ "ID: " ++ show (idTarefa t) ++ ", Descrição: " ++ descricao t ++ ", Status: " ++ show (status t) ++ ", Prioridade: " ++ show (prioridade t) ++ ", Categoria: " ++ show (categoria t) ++ ", Prazo: " ++ show (prazo t) ++ ", Tags: " ++ show (tags t)

validarIdentificador :: Int -> [Tarefa] -> Tarefa -> Maybe [Tarefa]
validarIdentificador identificador listaDeTarefas novaTarefa
    | null listaDeTarefas = Nothing
    | elem identificador (map idTarefa listaDeTarefas) = Nothing
    | otherwise = Just (novaTarefa : listaDeTarefas)

-- Adiciona nova tarefa na lista que contem o restante da lista
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa novaTarefa listaDeTarefas = novaTarefa : listaDeTarefas

-- Remove uma tarefa da lista de tarefas, dado o identificador da tarefa
removerTarefa :: Int -> [Tarefa] -> Maybe [Tarefa]
removerTarefa identificador listaDeTarefas
    | null listaDeTarefas = Nothing 
    | notElem identificador (map idTarefa listaDeTarefas) = Nothing 
    | otherwise = Just [t | t <- listaDeTarefas, idTarefa t /= identificador]

-- Marca uma tarefa como concluída, dado o identificador da tarefa
marcarConcluída :: Int -> [Tarefa] -> Maybe [Tarefa]
marcarConcluída identificador listaDeTarefas 
    | null listaDeTarefas = Nothing
    | notElem identificador (map idTarefa listaDeTarefas) = Nothing 
    | otherwise = Just [if idTarefa t == identificador then t {status = Concluída} else t | t <- listaDeTarefas]

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria categoriaProcurado listaDeTarefas = [t | t <- listaDeTarefas, categoria t == categoriaProcurado]

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade prioridadeProcurado listaDeTarefas = [t | t <- listaDeTarefas, prioridade t == prioridadeProcurado]

ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade listaDeTarefas = sortBy (comparing prioridade) listaDeTarefas

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus statusProcurado listaDeTarefas = [t | t <- listaDeTarefas, status t == statusProcurado]

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave palavra listaDeTarefas = [t | t <- listaDeTarefas, isInfixOf palavra (descricao t)]

verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos listaDeTarefas dia = [t | t <- listaDeTarefas, case prazo t of
    Just prazo -> diffDays dia prazo > 0
    Nothing -> False]

calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
calcularDiasRestantes tarefa dia = case prazo tarefa of
    Just prazo -> Just (fromIntegral (diffDays prazo dia))
    Nothing -> Nothing

salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo arquivo listaDeTarefas = writeFile arquivo (show listaDeTarefas)

carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo arquivo = do
    conteudo <- readFile arquivo
    let tarefas = read conteudo :: [Tarefa]
    return tarefas
