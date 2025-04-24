module Funcoes(adicionarTarefa, removerTarefa, marcarConcluída, 
listarPorCategoria, mostrarTarefas, listarPorPrioridade, 
ordenarPorPrioridade, filtrarPorStatus, buscarPorPalavraChave, 
verificarAtrasos, validarIdentificador, calcularDiasRestantes,
buscarTarefaPorIdentificador, salvarEmArquivo, carregarDeArquivo,
porcentagemDadoFormatado, nuvemDeTags, filtrarPorTag) where
import Tipos
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.List (isInfixOf)
import Data.Time.Calendar (diffDays, Day)

-- Função que transforma todas tarefas de uma lista em uma saida formatada
-- Exibe o ID, descrição, status, prioridade, categoria, prazo e tags de cada tarefa
mostrarTarefas :: [Tarefa] -> IO ()
mostrarTarefas listaDeTarefas = mapM_ mostrarTarefa listaDeTarefas

-- Função que formata a porcentagem de um dado em relação a outro dado
porcentagemDadoFormatado :: Int -> Int -> String
porcentagemDadoFormatado dado porcentagem = " (" ++ show (fromIntegral(dado * 100) / fromIntegral(porcentagem)) ++ "%)"

-- Função que formata a porcentagem de um dado em relação a outro dado
buscarTarefaPorIdentificador :: [Tarefa] -> Int -> Maybe Tarefa
buscarTarefaPorIdentificador listaDeTarefas identificador = case filter (\x -> idTarefa x == identificador) listaDeTarefas of
    (t:_) -> Just t
    [] -> Nothing 

-- Função que formata a saida de uma tarefa
mostrarTarefa :: Tarefa -> IO ()
mostrarTarefa t = putStrLn $ "ID: " ++ show (idTarefa t) ++ ", Descrição: " ++ descricao t ++ ", Status: " ++ show (status t) ++ ", Prioridade: " ++ show (prioridade t) ++ ", Categoria: " ++ show (categoria t) ++ ", Prazo: " ++ show (prazo t) ++ ", Tags: " ++ show (tags t)

-- Função que valida se o identificador da tarefa já existe na lista de tarefas
validarIdentificador :: Int -> [Tarefa] -> Tarefa -> Maybe [Tarefa]
validarIdentificador identificador listaDeTarefas novaTarefa
    -- Verifica se a lista de tarefas está vazia ou se o identificador já existe na lista
    -- Se a lista estiver vazia ou o identificador não existir, retorna Nothing
    | null listaDeTarefas = Nothing
    | elem identificador (map idTarefa listaDeTarefas) = Nothing
    | otherwise = Just (novaTarefa : listaDeTarefas)

-- Adiciona nova tarefa na lista que contem o restante da lista
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
-- A função recebe uma nova tarefa e a lista de tarefas existentes, e adiciona a nova tarefa no início da lista
adicionarTarefa novaTarefa listaDeTarefas = novaTarefa : listaDeTarefas

-- Remove uma tarefa da lista de tarefas, dado o identificador da tarefa
removerTarefa :: Int -> [Tarefa] -> Maybe [Tarefa]
removerTarefa identificador listaDeTarefas
    -- Verifica se a lista de tarefas está vazia ou se o identificador não existe na lista
    -- Se a lista estiver vazia ou o identificador não existir, retorna Nothing
    | null listaDeTarefas = Nothing 
    | notElem identificador (map idTarefa listaDeTarefas) = Nothing 
    | otherwise = Just [t | t <- listaDeTarefas, idTarefa t /= identificador]

-- Marca uma tarefa como concluída, dado o identificador da tarefa
marcarConcluída :: Int -> [Tarefa] -> Maybe [Tarefa]
marcarConcluída identificador listaDeTarefas 
    -- Verifica se a lista de tarefas está vazia ou se o identificador não existe na lista
    -- Se a lista estiver vazia ou o identificador não existir, retorna Nothing
    | null listaDeTarefas = Nothing
    | notElem identificador (map idTarefa listaDeTarefas) = Nothing 
    | otherwise = Just [if idTarefa t == identificador then t {status = Concluída} else t | t <- listaDeTarefas]

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
-- A função filtra a lista de tarefas, retornando apenas aquelas que têm a categoria procurada
listarPorCategoria categoriaProcurado listaDeTarefas = [t | t <- listaDeTarefas, categoria t == categoriaProcurado]

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
-- A função filtra a lista de tarefas, retornando apenas aquelas que têm a prioridade procurada
listarPorPrioridade prioridadeProcurado listaDeTarefas = [t | t <- listaDeTarefas, prioridade t == prioridadeProcurado]

ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
-- A função ordena a lista de tarefas pela prioridade, do mais alto para o mais baixo utilizando a função sortBy e a função comparing
-- do módulo Data.Ord
ordenarPorPrioridade listaDeTarefas = sortBy (comparing prioridade) listaDeTarefas

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
-- A função filtra a lista de tarefas, retornando apenas aquelas que têm o status procurado
filtrarPorStatus statusProcurado listaDeTarefas = [t | t <- listaDeTarefas, status t == statusProcurado]

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
-- A função busca tarefas que contêm a palavra-chave na descrição
buscarPorPalavraChave palavra listaDeTarefas = [t | t <- listaDeTarefas, isInfixOf palavra (descricao t)]

verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
-- A função verifica se a tarefa está atrasada comparando o prazo com a data atual
verificarAtrasos listaDeTarefas dia = [t | t <- listaDeTarefas, case prazo t of
    Just prazo -> diffDays dia prazo > 0
    Nothing -> False]

calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
-- A função calcula a diferença em dias entre o prazo da tarefa e o dia atual usando a função diffDays
calcularDiasRestantes tarefa dia = case prazo tarefa of
    Just prazo -> Just (fromIntegral (diffDays prazo dia))
    Nothing -> Nothing

-- Função que salva tarefas em um arquivo
salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
-- A função recebe o caminho do arquivo e a lista de tarefas, e escreve a lista de tarefas no arquivo
salvarEmArquivo arquivo listaDeTarefas = writeFile arquivo (show listaDeTarefas)

-- Função que carrega tarefas de um arquivo
carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo arquivo = do
    -- Lê o conteúdo do arquivo e converte para uma lista de tarefas usando a função read
    conteudo <- readFile arquivo
    let tarefas = read conteudo :: [Tarefa]
    return tarefas

-- Função que filtra tarefas por tag
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
-- A função filtra a lista de tarefas, retornando apenas aquelas que contêm a tag procurada
filtrarPorTag tagProcurada listaTarefa = [t | t <- listaTarefa, tagProcurada `elem` tags t]

-- Função que gera uma nuvem de tags a partir da lista de tarefas
nuvemDeTags :: [Tarefa] -> [(String, Int)]
-- A função gera uma lista de tuplas onde cada tupla contém uma tag e a quantidade de tarefas que possuem essa tag na lista de tarefas
nuvemDeTags listaTarefa = [(tag, length (filter (\t -> tag `elem` tags t) listaTarefa)) | tag <- tagsUnicas]
    where
        -- Retorna uma lista de tags únicas
        tagsUnicas = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] (concat (map tags listaTarefa))