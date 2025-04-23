module Persistencia(listaMenu, adicionarTarefaMain, removerTarefaMain, marcarConcluídaMain, listarPorCategoriaMain, ordenarPorPrioridadeMain) where
import Funcoes
import Tipos
import System.IO
import Data.Time.Calendar (fromGregorian, Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)


-- função para mostrar o menu de opcoes
listaMenu :: IO ()
listaMenu = do
    putStrLn "========================="
    putStrLn "Escolha uma opcao:"
    putStrLn "1. Adicionar tarefa"
    putStrLn "2. Remover tarefa"
    putStrLn "3. Marcar tarefa como concluída"
    putStrLn "4. Listar tarefas por categoria"
    putStrLn "5. Listar tarefas por prioridade"
    putStrLn "6. Ordenar tarefas por prioridade"
    putStrLn "7. Filtrar tarefas por status"
    putStrLn "8. Buscar tarefas por palavra-chave"
    putStrLn "9. Verificar atrasos"
    putStrLn "10. Calcular dias restantes para a tarefa"
    putStrLn "11. Filtrar tarefas por tag"
    putStrLn "12. Nuvem de tags"
    putStrLn "13. Salvar em arquivo"
    putStrLn "14. Carregar de arquivo"
    putStrLn "15. Sair"
    putStrLn "========================="

adicionarTarefaMain :: [Tarefa] -> IO [Tarefa] -- Recebe uma lista de tarefas e retorna a lista atualizada com a nova tarefa adicionada
adicionarTarefaMain tarefas = do
    -- Solicita os dados da nova tarefa ao usuário
    putStrLn "Digite a descricao da tarefa:" 
    descricao <- getLine
    
    -- Verifica se a descrição não está vazia
    if null descricao then do
        putStrLn "Descricao não pode ser vazia!"
        adicionarTarefaMain tarefas -- Chama a função novamente para solicitar a descrição
    
    else do
        putStrLn "Digite a prioridade da tarefa (Baixa, Media, Alta):"
        prioridadeIO <- getLine
        
        -- Verifica se a prioridade é válida, caso contrário, define como Baixa
        prioridade <- case prioridadeIO of
                "Baixa" -> return Baixa
                "Media" -> return Media
                "Alta" -> return Alta
                _ -> do
                    putStrLn "Prioridade inválida! Usando prioridade padrão (Baixa)."
                    return Baixa
    
        putStrLn "Digite a categoria (Trabalho, Estudos, Pessoal, Outro):"
        categoriaIO <- getLine
        
        -- Verifica se a categoria é válida, caso contrário, define como Pessoal
        categoria <- case categoriaIO of
            "Trabalho" -> return Trabalho
            "Estudos" -> return Estudos
            "Pessoal" -> return Pessoal
            "Outro" -> return Outro
            _ -> do
                putStrLn "Categoria inválida! Usando categoria padrão Pessoal."
                return Outro
        
        putStrLn "Digite a data de prazo (YYYY-MM-DD):"
        prazoIO <- getLine
        
        -- Verifica se a data de prazo é válida, caso contrário, define como Nothing
        let prazo = if null prazoIO then Nothing
            else case parseTimeM True defaultTimeLocale "%Y-%m-%d" prazoIO of
                Just day -> Just day
                Nothing  -> error "Data inválida. Use o formato YYYY-MM-DD."
        
        -- Solicita as tags ao usuário
        putStrLn "Digite as tags (separadas por &):"
        tagsIO <- getLine

        -- Substitui os caracteres '&' por espaços para separar as tags.
        let tags = words (map (\c -> if c == '&' then ' ' else c) tagsIO)

        -- Gera um identificador único para a nova tarefa com base no maior identificador existente na lista de tarefas ou 1 se a lista estiver vazia.
        idTarefa <- if null tarefas then return 1
            else return (foldl (\x y -> if x > y then x else y) 0 [idTarefa t | t <- tarefas] + 1)

        -- Cria a nova tarefa com os dados fornecidos pelo usuário e o identificador gerado.
        let novaTarefa = Tarefa { idTarefa = idTarefa, descricao = descricao, status = Pendente, prioridade = prioridade, categoria = categoria, prazo = prazo, tags = tags }
        
        let tarefasAtualizadas = adicionarTarefa novaTarefa tarefas
        putStrLn "Tarefa adicionada com sucesso!"
        return tarefasAtualizadas

removerTarefaMain :: [Tarefa] -> IO [Tarefa] -- Recebe uma lista de tarefas e retorna a lista atualizada com a tarefa removida do identificador fornecido
removerTarefaMain tarefas = do
    -- Solicita o ID da tarefa a ser removida ao usuário
    putStrLn "Digite o ID da tarefa a ser removida:"
    idStr <- getLine
    let idTarefa = read idStr :: Int
    case removerTarefa idTarefa tarefas of
        Just tarefasAtualizadas -> do
            putStrLn "Tarefa removida com sucesso!"
            return tarefasAtualizadas
        Nothing -> do
            putStrLn "Tarefa não encontrada ou lista vazia."
            return tarefas

marcarConcluídaMain :: [Tarefa] -> IO [Tarefa] -- Recebe uma lista de tarefas e retorna a lista atualizada com a tarefa marcada como concluída do identificador fornecido
marcarConcluídaMain tarefas = do
    -- Solicita o ID da tarefa a ser marcada como concluída ao usuário
    putStrLn "Digite o ID da tarefa a ser marcada como concluída:"
    idStr <- getLine
    let idTarefa = read idStr :: Int
    
    -- Chama a função marcarConcluída para atualizar o status da tarefa
    -- Se a tarefa for encontrada, atualiza o status para Concluída e retorna a lista atualizada
    case marcarConcluída idTarefa tarefas of
        Just tarefasAtualizadas -> do
            putStrLn "Tarefa marcada como concluída com sucesso!"
            return tarefasAtualizadas
        Nothing -> do
            putStrLn "Tarefa não encontrada ou lista vazia."
            return tarefas

listarPorCategoriaMain :: [Tarefa] -> IO [Tarefa] -- Recebe uma lista de tarefas e retorna a lista filtrada por categoria
listarPorCategoriaMain tarefas = do
    -- Solicita a categoria ao usuário
    putStrLn "Digite a categoria (Trabalho, Estudos, Pessoal, Outro):"
    categoriaIO <- getLine
    
    -- Verifica se a categoria é válida, caso contrário, define como Pessoal
    categoria <- case categoriaIO of
        "Trabalho" -> return Trabalho
        "Estudos" -> return Estudos
        "Pessoal" -> return Pessoal
        "Outro" -> return Outro
        _ -> do
            putStrLn "Categoria inválida! Usando categoria padrão Pessoal."
            return Pessoal
    
    -- Filtra as tarefas pela categoria fornecida e retorna a lista filtrada
    case listarPorCategoria categoria tarefas of
        Just tarefasFiltradas -> do
            putStrLn "Tarefas filtradas por categoria:"
            mostrarTarefas tarefasFiltradas
            return tarefasFiltradas
        Nothing -> do
            putStrLn "Nenhuma tarefa encontrada ou lista vazia."
            return []



ordenarPorPrioridadeMain :: [Tarefa] -> IO [Tarefa]
ordenarPorPrioridadeMain tarefas
    | null tarefas = do return []
    | otherwise = do
        putStrLn "Tarefas oordenadas por prioridade"
        mostrarTarefas (ordenarPorPrioridade tarefas)
        return []