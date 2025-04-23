module Persistencia(listaMenu, adicionarTarefaMain, removerTarefaMain, marcarConcluídaMain, listarPorCategoriaMain, listarPorPrioridadeMain, ordenarPorPrioridadeMain, filtrarPorStatusMain, buscarPorPalavraChaveMain, verificarAtrasosMain) where
import Funcoes
import Tipos
import System.IO
import Data.Time.Calendar (Day, fromGregorian, diffDays)
import Data.Time.LocalTime
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Text.Read (readMaybe)


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
                Just dia -> Just dia
                Nothing  -> error "Data inválida. Use o formato YYYY-MM-DD."

        -- Solicita as tags ao usuário
        putStrLn "Digite as tags (separadas por &):"
        tagsIO <- getLine

        -- Substitui os caracteres '&' por espaços para separar as tags.
        let tags = words (map (\c -> if c == '&' then ' ' else c) tagsIO)

        putStrLn "Por ultimo, digite o ID da tarefa:"
        idStr <- getLine

        case readMaybe idStr :: Maybe Int of
            Nothing -> do
                putStrLn "Erro: Entrada inválida. Certifique-se de digitar um número inteiro."
                adicionarTarefaMain tarefas 
            Just idTarefa -> do
                let tarefaUsuario = Tarefa { idTarefa = idTarefa, descricao = descricao, status = Pendente, prioridade = prioridade, categoria = categoria, prazo = prazo, tags = tags }
                case validarIdentificador idTarefa tarefas tarefaUsuario of
                    Just tarefas -> do
                        putStrLn "Tarefa adicionada com sucesso!"
                        return tarefas
                    Nothing -> do
                        putStrLn "ID já existe! Tente novamente."
                        adicionarTarefaMain tarefas 

removerTarefaMain :: [Tarefa] -> IO [Tarefa] -- Recebe uma lista de tarefas e retorna a lista atualizada com a tarefa removida do identificador fornecido
removerTarefaMain tarefas = do
    -- Solicita o ID da tarefa a ser removida ao usuário
    putStrLn "Digite o ID da tarefa a ser removida:"
    idStr <- getLine
    -- Verifica se o ID é um número inteiro
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
        Just tarefasConcluidas -> do
            putStrLn "Tarefa marcada como concluída com sucesso!"
            return tarefasConcluidas
        Nothing -> do
            putStrLn "Tarefa não encontrada ou lista vazia."
            return tarefas

listarPorCategoriaMain :: [Tarefa] -> IO () -- Recebe uma lista de tarefas e retorna a lista filtrada por categoria
listarPorCategoriaMain tarefas = do
    -- Solicita a categoria ao usuário
    putStrLn "Digite a categoria (Trabalho, Estudos, Pessoal, Outro):"
    categoriaIO <- getLine
    
    -- Verifica se a categoria é válida, caso contrário, chama a função novamente
    if notElem categoriaIO ["Trabalho", "Estudos", "Pessoal", "Outro"] then do
        putStrLn $ "Erro! Palavra " ++ categoriaIO ++ " não corresponde a lista de categoria ['Trabalho', 'Estudos', 'Pessoal', 'Outro']."
        listarPorCategoriaMain tarefas
    else do
        categoria <- case categoriaIO of
            "Trabalho" -> return Trabalho
            "Estudos" -> return Estudos
            "Pessoal" -> return Pessoal
            "Outro" -> return Outro
    
        -- Filtra as tarefas pela categoria fornecida e retorna a lista filtrada
        if null (listarPorCategoria categoria tarefas) then do putStrLn "Nenhuma tarefa com categoria escolhida encontrada ou lista vazia."
            else do 
                putStrLn "Tarefas listadas por categoria:"
                mostrarTarefas (listarPorCategoria categoria tarefas)

listarPorPrioridadeMain :: [Tarefa] -> IO ()
listarPorPrioridadeMain tarefas = do
    putStrLn "Digite a prioridade (Baixa, Media, Alta)"
    prioridadeIO <- getLine

    if notElem prioridadeIO ["Baixa" , "Media" , "Alta"] then do
        putStrLn $ "Erro! Palavra " ++ prioridadeIO ++ " não corresponde a lista de prioridade (Baixa, Media, Alta)"
        listarPorPrioridadeMain tarefas
    else do
        prioridade <- case prioridadeIO of
            "Baixa" -> return Baixa
            "Media" -> return Media
            "Alta" -> return Alta
        
        if null (listarPorPrioridade prioridade tarefas) then do putStrLn "Nenhuma tarefa com prioridade escolhida encontrada ou lista vazia."
            else do
                putStrLn "Tarefas listadas por prioridade:"
                mostrarTarefas (listarPorPrioridade prioridade tarefas)

ordenarPorPrioridadeMain :: [Tarefa] -> IO ()
ordenarPorPrioridadeMain tarefas
    | null tarefas = do putStrLn "Nenhuma tarefa para ser ordenada!"
    | otherwise = do
        putStrLn "Tarefas oordenadas por prioridade"
        mostrarTarefas (ordenarPorPrioridade tarefas)

filtrarPorStatusMain :: [Tarefa] -> IO()
filtrarPorStatusMain tarefas = do
    putStrLn "Digite o status (Pendente, Concluída):"
    statusIO <- getLine

    if notElem statusIO ["Pendente", "Concluída"] then do 
        putStrLn $ "Erro! Palavra " ++ statusIO ++ " não corresponde aos status ['Pendente', 'Concluída']."
        filtrarPorStatusMain tarefas
    else do
        status <- case statusIO of
            "Pendente" -> return Pendente
            "Concluída" -> return Concluída

        mostrarTarefas (filtrarPorStatus status tarefas)
    
buscarPorPalavraChaveMain :: [Tarefa] -> IO()
buscarPorPalavraChaveMain tarefas = do 
    putStrLn "Digite a palavra chave a ser procurada:"
    palavraChaveIO <- getLine
    if null palavraChaveIO then do 
        putStrLn $ "Erro! Palavra invalida, tente novamente."
        buscarPorPalavraChaveMain tarefas
    else do
        if null (buscarPorPalavraChave palavraChaveIO tarefas) then do putStrLn $ "Nenhuma tarefa com a palavra chave " ++ palavraChaveIO ++ " na descrição"
        else do
            mostrarTarefas (buscarPorPalavraChave palavraChaveIO tarefas)

verificarAtrasosMain :: [Tarefa] -> IO()
verificarAtrasosMain tarefas = do
    putStrLn "Tarefas com prazo atrasado:"
    localTime <- getZonedTime
    let day = localDay (zonedTimeToLocalTime localTime)
    mostrarTarefas (verificarAtrasos tarefas day)


