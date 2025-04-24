module Persistencia(menuOpcoesMain, adicionarTarefaMain, removerTarefaMain,
marcarConcluídaMain, listarPorCategoriaMain, listarPorPrioridadeMain, 
ordenarPorPrioridadeMain, filtrarPorStatusMain, buscarPorPalavraChaveMain, 
verificarAtrasosMain, calcularDiasRestantesMain, salvarEmArquivoMain,
carregarDeArquivoMain, relatorioMain, listarMain, filtrarPorTagMain, 
nuvemDeTagsMain) where
    
import Funcoes
import Tipos
import System.IO
import Data.Time.Calendar (Day, fromGregorian, diffDays)
import Data.Time.LocalTime
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Text.Read (readMaybe)


-- função para mostrar o menu de opcoes
menuOpcoesMain :: IO ()
menuOpcoesMain = do
    putStrLn "========================="
    putStrLn "Escolha uma opcao:"
    putStrLn "1. Adicionar tarefa"
    putStrLn "2. Remover tarefa"
    putStrLn "3. Marcar tarefa como concluída"
    putStrLn "4. Listar tarefas"
    putStrLn "5. Calcular dias restantes de uma tarefa"
    putStrLn "6. Filtrar tarefas por tag"
    putStrLn "7. Nuvem de tags"
    putStrLn "8. Salvar em arquivo (tarefas.txt)"
    putStrLn "9. Carregar tarefas de arquivo (tarefas.txt)"
    putStrLn "10. Relatório de tarefas"
    putStrLn "11. Sair"
    putStrLn "========================="

-- Função para mostar opções de listagem
listarMenu :: IO ()
listarMenu = do
    putStrLn "Deseja listar as tarefas como?"
    putStrLn "1. Listar todas as tarefas"
    putStrLn "2. Listar tarefas por categoria"
    putStrLn "3. Listar tarefas por prioridade"
    putStrLn "4. Listar tarefas por status"
    putStrLn "5. Listar tarefas por palavra-chave"
    putStrLn "6. Listar tarefas com ordem por prioridade"
    putStrLn "7. Listar tarefas com prazo atrasado"

-- Função principal para listar tarefas
-- Recebe uma lista de tarefas e exibe o menu de listagem
-- Dependendo da opção escolhida pelo usuário, chama a função correspondente
listarMain :: [Tarefa] -> IO ()
listarMain tarefas = do
    listarMenu
    opcao <- getLine

    case opcao of
        "1" -> listarGeralMain tarefas
        "2" -> listarPorCategoriaMain tarefas
        "3" -> listarPorPrioridadeMain tarefas
        "4" -> filtrarPorStatusMain tarefas
        "5" -> buscarPorPalavraChaveMain tarefas
        "6" -> ordenarPorPrioridadeMain tarefas
        "7" -> verificarAtrasosMain tarefas
        _   -> putStrLn "Opção inválida!"

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

        putStrLn "Por último, digite o ID da tarefa:"
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
    case readMaybe idStr :: Maybe Int of
        Nothing -> do
            putStrLn "Erro: Entrada inválida. Certifique-se de digitar um número inteiro."
            removerTarefaMain tarefas -- Chama a função novamente para solicitar o ID
        Just idTarefa -> do
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
    -- Verifica se o ID é um número inteiro
    case readMaybe idStr :: Maybe Int of
        Nothing -> do
            putStrLn "Erro: Entrada inválida. Certifique-se de digitar um número inteiro."
            marcarConcluídaMain tarefas -- Chama a função novamente para solicitar o ID
        Just idTarefa -> do
            case marcarConcluída idTarefa tarefas of
                Just tarefasConcluidas -> do
                    putStrLn "Tarefa marcada como concluída com sucesso!"
                    return tarefasConcluidas
                Nothing -> do
                    putStrLn "Tarefa não encontrada ou lista vazia."
                    return tarefas

listarGeralMain :: [Tarefa] -> IO () -- Recebe uma lista de tarefas e retorna a lista geral
listarGeralMain tarefas = do
    if null tarefas then do
        putStrLn "Nenhuma tarefa cadastrada!"
    else do
        putStrLn "Tarefas cadastradas:"
        mostrarTarefas tarefas

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

listarPorPrioridadeMain :: [Tarefa] -> IO () -- Recebe uma lista de tarefas e retorna a lista filtrada por prioridade
listarPorPrioridadeMain tarefas = do
    putStrLn "Digite a prioridade (Baixa, Media, Alta)"
    prioridadeIO <- getLine
    -- Verifica se a prioridade é válida, caso contrário, chama a função novamente
    if notElem prioridadeIO ["Baixa" , "Media" , "Alta"] then do
        putStrLn $ "Erro! Palavra " ++ prioridadeIO ++ " não corresponde a lista de prioridade (Baixa, Media, Alta)"
        listarPorPrioridadeMain tarefas
    else do
        prioridade <- case prioridadeIO of
            "Baixa" -> return Baixa
            "Media" -> return Media
            "Alta" -> return Alta
        -- Filtra as tarefas pela prioridade fornecida e retorna a lista filtrada
        if null (listarPorPrioridade prioridade tarefas) then do putStrLn "Nenhuma tarefa com prioridade escolhida encontrada ou lista vazia."
            else do
                putStrLn "Tarefas listadas por prioridade:"
                mostrarTarefas (listarPorPrioridade prioridade tarefas)

ordenarPorPrioridadeMain :: [Tarefa] -> IO () -- Recebe uma lista de tarefas e retorna a lista ordenada por prioridade
ordenarPorPrioridadeMain tarefas
    -- Verifica se a lista de tarefas está vazia
    | null tarefas = do putStrLn "Nenhuma tarefa para ser ordenada!"
    | otherwise = do
        putStrLn "Tarefas oordenadas por prioridade"
        mostrarTarefas (ordenarPorPrioridade tarefas)

filtrarPorStatusMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e retorna a lista filtrada por status
filtrarPorStatusMain tarefas = do
    -- Solicita o status ao usuário
    putStrLn "Digite o status (Pendente, Concluída):"
    statusIO <- getLine

    -- Verifica se o status é válido, caso contrário, chama a função novamente
    if notElem statusIO ["Pendente", "Concluída"] then do 
        putStrLn $ "Erro! Palavra " ++ statusIO ++ " não corresponde aos status ['Pendente', 'Concluída']."
        filtrarPorStatusMain tarefas
    else do
        status <- case statusIO of
            "Pendente" -> return Pendente
            "Concluída" -> return Concluída

        mostrarTarefas (filtrarPorStatus status tarefas)
    
buscarPorPalavraChaveMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e retorna a lista filtrada por palavra-chave
buscarPorPalavraChaveMain tarefas = do 
    -- Solicita a palavra-chave ao usuário
    putStrLn "Digite a palavra chave a ser procurada:"
    palavraChaveIO <- getLine
    -- Verifica se a palavra-chave não está vazia, caso contrário, chama a função novamente
    if null palavraChaveIO then do 
        putStrLn $ "Erro! Palavra invalida, tente novamente."
        buscarPorPalavraChaveMain tarefas
    else do
        if null (buscarPorPalavraChave palavraChaveIO tarefas) then do putStrLn $ "Nenhuma tarefa com a palavra chave " ++ palavraChaveIO ++ " na descrição"
        else do
            mostrarTarefas (buscarPorPalavraChave palavraChaveIO tarefas)

verificarAtrasosMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e retorna a lista filtrada por tarefas com prazo atrasado
verificarAtrasosMain tarefas = do
    putStrLn "Tarefas com prazo atrasado:"
    localTime <- getZonedTime -- Obtém o horário local atual
    let day = localDay (zonedTimeToLocalTime localTime) -- Converte para o tipo Day
    mostrarTarefas (verificarAtrasos tarefas day) -- Filtra as tarefas com prazo atrasado e exibe

calcularDiasRestantesMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e retorna a quantidade de dias restantes para o prazo da tarefa
calcularDiasRestantesMain tarefas = do
    -- Solicita o identificador da tarefa ao usuário
    putStrLn "Digite o identificador da tarefa que deseja calcular os dias que falta para terminar o prazo:"
    identificador <- getLine
    -- Verifica se o identificador é um número inteiro
    case readMaybe identificador :: Maybe Int of

        Nothing -> do
            putStrLn "Erro! Digite novamente o identificador!"
            calcularDiasRestantesMain tarefas
        Just idTarefa -> do
            localTime <- getZonedTime -- Obtém o horário local atual
            let day = localDay (zonedTimeToLocalTime localTime) -- Converte para o tipo Day
            -- Busca a tarefa pelo identificador fornecido
            case buscarTarefaPorIdentificador tarefas idTarefa of
                Nothing -> do
                    putStrLn "Erro! Identificador não existe na lista de tarefas."
                    calcularDiasRestantesMain tarefas
                Just tarefaEncontrada -> do
                    -- Verifica se a tarefa possui prazo definido
                    let diasRestantes = calcularDiasRestantes tarefaEncontrada day
                    case diasRestantes of
                        Just dias -> if dias > 0 then putStrLn $ "Dias restantes: " ++ show dias else putStrLn $ "Passou do prazo! Tem " ++ show (abs dias) ++" que já passou do prazo da tarefa."
                        Nothing -> putStrLn "A tarefa não possui prazo definido."

filtrarPorTagMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e retorna a lista filtrada por tag
filtrarPorTagMain tarefas = do
  putStrLn "Digite a Tag a ser procurada:"
  tagProcurada <- getLine
  -- Verifica se a tag não está vazia, caso contrário, chama a função novamente
  if null tagProcurada then do
      putStrLn $ "Erro! Tag inserida inválida."
      filtrarPorTagMain tarefas
  else do
       if null (filtrarPorTag tagProcurada tarefas) then do putStrLn $ "Nenhuma tarefa com a tag " ++ tagProcurada ++ " encontrada."
       else do
          mostrarTarefas (filtrarPorTag tagProcurada tarefas)

nuvemDeTagsMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e retorna a nuvem de tags
nuvemDeTagsMain tarefas = do
    -- Solicita a tag ao usuário
     let nuvem = nuvemDeTags tarefas
     -- Verifica se a nuvem de tags está vazia, caso contrário, chama a função novamente
     if null nuvem then do putStrLn "Nenhuma tag encontrada."
     else do mapM_ (\(tag, qtd) -> putStrLn $ "- " ++ tag ++ ": " ++ show qtd) nuvem
 
salvarEmArquivoMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e salva em um arquivo
salvarEmArquivoMain tarefas = do
    putStrLn "Salvando listas no arquivo 'tarefas.txt'"
    -- Por padrão, o arquivo é o "tarefas.txt"
    salvarEmArquivo "tarefas.txt" tarefas

carregarDeArquivoMain :: IO [Tarefa] -- Recebe uma lista de tarefas e carrega de um arquivo
carregarDeArquivoMain = do
    -- Carrega as tarefas do arquivo "tarefas.txt"
    putStrLn "Carregando tarefas do arquivo 'tarefas.txt'"
    tarefas <- carregarDeArquivo "tarefas.txt" -- Por padrão, o arquivo é o "tarefas.txt"
    putStrLn "Tarefas carregadas com sucesso!"
    return tarefas

relatorioMain :: [Tarefa] -> IO() -- Recebe uma lista de tarefas e retorna um relatório com as informações das tarefas
relatorioMain tarefas = do
    -- Pega o total de tarefas, tarefas pendentes, concluídas e das categorias
    let totalTarefas = length tarefas
    let tarefasPendentes = length (filter (\t -> status t == Pendente) tarefas)
    let tarefasConcluidas = length (filter (\t -> status t == Concluída) tarefas)
    let tarefasTrabalho = length (filter (\t -> categoria t == Trabalho) tarefas)
    let tarefasEstudos = length (filter (\t -> categoria t == Estudos) tarefas)
    let tarefasPessoal = length (filter (\t -> categoria t == Pessoal) tarefas)
    let tarefasOutro = length (filter (\t -> categoria t == Outro) tarefas)

    -- Verifica se contém mais de uma tarefa e coloca a palavra no plural ou singular
    let palavraTotalTarefas = if totalTarefas == 1 then "tarefa" else "tarefas"
    let palavraTrabalho = if tarefasTrabalho == 1 then "tarefa" else "tarefas"
    let palavraEstudos = if tarefasEstudos == 1 then "tarefa" else "tarefas"
    let palavraPessoal = if tarefasPessoal == 1 then "tarefa" else "tarefas"
    let palavraOutro = if tarefasOutro == 1 then "tarefa" else "tarefas"

    if totalTarefas == 0 then do
        putStrLn "Nenhuma tarefa cadastrada!"
        
    else do
        putStrLn "========================="
        putStrLn "Relatório de Tarefas:"
        putStrLn "========================="
        putStrLn $ "- Total de " ++ palavraTotalTarefas ++ " : " ++ show totalTarefas
        putStrLn $ "- Tarefas pendentes: " ++ show tarefasPendentes ++ porcentagemDadoFormatado tarefasPendentes totalTarefas
        putStrLn $ "- Tarefas concluídas: " ++ show tarefasConcluidas ++ porcentagemDadoFormatado tarefasConcluidas totalTarefas
        putStrLn $ "- Distribuíção por categoria:"
        putStrLn $ "  * Trabalho: " ++ show (tarefasTrabalho) ++ " " ++ palavraTrabalho ++ " " ++ porcentagemDadoFormatado tarefasTrabalho totalTarefas 
        putStrLn $ "  * Estudos: " ++ show (tarefasEstudos) ++ " " ++ palavraEstudos ++ " " ++ porcentagemDadoFormatado tarefasEstudos totalTarefas
        putStrLn $ "  * Pessoal: " ++ show (tarefasPessoal) ++ " " ++ palavraPessoal ++ " " ++ porcentagemDadoFormatado tarefasPessoal totalTarefas
        putStrLn $ "  * Outro: " ++ show (tarefasOutro) ++ " " ++ palavraOutro ++ " " ++ porcentagemDadoFormatado tarefasOutro totalTarefas
