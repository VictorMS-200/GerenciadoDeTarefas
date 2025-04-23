module Main(main) where

import Data.Time.Calendar (Day, fromGregorian)
import System.IO
import Funcoes
import Persistencia
import Tipos

tarefasInicial :: [Tarefa]
tarefasInicial = [ Tarefa 1 "Estudar Haskell" Pendente Alta Estudos (Just (fromGregorian 2025 04 11)) ["ufu", "haskell"], Tarefa 2 "Fazer compras" Concluída Media Pessoal (Just (fromGregorian 2025 04 11)) ["casa"], Tarefa 3 "Finalizar projeto" Pendente Alta Trabalho (Just (fromGregorian 2025 04 11)) ["dev", "haskell"]]

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8

    putStrLn "Bem-vindo ao gerenciador de tarefas!"
    loop tarefasInicial

loop :: [Tarefa] -> IO ()
loop tarefas = do
    listaMenu
    opcao <- getLine
    case opcao of
        "1" -> do
            novasTarefas <- adicionarTarefaMain tarefas
            loop novasTarefas
        "2" -> do
            tarefasAtualizadas <- removerTarefaMain tarefas
            loop tarefasAtualizadas
        "3" -> do
            tarefasAtualizadas <- marcarConcluídaMain tarefas
            loop tarefasAtualizadas
        "4" -> do
            listarPorCategoriaMain tarefas
            loop Tarefas
        "15" -> putStrLn "Saindo do programa..."
        _ -> do
            putStrLn "Opção inválida!"
            loop tarefas

    
