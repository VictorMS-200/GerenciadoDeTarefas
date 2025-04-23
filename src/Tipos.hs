module Tipos(Status(..), Prioridade(..), Categoria(..), Tarefa(..)) where

import Data.Time.Calendar 

data Status = Pendente | Concluída deriving (Show, Eq)

data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Ord)

data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Show, Eq)

-- Registro tarefa
data Tarefa = Tarefa { 
idTarefa :: Int
, descricao :: String
, status :: Status
, prioridade :: Prioridade
, categoria :: Categoria
, prazo :: Maybe Day
, tags :: [String]
} deriving (Show, Eq)