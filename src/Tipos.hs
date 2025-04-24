module Tipos(Status(Pendente, Concluída), Prioridade(Baixa, Media, Alta), Categoria(Trabalho, Estudos, Pessoal, Outro)
, Tarefa(Tarefa), idTarefa, descricao, status, prioridade, categoria, prazo, tags) where


import Data.Time.Calendar (Day)

data Status = Pendente | Concluída deriving (Show, Eq, Read)

data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Ord, Read)

data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Show, Eq, Read)

-- Registro tarefa
data Tarefa = Tarefa { 
idTarefa :: Int
, descricao :: String
, status :: Status
, prioridade :: Prioridade
, categoria :: Categoria
, prazo :: Maybe Day
, tags :: [String]
} deriving (Show, Eq, Read)