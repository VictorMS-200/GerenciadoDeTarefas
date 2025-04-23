# Sistema de gerenciamento de tarefas

## Tipos de dados

### Tarefa
- idTarefa: Int (Identificador único da tarefa)
- Descrição: String
- Prioridade: String (Baixa, Média, Alta)
- Status: String (Pendente, Concluída)
- Categoria: String (Pessoal, Trabalho, Estudo)
- Prazo: String (Data no formato "YYYY-MM-DD")
- Tags: [String] (Lista de tags associadas à tarefa)

```haskell
data Tarefa = Tarefa { idTarefa :: Int, descricao :: String, prioridade :: String, status :: String, categoria :: String, prazo :: String, tags :: [String] } deriving (Show, Eq)
```

### Prioridade
- Parâmetros: Baixa, Media, Alta
- Derivando de: Show, Eq
```haskell
data Prioridade = Baixa | Media | Alta deriving (Show, Eq)
```
### Status
- Parâmetros: Pendente, Concluida
- Derivando de: Show, Eq
```haskell
data Status = Pendente | Concluida deriving (Show, Eq)
```
### Categoria
- Parâmetros: Pessoal, Trabalho, Estudo
- Derivando de: Show, Eq
```haskell
data Categoria = Pessoal | Trabalho | Estudo deriving (Show, Eq)
```

# Funções principais

## adicionarTarefa

```haskell
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
```

- Parâmetros: Tarefa -> [Tarefa]
- Retorna: [Tarefa]

## Funcionalidade:
Adiciona uma tarefa à lista de tarefas passada como parâmetro e retorna a nova lista de tarefas. A tarefa é composta por descrição, prioridade, status, categoria, prazo, tags e identificador.

## Usos:
- Função usada na função principal do sistema para adicionar uma nova tarefa à lista de tarefas. [Ver função [adicionarTarefaMain](#adicionartarefamain)].

# Funções de manipulação de tarefas

## adicionarTarefaMain
```haskell
adicionarTarefaMain :: [Tarefa] -> IO [Tarefa]
```
- Parâmetros: [Tarefa]
- Retorna: IO [Tarefa]


### Funcionalidade:
Adiciona uma tarefa criada a partir dos dados de entrada do usuário e adiciona esta na lista de tarefas informada no parâmetro. A tarefa é composta por descrição, prioridade, status, categoria, prazo, tags e identificador.

### Observação:
- O prazo deve ser uma data válida no formato "YYYY-MM-DD".
- O identificador é criado automaticamente e sua lógica é gerada pelo sistema. Na prática, o identificador é o maior identificador já existente na lista de tarefas + 1, ou se não houver tarefa na lista passada no parâmetro, o identificador será 1.
- O status da tarefa é sempre "Pendente" quando a tarefa é criada.
- Na hora da criação, existe um padrão para alguns campos, como:
    * A prioridade é sempre "Baixa" quando o usuário não informa ou informa um valor inválido.
    * A categoria é sempre "Pessoal" quando o usuário não informa ou informa um valor inválido.
