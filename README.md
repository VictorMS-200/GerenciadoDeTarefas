# Sistema de gerenciamento de tarefas

Esse é um sistema de gerenciamento de tarefas feito usando a linguagem Haskell. O sistema permite adicionar, remover e listar tarefas, além de filtrar por prioridade, status e categoria.

# Sumário
- [Como usar](#como-usar)
- [Estrutura do projeto](#estrutura-do-projeto)
- [Tipos de dados](#tipos-de-dados)
    - [Tarefa](#tarefa)
    - [Prioridade](#prioridade)
    - [Status](#status)
    - [Categoria](#categoria)
- [Funções principais](#funções-principais)
  - [adicionarTarefa](#adicionartarefa)
- [Funções de manipulação de tarefas](#funções-de-manipulação-de-tarefas)
  - [adicionarTarefaMain](#adicionartarefamain)


# Como usar
Para compilar e executar o projeto, primeiro, você precisa ter o [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/downloads/) instalado.

Após instalar o GHC, você pode clonar o repositório do projeto e navegar até o diretório `src`:

```bash
cd src
```

E então, você pode compilar o projeto com o seguinte comando:

```bash
ghc -o gerenciadorDeTarefas .\Main.hs 
```

Depois de compilar, você pode executar o programa com o seguinte comando:

```bash
.\gerenciadorDeTarefas
```

#### Obs: o nome {gerenciadorDeTarefas} pode ser qualquer nome que você preferir, desde que não tenha espaços ou caracteres especiais.

# Estrutura do projeto

O projeto é dividido em módulos, cada um responsável por uma parte específica do sistema. A estrutura do projeto é a seguinte:

```
GerenciadoDeTarefas
├── src
│   ├── Funcoes.hs
│   ├── Main.hs
│   ├── Persistencia.hs
│   └── Tipos.hs
├── README.md
├── LICENSE
└── .gitignore
```

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
