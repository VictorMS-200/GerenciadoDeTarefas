# Sistema de gerenciamento de tarefas

Esse é um sistema de gerenciamento de tarefas feito usando a linguagem Haskell. O sistema permite adicionar, remover e listar tarefas, além de filtrar por prioridade, status e categoria.

# Sumário
- [Como usar](#como-usar)
- [Tipos de dados](#tipos-de-dados)
    - [Tarefa](#tarefa)
    - [Prioridade](#prioridade)
    - [Status](#status)
    - [Categoria](#categoria)
- [Funções principais](#funções-principais)
    - [mostrarTarefas](#mostrartarefas)
    - [porcentagemDadoFormatado](#porcentagemdadodeformatado)
    - [buscarTarefaPorIdentificador](#buscartarefaporidentificador)
    - [mostrarTarefa](#mostrartarefa)
    - [validarIdentificador](#validaridentificador)
    - [adicionarTarefa](#adicionartarefa)
    - [removerTarefa](#removertarefa)
    - [marcarConcluída](#marcarconcluída)
    - [listarPorCategoria](#listarporcategoria)
    - [listarPorPrioridade](#listarporprioridade)
    - [ordenarPorPrioridade](#ordenarporprioridade)
    - [filtrarPorStatus](#filtrarporstatus)
    - [buscarPorPalavraChave](#buscarporpalavrachave)
    - [verificarAtrasos](#verificaratrasos)
    - [calcularDiasRestantes](#calculardiasrestantes)
    - [salvarEmArquivo](#salvaremaquivo)
    - [carregarDeArquivo](#carregardearquivo)
    - [filtrarPorTag](#filtrarportag)
- [Funções de manipulação de tarefas](#funções-de-manipulação-de-tarefas)
  - [adicionarTarefaMain](#adicionartarefamain)
- [Referências](#referências)


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

Essas são funções na qual são usadas para manipular as tarefas. Elas são responsáveis por adicionar, remover e listar tarefas, além de filtrar por prioridade, status e categoria.
Essas funções são usadas nas funções de manipulação de tarefas, que são responsáveis por interagir com o usuário e coletar os dados necessários para manipular as tarefas.

## mostrarTarefas

```haskell
mostrarTarefas :: [Tarefa] -> IO ()
```
- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Recebe uma lista de tarefas e imprime cada tarefa no console.

## porcentagemDadoFormatado

```haskell
porcentagemDadoFormatado :: Int -> Int -> String
```

- Parâmetros: Int -> Int
- Retorna: String

### Funcionalidade:
Recebe dois números inteiros e retorna uma string formatada com a porcentagem do primeiro número em relação ao segundo.

- Exemplo: `porcentagemDadoFormatado 5 10` retorna `" (50%)"`.

## buscarTarefaPorIdentificador
    
```haskell
buscarTarefaPorIdentificador :: [Tarefa] -> Int -> Maybe Tarefa
```

- Parâmetros: [Tarefa] -> Int
- Retorna: Maybe Tarefa

### Funcionalidade:
Recebe uma lista de tarefas e um identificador, e retorna a primeira tarefa que possui o identificador. Se não encontrar, retorna Nothing.

## mostrarTarefa

```haskell
mostrarTarefa :: Tarefa -> IO ()
```

- Parâmetros: Tarefa
- Retorna: IO ()

### Funcionalidade:
Imprime os detalhes de uma tarefa no console. A tarefa é composta por descrição, prioridade, status, categoria, prazo, tags e identificador.

## validarIdentificador

```haskell
validarIdentificador :: Int -> [Tarefa] -> Tarefa -> Maybe [Tarefa]
```

- Parâmetros: Int -> [Tarefa] -> Tarefa
- Retorna: Maybe [Tarefa]

### Funcionalidade:
Verifica se o identificador da tarefa já existe na lista de tarefas. Se existir, retorna Nothing. Caso contrário, retorna Just com a lista de tarefas.

## adicionarTarefa

```haskell
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
```

- Parâmetros: Tarefa -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:
Adiciona uma tarefa à lista de tarefas passada como parâmetro e retorna a nova lista de tarefas. A tarefa é composta por descrição, prioridade, status, categoria, prazo, tags e identificador.

## removerTarefa

```haskell
removerTarefa :: Int -> [Tarefa] ->  Maybe [Tarefa]
```

- Parâmetros: Int -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:
Remove uma tarefa da lista de tarefas com base no identificador. Se a tarefa não for encontrada, retorna a lista original.

## marcarConcluída

```haskell
marcarConcluída :: Int -> [Tarefa] -> Maybe [Tarefa]
```

- Parâmetros: Int -> [Tarefa]
- Retorna: Maybe [Tarefa]

### Funcionalidade:
Marca uma tarefa como concluída com base no identificador. Se a tarefa não for encontrada, retorna Nothing.

## listarPorCategoria

```haskell
listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
```

- Parâmetros: Categoria -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:
Lista as tarefas filtradas por categoria. Se a categoria não for encontrada, retorna uma lista vazia.

## listarPorPrioridade

```haskell
listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
```

- Parâmetros: Prioridade -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:

Lista as tarefas filtradas por prioridade. Se a prioridade não for encontrada, retorna uma lista vazia.

## ordenarPorPrioridade

```haskell
ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
```
- Parâmetros: [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:

Ordena as tarefas por prioridade. As prioridades são ordenadas da seguinte forma: Baixa, Média, Alta. Se a prioridade não for encontrada, retorna a lista original.

## filtrarPorStatus

```haskell
filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
```

- Parâmetros: Status -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:

Filtra as tarefas com base no status. Se o status não for encontrado, retorna uma lista vazia.

## buscarPorPalavraChave

```haskell
buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
```

- Parâmetros: String -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:

Recebe uma palavra e uma lista de tarefas, e retorna a lista de tarefas com as tarefas que contem a palavra informada na descrição das tarefas.

## verificarAtrasos

```haskell
verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
```

- Parâmetros: [Tarefa] -> Day
- Retorna: [Tarefa]

### Funcionalidade:

Verifica se há tarefas com prazo vencido. Se houver, retorna uma lista com as tarefas que estão atrasadas. Caso contrário, retorna uma lista vazia.

## calcularDiasRestantes

```haskell
calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
```

- Parâmetros: Tarefa -> Day
- Retorna: Maybe Int

### Funcionalidade:

Calcula a quantidade de dias restantes para o prazo de uma tarefa. Se o prazo não for válido, retorna Nothing. Caso contrário, retorna Just com a quantidade de dias restantes.

## salvarEmArquivo

```haskell
salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
```

- Parâmetros: FilePath -> [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Salva a lista de tarefas em um arquivo no formato CSV. O arquivo é salvo no caminho especificado pelo parâmetro FilePath.

## carregarDeArquivo

```haskell
carregarDeArquivo :: FilePath -> IO [Tarefa]
```

- Parâmetros: FilePath
- Retorna: IO [Tarefa]

### Funcionalidade:

Carrega a lista de tarefas de um arquivo no formato CSV. O arquivo é carregado do caminho especificado pelo parâmetro FilePath. Se o arquivo não existir ou não puder ser lido, retorna uma lista vazia.

## filtrarPorTag

```haskell
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
```

- Parâmetros: String -> [Tarefa]
- Retorna: [Tarefa]

## Funcionalidade:

Filtra as tarefas com base em uma tag. Se a tag não for encontrada, retorna uma lista vazia.


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
- O prazo deve ser uma data válida no formato "YYYY-MM-DD". Caso contrário, o sistema irá retornar um erro e irá usar o valor Nothing para o prazo.
- O identificador não pode ser duplicado. Caso o usuário informe um identificador já existente, o sistema irá retornar um erro e o usuário deverá informar um novo identificador.
- O status da tarefa é sempre "Pendente" quando a tarefa é criada.
- Na hora da criação, existe um padrão para alguns campos, como:
    * A prioridade é sempre "Baixa" quando o usuário não informa ou informa um valor inválido.
    * A categoria é sempre "Pessoal" quando o usuário não informa ou informa um valor inválido.


## Referências
- [Haskell Wiki](https://wiki.haskell.org/Haskell)
- [Data Time Calendar](https://hackage.haskell.org/package/time-1.14/docs/Data-Time-Calendar.html)
- [Data Time LocalTime](https://hackage.haskell.org/package/time-1.14/docs/Data-Time-LocalTime.html)
- [GHC](https://www.haskell.org/ghc/)
- [mapM_](https://stackoverflow.com/questions/27609062/what-is-the-difference-between-mapm-and-mapm-in-haskell)
- [writeFile](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:writeFile)
- [isInfixOf](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:isInfixOf)