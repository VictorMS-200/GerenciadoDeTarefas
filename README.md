# Sistema de gerenciamento de tarefas

Esse é um sistema de gerenciamento de tarefas feito usando a linguagem Haskell. O sistema permite adicionar, remover e listar tarefas, além de filtrar por prioridade, status e categoria.

O projeto foi desenvolvido como parte de um trabalho acadêmico da disciplina de programação funcional da Universidade Federal de Uberlândia (UFU). O objetivo do projeto é praticar os conceitos de programação funcional e aprender a usar a linguagem Haskell.

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
    - [listaMenu](#listamenu)
    - [listarMenu](#listarmenu)
    - [listarMain](#listarmain)
    - [adicionarTarefaMain](#adicionartarefamain)
    - [removerTarefaMain](#removertarefamain)
    - [marcarConcluídaMain](#marcarconcluídamain)
    - [listarGeralMain](#listargeralmain)
    - [listarPorCategoriaMain](#listarporcategoriamain)
    - [listarPorPrioridadeMain](#listarporprioridademain)
    - [ordenarPorPrioridadeMain](#ordenarporprioridademain)
    - [filtrarPorStatusMain](#filtrarporstatusmain)
    - [buscarPorPalavraChaveMain](#buscarporpalavrachavemain)
    - [verificarAtrasosMain](#verificaratrasosmain)
    - [calcularDiasRestantesMain](#calculardiasrestantesmain)
    - [filtrarPorTagMain](#filtrarportagmain)
    - [nuvemDeTagsMain](#nuvemdestagsmain)
    - [salvarEmArquivoMain](#salvaremaquifomain)
    - [carregarDeArquivoMain](#carregardearquivo)
    - [relatorioMain](#relatoriomain)
- [Referências](#referências)


# Como usar
Para compilar e executar o projeto, primeiro, você precisa ter o [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/downloads/) instalado.

Após instalar o GHC, você pode clonar o repositório do projeto usando o seguinte comando:

```bash	
git clone https://github.com/VictorMS-200/GerenciadoDeTarefas.git
```

Navegue até o diretório `src`:

```bash
cd GerenciadoDeTarefas/src
```

Então, você pode compilar o projeto com o seguinte comando:

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
data Tarefa = Tarefa { idTarefa :: Int, descricao :: String, prioridade :: String, status :: String, categoria :: String, prazo :: String, tags :: [String] } deriving (Show, Eq, Read)
```

### Prioridade
- Parâmetros: Baixa, Media, Alta
- Derivando de: Show, Eq
```haskell
data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Read)
```
### Status
- Parâmetros: Pendente, Concluida
- Derivando de: Show, Eq
```haskell
data Status = Pendente | Concluida deriving (Show, Eq, Read)
```
### Categoria
- Parâmetros: Pessoal, Trabalho, Estudo
- Derivando de: Show, Eq
```haskell
data Categoria = Pessoal | Trabalho | Estudo deriving (Show, Eq, Read)
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
Recebe uma tarefa e imprime suas informações no console formatada.

- Exemplo: `mostrarTarefa {idTarefa = 1, descricao = "Estudar Haskell", status = Pendente, prioridade = Alta, categoria = Estudos, prazo = Just 2025-04-11, tags = ["ufu","haskell"]}` imprime:
```
ID: 1, Descrição: Estudar Haskell, Status: Pendente, Prioridade: Alta, Categoria: Estudos, Prazo: Just 2025-04-11, Tags: ["ufu","haskell"]
```

## validarIdentificador

```haskell
validarIdentificador :: Int -> [Tarefa] -> Tarefa -> Maybe [Tarefa]
```

- Parâmetros: Int -> [Tarefa] -> Tarefa
- Retorna: Maybe [Tarefa]

### Funcionalidade:
Verifica se o identificador da tarefa já existe na lista de tarefas. Se existir, retorna Nothing. Caso contrário, retorna Just com a lista de tarefas com a nova tarefa adicionada.

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
Remove uma tarefa da lista de tarefas com base no identificador. Se a tarefa não for encontrada, retorna Nothing. Caso contrário, retorna Just com a nova lista de tarefas.

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
Recebe uma categoria e uma lista de tarefas, e retorna a lista de tarefas filtradas pela categoria. Se a categoria não for encontrada, retorna uma lista vazia.

## listarPorPrioridade

```haskell
listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
```

- Parâmetros: Prioridade -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:

Recebe uma prioridade e uma lista de tarefas, e retorna a lista de tarefas filtradas pela prioridade. Se a prioridade não for encontrada, retorna uma lista vazia.

## ordenarPorPrioridade

```haskell
ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
```
- Parâmetros: [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:

Recebe uma prioridade e uma lista de tarefas, e retorna a lista de tarefas filtradas pela prioridade. Se a prioridade não for encontrada, retorna uma lista vazia.

### observação:
- A prioridade é ordenada da seguinte forma: Alta, Média, Baixa. As tarefas com prioridade mais alta aparecem primeiro na lista.

## filtrarPorStatus

```haskell
filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
```

- Parâmetros: Status -> [Tarefa]
- Retorna: [Tarefa]

### Funcionalidade:

Recebe um status e uma lista de tarefas, e retorna a lista de tarefas filtradas pelo status. Se o status não for encontrado, retorna uma lista vazia.

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

Recebe uma lista de tarefas e uma data do tipo Day (Data.Time.Calendar), e retorna a lista de tarefas que estão atrasadas em relação à data informada. Se não houver tarefas atrasadas, retorna uma lista vazia.

## calcularDiasRestantes

```haskell
calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
```

- Parâmetros: Tarefa -> Day
- Retorna: Maybe Int

### Funcionalidade:

Calcula a quantidade de dias restantes para o prazo de uma tarefa. Se o Day (Data.Time.Calendar) for Nothing, retorna Nothing. Caso contrário, retorna Just com a quantidade de dias restantes.

## salvarEmArquivo

```haskell
salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
```

- Parâmetros: FilePath -> [Tarefa]
- Retorna: IO ()

### Funcionalidade:

Salva uma lista de tarefas em um arquivo no caminho especificado pelo parâmetro FilePath.

## carregarDeArquivo

```haskell
carregarDeArquivo :: FilePath -> IO [Tarefa]
```

- Parâmetros: FilePath
- Retorna: IO [Tarefa]

### Funcionalidade:

Carrega a lista de tarefas de um arquivo no caminho especificado pelo parâmetro FilePath e retorna com o formato de uma lista de tarefas.

## filtrarPorTag

```haskell
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
```

- Parâmetros: String -> [Tarefa]
- Retorna: [Tarefa]

## Funcionalidade:

Filtra as tarefas com base na tag informada. Retorna uma lista de tarefas que contém a tag especificada. Se não houver tarefas com a tag, retorna uma lista vazia.

## nuvemDeTags

```haskell
nuvemDeTags :: [Tarefa] -> [(String, Int)]
```

- Parâmetros: [Tarefa]
- Retorna: [(String, Int)]

### Funcionalidade:

Recebe uma lista de tarefas e retorna uma lista de tuplas com as tags e a quantidade de tarefas associadas a cada tag.

# Funções de manipulação de tarefas

Essas funções são responsáveis por coletar os dados de entrada do usuário e chamar as funções principais para realizar as operações desejadas.

## listaMenu
```haskell
listaMenu :: IO ()
```
- Retorna: IO ()

### Funcionalidade:
Exibe um menu de opções de como listar as tarefas para o usuário. Essa função é chamada na função [listarMain](#listarmain) quando o usuário escolhe a opção de listar tarefas.

- As opções disponíveis são:
    * Listar todas as tarefas
    * Listar tarefas por categoria
    * Listar tarefas por prioridade
    * Listar tarefas por status
    * Listar tarefas por palavra-chave
    * Listar tarefas com ordem por prioridade
    * Listar tarefas com prazo atrasado

## listarMenu
```haskell
listarMenu :: IO ()
```
- Retorna: IO ()

## Funcionalidade:

Exibe o [menu lista](#listaMenu) para o usuário, coleta a opção desejada e chama a função correspondente. Se a opção não for válida, exibe uma mensagem de erro e chama a função novamente.

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

## removerTarefaMain
```haskell
removerTarefaMain :: [Tarefa] -> IO [Tarefa]
```

- Parâmetros: [Tarefa]
- Retorna: IO [Tarefa]

### Funcionalidade:
Remove uma tarefa da lista de tarefas com base no identificador. Se a tarefa não for encontrada, retorna uma mensagem de erro e chama a função novamente. Caso contrário, retorna a nova lista de tarefas.

### Observação:
- O identificador deve ser um número inteiro positivo. Caso contrário, o sistema irá retornar um erro e irá pedir para o usuário informar um novo identificador.

## marcarConcluídaMain
```haskell
marcarConcluídaMain :: [Tarefa] -> IO [Tarefa]
```

- Parâmetros: [Tarefa]
- Retorna: IO [Tarefa]

### Funcionalidade:
Marca uma tarefa como concluída com base no identificador. Se a tarefa não for encontrada, retorna uma mensagem de erro e chama a função novamente. Caso contrário, retorna a nova lista de tarefas.

### Observação:
- O identificador deve ser um número inteiro positivo. Caso contrário, o sistema irá retornar um erro e irá pedir para o usuário informar um novo identificador.

## listarGeralMain
```haskell
listarGeralMain :: [Tarefa] -> IO ()
```
- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Mostrar todas as tarefas cadastradas no sistema. Essa função é chamada quando o usuário escolhe a opção de listar tarefas na função [listarMain](#listarmain).

## listarPorCategoriaMain

```haskell
listarPorCategoriaMain :: [Tarefa] -> IO ()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Lista as tarefas filtradas de acordo com a categoria informada pelo usuário. Essa função é chamada quando o usuário escolhe a opção de listar tarefas por categoria na função [listarMain](#listarmain).

### Observação:

- As categorias válidas são: "Pessoal", "Trabalho" e "Estudo". Caso o usuário informe uma categoria inválida, o sistema irá retornar um erro e irá retornar a função novamente.

## listarPorPrioridadeMain

```haskell
listarPorPrioridadeMain :: [Tarefa] -> IO()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Lista as tarefas filtradas de acordo com a prioridade informada pelo usuário. Essa função é chamada quando o usuário escolhe a opção de listar tarefas por prioridade na função [listarMain](#listarmain).

### Observação:
- As prioridades válidas são: "Baixa", "Média" e "Alta". Caso o usuário informe uma prioridade inválida, o sistema irá retornar um erro e irá retornar a função novamente.

## ordenarPorPrioridadeMain
    
```haskell
ordenarPorPrioridadeMain :: [Tarefa] -> IO ()
```
- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Lista as tarefas ordenadas de acordo com a prioridade. A prioridade é ordenada da seguinte forma: Alta, Média, Baixa. Essa função é chamada quando o usuário escolhe a opção de listar tarefas por prioridade na função [listarMain](#listarmain).


## filtrarPorStatusMain

```haskell
filtrarPorStatusMain :: [Tarefa] -> IO ()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

## Funcionalidade:
Filtra as tarefas de acordo com o status informado pelo usuário. Essa função é chamada quando o usuário escolhe a opção de listar tarefas por status na função [listarMain](#listarmain).

### Observação:
- Os status válidos são: "Pendente" e "Concluída". Caso o usuário informe um status inválido, o sistema irá retornar um erro e irá retornar a função novamente.

## buscarPorPalavraChaveMain

```haskell
buscarPorPalavraChaveMain :: [Tarefa] -> IO ()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Filtra as tarefas de acordo com a palavra-chave informada pelo usuário. Essa função é chamada quando o usuário escolhe a opção de buscar tarefas por palavra-chave na função [listarMain](#listarmain).

### Observação:
- A palavra-chave deve ser uma string. Caso o usuário informe nenhum valor, o sistema irá retornar um erro e irá chamar a função novamente.

## verificarAtrasosMain

```haskell
verificarAtrasosMain :: [Tarefa] -> IO ()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Retorna as tarefas que estão atrasadas em relação à data atual. Essa função é chamada quando o usuário escolhe a opção de listar tarefas com prazo atrasado na função [listarMain](#listarmain).

### Observação:
- A data atual é obtida através do módulo Data.Time.LocalTime.

## calcularDiasRestantesMain

```haskell
calcularDiasRestantesMain :: [Tarefa] -> IO ()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:

Calcula a quantidade de dias restantes para o prazo de uma tarefa. Se o dia da tarefa for Nothing, retorna uma mensagem descritiva. Caso contrário, retorna a quantidade de dias restantes.

### Observação:
- Se o prazo já tiver passado, o sistema irá retornar uma mensagem falando quantos dias já passou do prazo.

### filtrarPorTagMain

```haskell
filtrarPorTagMain :: [Tarefa] -> IO ()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

## Funcionalidade:
Filtra as tarefas de acordo com a tag informada pelo usuário. 

## nuvemDeTagsMain

```haskell
nuvemDeTagsMain :: [Tarefa] -> IO ()
```
- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:

Mostra as tags cadastradas no sistema e a quantidade de tarefas associadas a cada tag.

## salvarEmArquivoMain

```haskell
salvarEmArquivoMain :: [Tarefa] -> IO ()
```

- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Salva a lista de tarefas em um arquivo. O usuário deve informar o nome do arquivo onde as tarefas serão salvas.

### Observação:
- O sistema irá sobrescrever o arquivo se ele já existir.
- O arquivo será salvo no mesmo diretório onde o programa está sendo executado.
- O arquivo tem o nome de "tarefas.txt" e o formato de texto simples.

### carregarDeArquivoMain
    
```haskell
carregarDeArquivoMain :: IO [Tarefa]
```

- Parâmetros: IO [Tarefa]
- Retorna: IO [Tarefa]

### Funcionalidade:
Carrega a lista de tarefas do arquivo "tarefas.txt".

### Observação:
- Os dados que já estiverem no sistema, serão sobrescritos pelos dados do arquivo.

## relatorioMain 

```haskell 
relatorioMain :: [Tarefa] -> IO ()
```
- Parâmetros: [Tarefa]
- Retorna: IO ()

### Funcionalidade:
Mostra o total de tarefas cadastrados, as tarefas pendentes e concluídas e a porcentagem em relação ao total de tarefas, além de mostrar a distribuição de tarefas por categoria e sua porcentagem em relação ao total de tarefas.

## Referências
- [Haskell Wiki](https://wiki.haskell.org/Haskell)
- [Data Time Calendar](https://hackage.haskell.org/package/time-1.14/docs/Data-Time-Calendar.html)
- [Data Time LocalTime](https://hackage.haskell.org/package/time-1.14/docs/Data-Time-LocalTime.html)
- [GHC](https://www.haskell.org/ghc/)
- [mapM_](https://stackoverflow.com/questions/27609062/what-is-the-difference-between-mapm-and-mapm-in-haskell)
- [writeFile](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:writeFile)
- [isInfixOf](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:isInfixOf)
- [comparing](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Ord.html#v:comparing)