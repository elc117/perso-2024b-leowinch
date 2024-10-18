{-# LANGUAGE OverloadedStrings #-}


import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text, pack, unpack)


-- tipo para Aluno
data Aluno = Aluno
    { alunoId :: Int
    , nome :: String
    , cpf :: String
    } deriving (Show)


-- lista de alunos (Teste para funções iniciais)
listaDeAlunos :: [Aluno]
listaDeAlunos = 
  [ Aluno 1 "Maria" "123.456.789-00" 
  , Aluno 2 "João" "987.654.321-00" 
  , Aluno 3 "Ana" "111.222.333-44" 
  ]


-- tipo para Turma
data Turma = Turma
    { turmaId   :: Int
    , nomeTurma :: String
    } deriving (Show)

-- Criando uma lista de turmas (Teste para funções iniciais)
listaDeTurmas :: [Turma]
listaDeTurmas =
  [ Turma 1 "Engenharia de Software"
  , Turma 2 "Arquitetura de computadores"
  , Turma 3 "Paradigmas de Programação"
  ]

-- tipo para representar a associção de um aluno com uma turma
data AlunoTurma = AlunoTurma
    { idAluno  :: Int
    , idTurma :: Int
    } deriving (Show)


-- lista de associção de alunos/turmaas (Teste para funções iniciais)
listaAlunoTurma :: [AlunoTurma]
listaAlunoTurma =  
  [ AlunoTurma 1 2
  , AlunoTurma 1 1
  , AlunoTurma 1 3
  , AlunoTurma 2 2
  , AlunoTurma 3 2
  , AlunoTurma 2 3
  ]

-- retorna os alunos de uma determinada turma, filtrando os alunos e de uma turma pelo id da turma 
buscarAlunosTurma :: Int -> [Aluno] -> [AlunoTurma] -> [Aluno]
buscarAlunosTurma idTurmaDesejada alunos alunosTurmas = [aluno | aluno <- alunos, any (\x -> idTurma x == idTurmaDesejada && idAluno x == alunoId aluno) alunosTurmas]

-- adiciona um aluno ao inicio de uma lista de alunos
adicionarAluno :: Aluno -> [Aluno] -> [Aluno]
adicionarAluno  novoAluno lista = novoAluno : lista


-- adiciona uma nova turma a lista de turmas
adicionarTurma :: Turma -> [Turma] -> [Turma]
adicionarTurma novaTurma listaTurmas = novaTurma : listaTurmas

-- adicionar um aluno a uma turma
adicionarAlunoTurma :: AlunoTurma -> [AlunoTurma] -> [AlunoTurma]
adicionarAlunoTurma novoAlunoTurma listaAlunoTurma = novoAlunoTurma : listaAlunoTurma 

-- filtra um aluno pelo id e retorna o mesmo (ou Nothing caso não tenha um aluno correspondente)
filtrarAlunoId :: [Aluno] -> Int -> Maybe Aluno
filtrarAlunoId listaAlunos id = 
  case filter (\a -> alunoId a == id) listaAlunos of
    [] -> Nothing
    (aluno:_) -> Just aluno
 

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    
    -- get para mostrar todos os alunos 
    get "/alunos" $ do
        setHeader "Content-type" "text/plain"
        let responseText = unlines (map show listaDeAlunos)
        text (pack responseText)
