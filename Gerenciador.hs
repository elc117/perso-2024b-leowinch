{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic) -- Facilita a conversão dos dados abstratos para Json, sem que precise implementar as funções de conversão manualmente
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Aeson (FromJSON, ToJSON, decode, encode, eitherDecode, (.=), object) -- Biblioteca para manipular Json em Haskell
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)


-- tipo para Aluno
data Aluno = Aluno
    { alunoId :: Int
    , nome :: String
    , cpf :: String
    } deriving (Generic, Show)


instance FromJSON Aluno
instance ToJSON Aluno 


-- tipo para Turma
data Turma = Turma
    { turmaId   :: Int
    , nomeTurma :: String
    } deriving (Generic, Show)

instance FromJSON Turma
instance ToJSON Turma

-- tipo para representar a associção de um aluno com uma turma
data AlunoTurma = AlunoTurma
    { idAluno  :: Int
    , idTurma :: Int
    } deriving (Generic, Show)

instance FromJSON AlunoTurma
instance ToJSON AlunoTurma

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

 
filtrarTurmasDeUmAluno :: Int -> [Turma] -> [AlunoTurma] -> [Turma]
filtrarTurmasDeUmAluno idaluno listaTurmas listaAlunoTurma = [turma | turma <- listaTurmas, any (\x -> idAluno x == idaluno && idTurma x == turmaId turma) listaAlunoTurma] 

-- filtra um aluno pelo id e retorna o mesmo (ou Nothing caso não tenha um aluno correspondente)
filtrarAlunoId :: [Aluno] -> Int -> Maybe Aluno
filtrarAlunoId listaAlunos id = 
  case filter (\a -> alunoId a == id) listaAlunos of
    [] -> Nothing
    (aluno:_) -> Just aluno
 

-- tenta ler o conteúdo do arquivo json apontado por "caminho" e caso consiga retorna uma lista do tipo x, caso não consiga retorna uma String com mensagem de erro 
lerArquivoJson :: FromJSON x => FilePath -> IO (Either String [x])
lerArquivoJson caminho = do 
   dados <- B.readFile caminho
   return (eitherDecode dados)

-- ler os Alunos da lista de alunos dentro do "alunos.json" (retorna uma lista do tipo Aluno com os alunos contidos no arquivo)
lerAlunos :: FilePath -> IO (Either String [Aluno])
lerAlunos = lerArquivoJson

-- ler as turmas da lista de turmas dentro de "turmas.json" (retorna uma lista do tipo Turma ou uma mensagem de erro)
lerTurmas :: FilePath -> IO (Either String [Turma])
lerTurmas = lerArquivoJson

-- ler as relações de aluno/turma de "alunoturma.json" (retorna uma lista de AlunoTurma ouuma mensagem de erro)
lerAlunoTurma :: FilePath -> IO (Either String [AlunoTurma])
lerAlunoTurma = lerArquivoJson

-- escreve a lista de alunos no arquivo passado em caminho formatado em json (caso não exista, o arquivo é criado)
escreverJsonAlunos :: FilePath -> [Aluno] -> IO ()
escreverJsonAlunos caminho listaAlunos = B.writeFile caminho (encode listaAlunos)

-- escreve a lista de turmas no arquivo passado em caminho formatado em json (caso não exista o arquivo é criado)
escreverJsonTurmas :: FilePath -> [Turma] -> IO ()
escreverJsonTurmas caminho listaTurma = B.writeFile caminho (encode listaTurma)

-- escreve a lista de relção entre aluno/turma no arquivo passado em caminho formatado em json(ou cria o arquivo caso ele não exista)
escreverJsonAlunoTurma :: FilePath -> [AlunoTurma] -> IO ()
escreverJsonAlunoTurma caminho listaAlunoTurma = B.writeFile caminho (encode listaAlunoTurma)


-- Função para criar um arquivo JSON vazio se ele não existir
criarArquivoVazio :: FilePath -> IO ()
criarArquivoVazio caminho = do
    existe <- doesFileExist caminho
    if not existe
        then B.writeFile caminho "[]"
        else return ()



main :: IO ()
main = do
  criarArquivoVazio "turmas.json"
  criarArquivoVazio "alunos.json"
  criarArquivoVazio "alunoTurma.json"
    
  scotty 3000 $ do
    middleware logStdoutDev
    
    -- get para mostrar todos os alunos 
    get "/alunos" $ do
        setHeader "Content-type" "application/json"
        response <- liftIO $ lerAlunos "alunos.json"
        case response of 
          Left mensagemErro -> json $ object ["erro ao abrir arquivo dos alunos" .= mensagemErro]
          Right alunos -> json alunos 
    
    -- get para mostrar alunos de uma turma
    get "/alunosdeumaturma/:idturma" $ do
       setHeader "Content-type" "application/json"
       idTurmaDesejada <- param "idturma" :: ActionM Int
       listaAlunosLida <- liftIO $ lerAlunos "alunos.json"
       listaAlunosTurmaLida <- liftIO $ lerAlunoTurma "alunoTurma.json"
       case (listaAlunosLida, listaAlunosTurmaLida) of 
          (Right listaAlunos, Right listaAlunosTurma) -> do 
            let alunosTurma = buscarAlunosTurma idTurmaDesejada listaAlunos listaAlunosTurma
            json alunosTurma
          (Left erroAlunos, _) -> json $ object ["erro ao abrir arquivo dos alunos" .= erroAlunos]
          (_, Left erroAlunoTurma) -> json $ object ["erro ao abrir arquivo de aluno/turma" .= erroAlunoTurma]

    -- get para mostrar todas as turmas
    get "/turmas" $ do
        setHeader "Content-type" "application/json"
        response <- liftIO $ lerTurmas "turmas.json"
        case response of
          Left mensagemErro -> json $ object ["erro ao abrir arquivo de turmas" .= mensagemErro]
          Right turmas -> json turmas
   
    -- get para mostrar as turmas de um aluno
    get "/turmasdeumaluno/:idaluno" $ do
       setHeader "Content-type" "application/json"
       idaluno <- param "idaluno" :: ActionM Int
       listaTurmasLida <- liftIO $ lerTurmas "turmas.json"
       listaAlunoTurmaLida <- liftIO $ lerAlunoTurma "alunoTurma.json"
       case (listaTurmasLida, listaAlunoTurmaLida) of 
         (Right listaTurmas, Right listaAlunosTurma) -> do
           let turmasAluno = filtrarTurmasDeUmAluno idaluno listaTurmas listaAlunosTurma
           json turmasAluno
         (Left erroTurmas,_) -> json $ object ["erro ao abrir o arquivo de turmas" .= erroTurmas]
         (_, Left erroAlunoTurma) -> json $ object ["erro ao abrir o arquivo AlunoTurma" .= erroAlunoTurma]

    -- get para mostar uma turma filtrada por id
    get "/turmaporid/:idturma" $ do
        setHeader "Content-type" "application/json"
        idTurma <- param "idturma" :: ActionM Int
        listaTurmasLidas <- liftIO $ lerTurmas "turmas.json"
        case listaTurmasLidas of
          Right turmas -> do
            let turma = filter (\x -> turmaId x == idTurma) turmas
            json turma
          Left mensagemErro -> json $ object ["erro ao abrir arquivo de turmas" .= mensagemErro]

    -- get para mostrar aluno filtrado por id
    get "/alunoporid/:idaluno" $ do
        setHeader "Content-type" "application/json"
        idAluno <- param "idaluno" :: ActionM Int
        listaAlunosLidas <- liftIO $ lerAlunos "alunos.json"
        case listaAlunosLidas of 
          Right alunos -> do
            let aluno = filter (\x -> alunoId x == idAluno) alunos
            json aluno
          Left mensagemErro -> json $ object ["erro ao abrir o arquivo de alunos" .= mensagemErro]

    post "/novoaluno" $ do
      setHeader "Content-type" "application/json"
      requisicao <- body
      let novoAluno = decode requisicao :: Maybe Aluno
      case novoAluno of
        Just aluno -> do
          listaAlunosLida <- liftIO $ lerAlunos "alunos.json"
          case listaAlunosLida of
            Right alunos -> do
             let novalistaAlunos = adicionarAluno aluno alunos
             liftIO $ escreverJsonAlunos "alunos.json" novalistaAlunos
            Left mensagemErro -> json $ object ["erro ao abrir arquivo de alunos" .= mensagemErro]
          json ("Aluno inserido com sucesso" :: String)
        Nothing -> do
          json ("Erro ao decodificar JSON" :: String) 
    

    post "/novaturma" $ do
      setHeader "Content-type" "application/json"
      requisicao <- body
      let novaTurma = decode requisicao :: Maybe Turma
      case novaTurma of
        Just turma -> do
          listaTurmasLida <- liftIO $ lerTurmas "turmas.json"
          case listaTurmasLida of
            Right turmas -> do
              let novalistaTurmas = adicionarTurma turma turmas
              liftIO $ escreverJsonTurmas "turmas.json" novalistaTurmas
            Left mensagemErro -> json $ object ["erro ao abrir arquivo de turmas" .= mensagemErro]
          json ("Turma inserida com sucesso" :: String) 
        Nothing -> do
          json ("Erro ao decodificar JSON" :: String)
          
    post "/inseriralunoturma" $ do
      setHeader "Content-type" "application/json"
      requisicao <- body
      let novoAlunoTurma = decode requisicao :: Maybe AlunoTurma
      case novoAlunoTurma of
        Just alunoTurma -> do
          listaAlunoTurmaLida <- liftIO $ lerAlunoTurma "alunoTurma.json"
          case listaAlunoTurmaLida  of
            Right alunosTurmas -> do
              let novalistaAlunoTurma = adicionarAlunoTurma alunoTurma alunosTurmas
              liftIO $ escreverJsonAlunoTurma "alunoTurma.json" novalistaAlunoTurma
            Left mensagemErro -> json $ object ["erro ao abrir arquivo de alunos/turmas" .= mensagemErro]
          json ("relação inserida com sucesso" :: String) 
        Nothing -> do
          json ("Erro ao decodificar JSON" :: String)