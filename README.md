# Simulador de Gerenciador de turmas/alunos

### Identificação
- **Nome:** Leonardo Winch Dallanora  
- **Curso:** Universidade Federal de Santa Maria, Bacharelado em Sistemas de Informação  

---

### Tema/Objetivo
O trabalho consiste na criação de um simulador de gerenciador de turmas de alunos. As principais funcionalidades incluem:

- Visualizar todos os alunos e filtrar alunos por ID.
- Visualizar todas as turmas e filtrar turmas por ID.
- Visualizar todos os alunos de uma turma específica.
- Visualizar as turmas de um aluno selecionado por ID.
- Adicionar um aluno à lista de alunos, criar uma nova turma e associar um aluno a uma turma, utilizando o método HTTP POST.


O armazenamento e a manipulação dos dados são realizados em formato JSON, o que facilita a organização. A estrutura do sistema utiliza métodos GET e POST para gerenciamento de informações.

---

### Processo de Desenvolvimento
  Para o desenvolvimento desse programa foi necessário adiquirir conhecimentos novos para utilizar na construção do programa. Primeiramente fui atrás de entender os tipos abstratos de dados (data) para representar os alunos, turmas e a relação
de um aluno com uma turma. Após conseguir representar em tipo abstrato, fiz as funções que fazem os filtros especificados anteriormente. Nessa parte não houve muitas dificuldades já que os conceitos aplicados são muito parecidos com os vistos 
em aulas e treinados nos exercícios ao longo do semestre. Após isso, fiz os métodos GET em formato de texto, como mostrado em aula, sem muitas dificuldades, para tirar dúvidas visitei o site do [Scotty](https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html) .
Depois disso fui pesquisar maneiras de manipular arquivos em Haskell e encontrei a biblioteca [Data.ByteString](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html), na qual foi utilizada as funções para manipulações de arquivos.
Depois, fui buscar sobre utilização de json em Haskell e cheguei na biblioteca [Aeson](https://hackage.haskell.org/package/aeson), porém percebi que teria que fazer muitas funções de conversão de tipos. Para facilitar esse processo, pesquisei no ChatGPT,
uma maneira mais simples de lidar com isso, com isso ele me apresentou a biblioteca [GHC.Generic](https://wiki.haskell.org/Generics), na qual facilitou bastante, diminuindo a implementação de conversões. Por fim, implementei os métodos POST para fazer inserções
de novos alunos, turmas e inserção de alunos em turmas.

---

### Resultado Final
#### link para vídeo de teste: [Vídeo](https://drive.google.com/file/d/1qgDZeoIGJ-91CaHtbwIg4J24_Q9l2pJb/view?usp=drive_link)


#### Primeiro end-point: localhost:3000/novoaluno (Post)
Insere um aluno em "alunos.json",  OBS: Os POST foram feitos através do Thunder Client para conseguir enviar um json no corpo da requisição.

dados utilizados: **{"alunoId":1,"cpf":"123.456.789-00","nome":"Maria"}** e **{"alunoId":2,"cpf":"111.222.333-44","nome":"Leonardo"}**.

#### Segundo end-point: localhost:3000/alunos (Get)
Irá mostrar todos os alunos inseridos em "alunos.json".

#### Terceiro end-point: localhost:3000/novaturma (Post)
Insere uma turma em "turmas.json"

dados utilizados: **{"turmaId": 1, "nomeTurma": "Paradigmas de Linguagens de Programacao"}** e **{"turmaId": 2, "nomeTurma": "Arquitetura de Computadores"}**.

#### Quarto end-point: localhost:3000/turmas (Get)
Irá mostrar todas as turmas inseridas em "turmas.json".

#### Quinto end-point: localhost:3000/inseriralunoturma (Post)
Insere uma nova relação aluno/turma, assim inserindo um aluno em uma turma.

dados utilizados: **{"idAluno": 1,"idTurma": 1}** e **{"idAluno": 2,"idTurma": 2}**

#### Sexto end-point: localhost:3000/alunosdeumaturma/2 (Get)
Irá mostrar todos os alunos de uma determinada turma, nesse caso será a turma com ID = 2.

#### Sétimo end-point: localhost:3000/turmasdeumaluno/2 (Get)
Irá mostrar todas as turmas de um determinado aluno, nesse caso será o aluno com ID = 2.

#### Oitavo end-point: localhost:3000/turmaporid/1 (Get)
Irá mostrar uma turma específica de acordo com o ID, nesse caso será a turma com ID = 1.

#### Nono end-point: localhost:3000/alunoporid/2 (Get)
Irá mostrar um aluno específico de acordo com o ID, nesse caso será o aluno com ID = 1.


---

### Referências e Créditos
- [Scotty - Web Framework para Haskell](https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html)  
- [ChatGPT - Tipos abstratos de dados em Haskell](https://chatgpt.com/)  
- [Aeson - Biblioteca para JSON](https://hackage.haskell.org/package/aeson)  
- [Data.ByteString - Manipulação de strings em Haskell](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html)  
