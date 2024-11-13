       IDENTIFICATION DIVISION.
       PROGRAM-ID. cadastrarFuncionario.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FUNCIONARIO-FILE ASSIGN TO "dadosFuncionario.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FUNCIONARIO-FILE.
       01 FUNCIONARIO-RECORD.
           05 FUNCIONARIO-DADOS PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-OPTION             PIC 9(5) VALUE 0.
       01 WS-DADOS-FUNCIONARIO.
               05 CODIGO                PIC 9(5).
               05 NOME                  PIC X(30).
               05 MATRICULA             PIC X(10).
               05 CARGO                 PIC X(20).
               05 DEPARTAMENTO          PIC X(20).
               05 SALARIO               PIC 9(9)V99.
               05 DATA_ADMISSAO         PIC X(10).
               05 DATA_DESLIGAMENTO     PIC X(10).
       01 WS-CODIGO                PIC 9(5).
       01 WS-NEW-CODIGO             PIC 9(5).
       01 WS-NEW-NOME               PIC X(30).
       01 WS-NEW-MATRICULA          PIC X(10).
       01 WS-NEW-CARGO              PIC X(20).
       01 WS-NEW-DEPARTAMENTO       PIC X(20).
       01 WS-NEW-SALARIO            PIC 9(9)V99.
       01 WS-NEW-DATA_ADMISSAO      PIC X(10).
       01 WS-NEW-DATA_DESLIGAMENTO  PIC X(10).
       01 END-OF-FILE-FLAG         PIC X VALUE 'N'.
           88 END-OF-FILE          VALUE 'Y'.
       01 WS-MENSAGEM-CONTINUAR    PIC X(30) 
       VALUE "Aperte enter para continuar...".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM TELA-PRINCIPAL
           PERFORM UNTIL WS-OPTION = 9
               DISPLAY "Escolha uma opcao: " WITH NO ADVANCING
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM CADASTRAR-FUNCIONARIO
                   WHEN 2
                       PERFORM CONSULTAR-FUNCIONARIO
                   WHEN 3
                       PERFORM ATUALIZAR-FUNCIONARIO
                   WHEN 4
                       PERFORM REMOVER-FUNCIONARIO
                   WHEN 9
                       PERFORM RETORNAR
                   WHEN OTHER
                       DISPLAY "Opcao invalida, tente novamente."
               END-EVALUATE
               PERFORM TELA-PRINCIPAL
           END-PERFORM
           STOP RUN.

       TELA-PRINCIPAL.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "             GESTAO FUNCIONARIOS                 "
           DISPLAY "================================================="
           DISPLAY "1 - Cadastrar Funcionario"
           DISPLAY "2 - Consultar Funcionario"
           DISPLAY "3 - Atualizar Funcionario"
           DISPLAY "4 - Remover Funcionario"
           DISPLAY "9 - Voltar ao menu principal"
           DISPLAY "=================================================".

       CADASTRAR-FUNCIONARIO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "            CADASTRAR FUNCIONARIO               "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario: " 
           WITH NO ADVANCING ACCEPT CODIGO
           DISPLAY "Digite o nome do funcionario: " WITH NO ADVANCING
           ACCEPT NOME
           DISPLAY "Digite a matricula do funcionario: " WITH NO ADVANCING
           ACCEPT MATRICULA
           DISPLAY "Digite o cargo do funcionario: " WITH NO ADVANCING
           ACCEPT CARGO
           DISPLAY "Digite o departamento: " WITH NO ADVANCING
           ACCEPT DEPARTAMENTO
           DISPLAY "Digite o salario: " WITH NO ADVANCING
           ACCEPT SALARIO
           DISPLAY "Digite a data de admissao: " WITH NO ADVANCING
           ACCEPT DATA_ADMISSAO
           DISPLAY "Digite a data de desligamento: " WITH NO ADVANCING
           ACCEPT DATA_DESLIGAMENTO

           MOVE SPACES TO FUNCIONARIO-DADOS.
           STRING CODIGO "," 
                  NOME "," 
                  MATRICULA "," 
                  CARGO "," 
                  DEPARTAMENTO "," 
                  SALARIO "," 
                  DATA_ADMISSAO "," 
                  DATA_DESLIGAMENTO
                  INTO FUNCIONARIO-DADOS.

           OPEN OUTPUT FUNCIONARIO-FILE.
           WRITE FUNCIONARIO-RECORD FROM FUNCIONARIO-DADOS.
           CLOSE FUNCIONARIO-FILE.

           DISPLAY "Funcionario cadastrado com sucesso!".

       CONSULTAR-FUNCIONARIO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "           CONSULTAR FUNCIONARIO                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario: " 
           WITH NO ADVANCING ACCEPT CODIGO

           MOVE SPACES TO FUNCIONARIO-DADOS.
           OPEN INPUT FUNCIONARIO-FILE.
           PERFORM UNTIL END-OF-FILE
               READ FUNCIONARIO-FILE INTO FUNCIONARIO-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-FUNCIONARIO-RECORD
                       IF CODIGO = WS-CODIGO
                           DISPLAY "Nome: " NOME
                           DISPLAY "Matricula: " MATRICULA
                           DISPLAY "Cargo: " CARGO
                           DISPLAY "Departamento: " DEPARTAMENTO
                           DISPLAY "Salario: " SALARIO
                           DISPLAY "Data de Admissao: " DATA_ADMISSAO
                           DISPLAY "Data de Desligamento: " DATA_DESLIGAMENTO
                           SET END-OF-FILE TO TRUE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE FUNCIONARIO-FILE.

           IF WS-CODIGO NOT = CODIGO
               DISPLAY "Funcionario nao encontrado."
           END-IF.

           DISPLAY WS-MENSAGEM-CONTINUAR
           ACCEPT WS-OPTION.

       PARSE-FUNCIONARIO-RECORD.
           UNSTRING FUNCIONARIO-DADOS
               DELIMITED BY ","
               INTO WS-CODIGO, NOME, MATRICULA, 
               CARGO, DEPARTAMENTO, SALARIO, 
               DATA_ADMISSAO, DATA_DESLIGAMENTO.

       ATUALIZAR-FUNCIONARIO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "          ATUALIZAR FUNCIONARIO                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario: " 
           WITH NO ADVANCING ACCEPT WS-CODIGO
           DISPLAY "Digite os novos dados do funcionario: "
           DISPLAY "Nome: " WITH NO ADVANCING ACCEPT WS-NEW-NOME
           DISPLAY "Matricula: " WITH NO ADVANCING ACCEPT WS-NEW-MATRICULA
           DISPLAY "Cargo: " WITH NO ADVANCING ACCEPT WS-NEW-CARGO
           DISPLAY "Departamento: " WITH NO ADVANCING ACCEPT WS-NEW-DEPARTAMENTO
           DISPLAY "Salario: " WITH NO ADVANCING ACCEPT WS-NEW-SALARIO
           DISPLAY "Data de Admissao: " WITH NO ADVANCING ACCEPT WS-NEW-DATA_ADMISSAO
           DISPLAY "Data de Desligamento: " WITH NO ADVANCING ACCEPT WS-NEW-DATA_DESLIGAMENTO

           MOVE SPACES TO FUNCIONARIO-DADOS.
           STRING WS-NEW-CODIGO "," 
                  WS-NEW-NOME "," 
                  WS-NEW-MATRICULA "," 
                  WS-NEW-CARGO "," 
                  WS-NEW-DEPARTAMENTO "," 
                  WS-NEW-SALARIO "," 
                  WS-NEW-DATA_ADMISSAO "," 
                  WS-NEW-DATA_DESLIGAMENTO
                  INTO FUNCIONARIO-DADOS.

           OPEN I-O FUNCIONARIO-FILE.
           PERFORM UNTIL END-OF-FILE
               READ FUNCIONARIO-FILE INTO FUNCIONARIO-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-FUNCIONARIO-RECORD
                       IF WS-CODIGO = CODIGO
                           REWRITE FUNCIONARIO-RECORD FROM FUNCIONARIO-DADOS
                           DISPLAY "Funcionario atualizado com sucesso!"
                           SET END-OF-FILE TO TRUE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE FUNCIONARIO-FILE.

       REMOVER-FUNCIONARIO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              REMOVER FUNCIONARIO               "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario: " 
           WITH NO ADVANCING ACCEPT WS-CODIGO

           OPEN I-O FUNCIONARIO-FILE.
           MOVE SPACES TO FUNCIONARIO-DADOS.
           PERFORM UNTIL END-OF-FILE
               READ FUNCIONARIO-FILE INTO FUNCIONARIO-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-FUNCIONARIO-RECORD
                       IF WS-CODIGO NOT = CODIGO
                           WRITE FUNCIONARIO-RECORD FROM FUNCIONARIO-DADOS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE FUNCIONARIO-FILE.

           DISPLAY "Funcionario removido com sucesso!".


       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.
           GOBACK.

