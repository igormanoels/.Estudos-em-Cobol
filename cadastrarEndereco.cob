       IDENTIFICATION DIVISION.
       PROGRAM-ID. cadastrarEndereco.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENDERECO-FILE ASSIGN TO "dadosEndereco.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ENDERECO-FILE.
       01 ENDERECO-RECORD.
           05 ENDERECO-DADOS PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-OPTION             PIC 9(5) VALUE 0.
       01 WS-DADOS-FUNCIONARIO-FORNECEDOR.
               05 CODIGO                PIC 9(5).
               05 ENDERECO.
                   10 RUA               PIC X(30).
                   10 NUMERO            PIC 9(5).
                   10 CEP               PIC 9(8).
                   10 CIDADE            PIC X(20).
                   10 ESTADO            PIC XX.
                   10 COMPLEMENTO       PIC X(20).
       01 WS-CODIGO                PIC 9(5).
       01 WS-NEW-CODIGO             PIC 9(5).
       01 WS-NEW-RUA                PIC X(30).
       01 WS-NEW-NUMERO             PIC 9(5).
       01 WS-NEW-CEP                PIC 9(8).
       01 WS-NEW-CIDADE             PIC X(20).
       01 WS-NEW-ESTADO             PIC XX.
       01 WS-NEW-COMPLEMENTO        PIC X(20).
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
                       PERFORM CADASTRAR-ENDERECO
                   WHEN 2
                       PERFORM CONSULTAR-ENDERECO
                   WHEN 3
                       PERFORM ATUALIZAR-ENDERECO
                   WHEN 4
                       PERFORM REMOVER-ENDERECO
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
           DISPLAY "                GESTAO ENDERECOS                 "
           DISPLAY "================================================="
           DISPLAY "1 - Cadastrar Endereco"
           DISPLAY "2 - Consultar Endereco"
           DISPLAY "3 - Atualizar Endereco"
           DISPLAY "4 - Remover Endereco"
           DISPLAY "9 - Voltar ao menu principal"
           DISPLAY "=================================================".

       CADASTRAR-ENDERECO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              CADASTRAR ENDERECO                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario/Fornecedor: " 
           WITH NO ADVANCING ACCEPT CODIGO
           DISPLAY "Digite o nome da rua: " WITH NO ADVANCING
           ACCEPT RUA
           DISPLAY "Digite o numero: " WITH NO ADVANCING
           ACCEPT NUMERO
           DISPLAY "Digite o CEP: " WITH NO ADVANCING
           ACCEPT CEP
           DISPLAY "Digite a cidade: " WITH NO ADVANCING
           ACCEPT CIDADE
           DISPLAY "Digite o estado: " WITH NO ADVANCING
           ACCEPT ESTADO
           DISPLAY "Digite o complemento(se houver): " WITH NO ADVANCING
           ACCEPT COMPLEMENTO

           MOVE SPACES TO ENDERECO-DADOS.
           STRING CODIGO "," 
                  RUA "," 
                  NUMERO "," 
                  CEP "," 
                  CIDADE "," 
                  ESTADO "," 
                  COMPLEMENTO
                  INTO ENDERECO-DADOS.

           OPEN OUTPUT ENDERECO-FILE.
           WRITE ENDERECO-RECORD FROM ENDERECO-DADOS.
           CLOSE ENDERECO-FILE.

           DISPLAY "Endereco cadastrado com sucesso!".

       CONSULTAR-ENDERECO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              CONSULTAR ENDERECO                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario/Fornecedor: " 
           WITH NO ADVANCING
           ACCEPT CODIGO

           MOVE SPACES TO ENDERECO-DADOS.
           OPEN INPUT ENDERECO-FILE.
           PERFORM UNTIL END-OF-FILE
               READ ENDERECO-FILE INTO ENDERECO-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-ENDERECO-RECORD
                       IF CODIGO = WS-CODIGO
                           DISPLAY "Rua: " RUA
                           DISPLAY "Numero: " NUMERO
                           DISPLAY "CEP: " CEP
                           DISPLAY "Cidade: " CIDADE
                           DISPLAY "Estado: " ESTADO
                           DISPLAY "Complemento: " COMPLEMENTO
                           SET END-OF-FILE TO TRUE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ENDERECO-FILE.

           IF WS-CODIGO NOT = CODIGO
               DISPLAY "Endereco nao encontrado."
           END-IF.

           DISPLAY WS-MENSAGEM-CONTINUAR
           ACCEPT WS-OPTION.

       PARSE-ENDERECO-RECORD.
           UNSTRING ENDERECO-DADOS
               DELIMITED BY ","
               INTO WS-CODIGO, RUA, NUMERO, CEP, 
               CIDADE, ESTADO, COMPLEMENTO.

       ATUALIZAR-ENDERECO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              ATUALIZAR ENDERECO                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario/Fornecedor: " 
           WITH NO ADVANCING ACCEPT WS-CODIGO
           DISPLAY "Digite os novos dados do endereco: "
           DISPLAY "Rua: " WITH NO ADVANCING ACCEPT WS-NEW-RUA
           DISPLAY "Numero: " WITH NO ADVANCING ACCEPT WS-NEW-NUMERO
           DISPLAY "CEP: " WITH NO ADVANCING ACCEPT WS-NEW-CEP
           DISPLAY "Cidade: " WITH NO ADVANCING ACCEPT WS-NEW-CIDADE
           DISPLAY "Estado: " WITH NO ADVANCING ACCEPT WS-NEW-ESTADO
           DISPLAY "Complemento: " WITH NO ADVANCING ACCEPT WS-NEW-COMPLEMENTO

           MOVE SPACES TO ENDERECO-DADOS.
           STRING WS-NEW-CODIGO "," 
                  WS-NEW-RUA "," 
                  WS-NEW-NUMERO "," 
                  WS-NEW-CEP "," 
                  WS-NEW-CIDADE "," 
                  WS-NEW-ESTADO "," 
                  WS-NEW-COMPLEMENTO
                  INTO ENDERECO-DADOS.

           OPEN I-O ENDERECO-FILE.
           PERFORM UNTIL END-OF-FILE
               READ ENDERECO-FILE INTO ENDERECO-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-ENDERECO-RECORD
                       IF WS-CODIGO = CODIGO
                           REWRITE ENDERECO-RECORD FROM ENDERECO-DADOS
                           DISPLAY "Endereco atualizado com sucesso!"
                           SET END-OF-FILE TO TRUE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ENDERECO-FILE.

       REMOVER-ENDERECO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "                REMOVER ENDERECO                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Funcionario/Fornecedor: " 
           WITH NO ADVANCING ACCEPT WS-CODIGO

           OPEN I-O ENDERECO-FILE.
           MOVE SPACES TO ENDERECO-DADOS.
           PERFORM UNTIL END-OF-FILE
               READ ENDERECO-FILE INTO ENDERECO-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-ENDERECO-RECORD
                       IF WS-CODIGO = CODIGO
                           DISPLAY "Endereco removido com sucesso!"
                       ELSE
                           WRITE ENDERECO-RECORD FROM ENDERECO-DADOS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ENDERECO-FILE.

       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           DISPLAY WS-MENSAGEM-CONTINUAR
           ACCEPT WS-OPTION.
           GOBACK.
