       IDENTIFICATION DIVISION.
       PROGRAM-ID. cadastrarFornecedor.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FORNECEDOR-FILE ASSIGN TO "dadosFornecedor.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FORNECEDOR-FILE.
       01 FORNECEDOR-RECORD.
           05 FORNECEDOR-DADOS PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-OPTION             PIC 9(5) VALUE 0.
       01 WS-DADOS-FUNCIONARIO-FORNECEDOR.
               05 CODIGO                PIC 9(5).
               05 NOME                  PIC X(30).
               05 CNPJ                  PIC X(14).
               05 ENDERECO.
                   10 RUA               PIC X(30).
                   10 NUMERO            PIC 9(5).
                   10 CEP               PIC 9(8).
                   10 CIDADE            PIC X(20).
                   10 ESTADO            PIC XX.
                   10 COMPLEMENTO       PIC X(20).
       01 WS-CODIGO                PIC 9(5).
       01 WS-NEW-CODIGO             PIC 9(5).
       01 WS-NEW-NOME               PIC X(30).
       01 WS-NEW-CNPJ               PIC X(14).
       01 WS-NEW-ENDERECO           PIC X(100).
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
                       PERFORM CADASTRAR-FORNECEDOR
                   WHEN 2
                       PERFORM CONSULTAR-FORNECEDOR
                   WHEN 3
                       PERFORM ATUALIZAR-FORNECEDOR
                   WHEN 4
                       PERFORM REMOVER-FORNECEDOR
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
           DISPLAY "             GESTAO FORNECEDORES                 "
           DISPLAY "================================================="
           DISPLAY "1 - Cadastrar Fornecedor"
           DISPLAY "2 - Consultar Fornecedor"
           DISPLAY "3 - Atualizar Fornecedor"
           DISPLAY "4 - Remover Fornecedor"
           DISPLAY "9 - Voltar ao menu principal"
           DISPLAY "=================================================".

       CADASTRAR-FORNECEDOR.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "            CADASTRAR FORNECEDOR                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Fornecedor: " 
           WITH NO ADVANCING ACCEPT CODIGO
           DISPLAY "Digite o nome do fornecedor: " WITH NO ADVANCING
           ACCEPT NOME
           DISPLAY "Digite o CNPJ do fornecedor: " WITH NO ADVANCING
           ACCEPT CNPJ
           DISPLAY "Digite o endereco do fornecedor: "
           DISPLAY "Rua: " WITH NO ADVANCING ACCEPT RUA
           DISPLAY "Numero: " WITH NO ADVANCING ACCEPT NUMERO
           DISPLAY "CEP: " WITH NO ADVANCING ACCEPT CEP
           DISPLAY "Cidade: " WITH NO ADVANCING ACCEPT CIDADE
           DISPLAY "Estado: " WITH NO ADVANCING ACCEPT ESTADO
           DISPLAY "Complemento: " WITH NO ADVANCING ACCEPT COMPLEMENTO

           MOVE SPACES TO FORNECEDOR-DADOS.
           STRING CODIGO "," 
                  NOME "," 
                  CNPJ "," 
                  RUA "," 
                  NUMERO "," 
                  CEP "," 
                  CIDADE "," 
                  ESTADO "," 
                  COMPLEMENTO
                  INTO FORNECEDOR-DADOS.

           OPEN OUTPUT FORNECEDOR-FILE.
           WRITE FORNECEDOR-RECORD FROM FORNECEDOR-DADOS.
           CLOSE FORNECEDOR-FILE.

           DISPLAY "Fornecedor cadastrado com sucesso!".

       CONSULTAR-FORNECEDOR.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "           CONSULTAR FORNECEDOR                  "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Fornecedor: " 
           WITH NO ADVANCING ACCEPT CODIGO

           MOVE SPACES TO FORNECEDOR-DADOS.
           OPEN INPUT FORNECEDOR-FILE.
           PERFORM UNTIL END-OF-FILE
               READ FORNECEDOR-FILE INTO FORNECEDOR-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-FORNECEDOR-RECORD
                       IF CODIGO = WS-CODIGO
                           DISPLAY "Nome: " NOME
                           DISPLAY "CNPJ: " CNPJ
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
           CLOSE FORNECEDOR-FILE.

           IF WS-CODIGO NOT = CODIGO
               DISPLAY "Fornecedor nao encontrado."
           END-IF.

           DISPLAY WS-MENSAGEM-CONTINUAR
           ACCEPT WS-OPTION.

       PARSE-FORNECEDOR-RECORD.
           UNSTRING FORNECEDOR-DADOS
               DELIMITED BY ","
               INTO WS-CODIGO, NOME, CNPJ, 
               RUA, NUMERO, CEP, CIDADE, 
               ESTADO, COMPLEMENTO.

       ATUALIZAR-FORNECEDOR.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "          ATUALIZAR FORNECEDOR                  "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Fornecedor: " 
           WITH NO ADVANCING ACCEPT WS-CODIGO
           DISPLAY "Digite os novos dados do fornecedor: "
           DISPLAY "Nome: " WITH NO ADVANCING ACCEPT WS-NEW-NOME
           DISPLAY "CNPJ: " WITH NO ADVANCING ACCEPT WS-NEW-CNPJ
           DISPLAY "Rua: " WITH NO ADVANCING ACCEPT WS-NEW-RUA
           DISPLAY "Numero: " WITH NO ADVANCING ACCEPT WS-NEW-NUMERO
           DISPLAY "CEP: " WITH NO ADVANCING ACCEPT WS-NEW-CEP
           DISPLAY "Cidade: " WITH NO ADVANCING ACCEPT WS-NEW-CIDADE
           DISPLAY "Estado: " WITH NO ADVANCING ACCEPT WS-NEW-ESTADO
           DISPLAY "Complemento: " WITH NO ADVANCING ACCEPT WS-NEW-COMPLEMENTO

           MOVE SPACES TO FORNECEDOR-DADOS.
           STRING WS-NEW-CODIGO "," 
                  WS-NEW-NOME "," 
                  WS-NEW-CNPJ "," 
                  WS-NEW-RUA "," 
                  WS-NEW-NUMERO "," 
                  WS-NEW-CEP "," 
                  WS-NEW-CIDADE "," 
                  WS-NEW-ESTADO "," 
                  WS-NEW-COMPLEMENTO
                  INTO FORNECEDOR-DADOS.

           OPEN I-O FORNECEDOR-FILE.
           PERFORM UNTIL END-OF-FILE
               READ FORNECEDOR-FILE INTO FORNECEDOR-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-FORNECEDOR-RECORD
                       IF WS-CODIGO = CODIGO
                           REWRITE FORNECEDOR-RECORD FROM FORNECEDOR-DADOS
                           DISPLAY "Fornecedor atualizado com sucesso!"
                           SET END-OF-FILE TO TRUE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE FORNECEDOR-FILE.

       REMOVER-FORNECEDOR.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              REMOVER FORNECEDOR                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Fornecedor: " 
           WITH NO ADVANCING ACCEPT WS-CODIGO

           OPEN I-O FORNECEDOR-FILE.
           MOVE SPACES TO FORNECEDOR-DADOS.
           PERFORM UNTIL END-OF-FILE
               READ FORNECEDOR-FILE INTO FORNECEDOR-DADOS
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PARSE-FORNECEDOR-RECORD
                       IF WS-CODIGO = CODIGO
                           CONTINUE
                       END-IF
                       WRITE FORNECEDOR-RECORD FROM FORNECEDOR-DADOS
               END-READ
           END-PERFORM
           CLOSE FORNECEDOR-FILE.

           DISPLAY "Fornecedor removido com sucesso!".
           
       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.
           GOBACK.
