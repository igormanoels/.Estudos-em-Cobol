       IDENTIFICATION DIVISION.
       PROGRAM-ID. gestaoFornecedor.

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
       01 FORNECEDOR.
           05 CODIGO            PIC 9(5).
           05 NOME-RAZAO        PIC X(30).
           05 NOME-FANTASIA     PIC X(30).
           05 CNPJ              PIC X(20).
           05 TELEFONE          PIC 9(11).
           05 EMAIL             PIC X(30).
       01 WS-END-OF-FILE        PIC X VALUE "N".

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
           DISPLAY "              GESTAO DE FORNECEDOR               "
           DISPLAY "================================================="
           DISPLAY "1 - Cadastrar Fornecedor"
           DISPLAY "2 - Consultar Fornecedor"
           DISPLAY "3 - Atualizar Fornecedor"
           DISPLAY "4 - Deletar Fornecedor"
           DISPLAY "9 - Voltar ao menu principal"
           DISPLAY "=================================================".

       CADASTRAR-FORNECEDOR.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "             CADASTRAR FORNECEDOR                "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo Fornecedor: " WITH NO ADVANCING 
           ACCEPT CODIGO
           DISPLAY "Digite o nome razao: " WITH NO ADVANCING
           ACCEPT NOME-RAZAO
           DISPLAY "Digite o nome fantasia: " WITH NO ADVANCING
           ACCEPT NOME-FANTASIA
           DISPLAY "Digite o CNPJ: " WITH NO ADVANCING
           ACCEPT CNPJ
           DISPLAY "Digite o telefone: " WITH NO ADVANCING
           ACCEPT TELEFONE
           DISPLAY "Digite o email: " WITH NO ADVANCING
           ACCEPT EMAIL

           MOVE SPACES TO FORNECEDOR-DADOS.
           STRING CODIGO "," 
                  NOME-RAZAO "," 
                  NOME-FANTASIA "," 
                  CNPJ "," 
                  TELEFONE "," 
                  EMAIL
                  INTO FORNECEDOR-DADOS.

           OPEN OUTPUT FORNECEDOR-FILE.
           WRITE FORNECEDOR-RECORD FROM FORNECEDOR-DADOS.
           CLOSE FORNECEDOR-FILE.

           DISPLAY "Fornecedor cadastrado com sucesso!".

       CONSULTAR-FORNECEDOR.
           CALL 'clearScreen' .
           DISPLAY "================================================="
           DISPLAY "             CONSULTAR FORNECEDOR                "
           DISPLAY "================================================="
           OPEN INPUT FORNECEDOR-FILE
           PERFORM READ-FORNECEDOR UNTIL WS-END-OF-FILE = "Y"
           CLOSE FORNECEDOR-FILE
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.

       READ-FORNECEDOR.
           READ FORNECEDOR-FILE INTO FORNECEDOR-RECORD
               AT END
                   MOVE "Y" TO WS-END-OF-FILE
               NOT AT END
                   UNSTRING FORNECEDOR-DADOS DELIMITED BY ","
                       INTO CODIGO NOME-RAZAO NOME-FANTASIA 
                       CNPJ TELEFONE EMAIL
                   DISPLAY "Codigo: " CODIGO
                   DISPLAY "Nome/Razao: " NOME-RAZAO
                   DISPLAY "Nome Fantasia: " NOME-FANTASIA
                   DISPLAY "CNPJ: " CNPJ
                   DISPLAY "Telefone: " TELEFONE
                   DISPLAY "Email: " EMAIL
                   DISPLAY "------------------------------------------"
           END-READ.

       ATUALIZAR-FORNECEDOR.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "             ATUALIZAR FORNECEDOR                "
           DISPLAY "================================================="
           DISPLAY "Em desenvolvimento."
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.

       REMOVER-FORNECEDOR.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "               REMOVER FORNECEDOR                "
           DISPLAY "================================================="
           DISPLAY "Em desenvolvimento."
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.

       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.
           GOBACK.
