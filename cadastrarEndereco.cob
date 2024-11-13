       IDENTIFICATION DIVISION.
       PROGRAM-ID. cadastrarEndereco.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

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
           DISPLAY "====================================="
           DISPLAY "         CADASTRAR ENDERECO          "
           DISPLAY "====================================="
           DISPLAY "1 - Cadastrar Produto"
           DISPLAY "2 - Consultar Produto"
           DISPLAY "3 - Adcionar Produto"
           DISPLAY "4 - Remover Produto"
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

           DISPLAY "Produto cadastrado com sucesso!".
         

       CONSULTAR-ENDERECO.
           DISPLAY "Em desenvolvimento.".

       ATUALIZAR-ENDERECO.
           DISPLAY "Em desenvolvimento.".
       
       REMOVER-ENDERECO.
           DISPLAY "Em desenvolvimento.".

       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           GOBACK.

