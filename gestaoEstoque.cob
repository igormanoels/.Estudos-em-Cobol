       IDENTIFICATION DIVISION.
       PROGRAM-ID. gestaoEstoque.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ESTOQUE-FILE ASSIGN TO "dadosEstoque.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ESTOQUE-FILE.
       01 ESTOQUE-RECORD.
           05 ESTOQUE-DADOS PIC X(100).

       WORKING-STORAGE SECTION.
           01 WS-OPTION             PIC 9 VALUE 0.
           01 PRODUTO.
               05 CODIGO            PIC 9(5).
               05 NOME              PIC X(30).
               05 QUANTIDADE        PIC 9(2).
               05 PRECO-UNIDADE     PIC 9(7)V99.
               05 COD-FORNECEDOR    PIC 9(6).
               05 VALOR-TOTAL       PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM TELA-PRINCIPAL
           PERFORM UNTIL WS-OPTION = 9
               DISPLAY "Escolha uma opcao: " WITH NO ADVANCING
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM CADASTRAR-PRODUTO
                   WHEN 2
                       PERFORM CONSULTAR-PRODUTO
                   WHEN 3
                       PERFORM ADICIONAR-PRODUTO
                   WHEN 4
                       PERFORM REMOVER-PRODUTO
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
           DISPLAY "                GESTAO DE ESTOQUE                "
           DISPLAY "================================================="
           DISPLAY "1 - Cadastrar Produto"
           DISPLAY "2 - Consultar Produto"
           DISPLAY "3 - Adicionar Produto"
           DISPLAY "4 - Remover Produto"
           DISPLAY "9 - Voltar ao menu principal"
           DISPLAY "=================================================".

       CADASTRAR-PRODUTO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              CADASTRAR PRODUTO                  "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Produto: " WITH NO ADVANCING
           ACCEPT CODIGO
           DISPLAY "Digite o nome do Produto: " WITH NO ADVANCING
           ACCEPT NOME
           DISPLAY "Digite a Quantidade: " WITH NO ADVANCING
           ACCEPT QUANTIDADE
           DISPLAY "Digite o preco unitario: " WITH NO ADVANCING
           ACCEPT PRECO-UNIDADE
           DISPLAY "Digite o codigo do fornecedor: " WITH NO ADVANCING
           ACCEPT COD-FORNECEDOR

           PERFORM CALCULAR-VALOR-TOTAL

           MOVE SPACES TO ESTOQUE-DADOS.
           STRING CODIGO "," 
                  NOME "," 
                  QUANTIDADE "," 
                  PRECO-UNIDADE "," 
                  COD-FORNECEDOR "," 
                  VALOR-TOTAL
                  INTO ESTOQUE-DADOS.

           OPEN OUTPUT ESTOQUE-FILE.
           WRITE ESTOQUE-RECORD FROM ESTOQUE-DADOS.
           CLOSE ESTOQUE-FILE.

           DISPLAY "Produto cadastrado com sucesso!".

       CALCULAR-VALOR-TOTAL.
           COMPUTE VALOR-TOTAL = PRECO-UNIDADE * QUANTIDADE.

       CONSULTAR-PRODUTO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              CONSULTAR PRODUTO                  "
           DISPLAY "================================================="
           DISPLAY "Em desenvolvimento.".

       ADICIONAR-PRODUTO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "               ADCIONAR PRODUTO                  "
           DISPLAY "================================================="
           DISPLAY "Em desenvolvimento.".

       REMOVER-PRODUTO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              REMOVER PRODUTO                  "
           DISPLAY "================================================="
           DISPLAY "Em desenvolvimento.".

       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           GOBACK.
