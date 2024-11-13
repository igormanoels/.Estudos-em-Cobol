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
           01 WS-END-OF-FILE        PIC X VALUE "N".
           01 WS-CODIGO-PRODUTO     PIC 9(5).
           01 WS-QUANTIDADE         PIC 9(2).
           01 WS-FILE-UPDATED       PIC X VALUE "N".

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
           STRING CODIGO DELIMITED BY SIZE "," 
                  NOME DELIMITED BY SIZE "," 
                  QUANTIDADE DELIMITED BY SIZE "," 
                  PRECO-UNIDADE DELIMITED BY SIZE "," 
                  COD-FORNECEDOR DELIMITED BY SIZE "," 
                  VALOR-TOTAL DELIMITED BY SIZE
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
           OPEN INPUT ESTOQUE-FILE
           PERFORM READ-ESTOQUE UNTIL WS-END-OF-FILE = "Y"
           CLOSE ESTOQUE-FILE
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.

       READ-ESTOQUE.
           READ ESTOQUE-FILE INTO ESTOQUE-RECORD
               AT END
                   MOVE "Y" TO WS-END-OF-FILE
               NOT AT END
                   UNSTRING ESTOQUE-DADOS DELIMITED BY ","
                       INTO CODIGO NOME QUANTIDADE PRECO-UNIDADE 
                       COD-FORNECEDOR VALOR-TOTAL
                   DISPLAY "Codigo: " CODIGO
                   DISPLAY "Nome: " NOME
                   DISPLAY "Quantidade: " QUANTIDADE
                   DISPLAY "Preco Unitario: " PRECO-UNIDADE
                   DISPLAY "Codigo Fornecedor: " COD-FORNECEDOR
                   DISPLAY "Valor Total: " VALOR-TOTAL
                   DISPLAY "------------------------------------------"
           END-READ.

       ADICIONAR-PRODUTO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "               ADICIONAR PRODUTO                 "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Produto: " WITH NO ADVANCING
           ACCEPT WS-CODIGO-PRODUTO
           DISPLAY "Digite a quantidade a adicionar: " WITH NO ADVANCING
           ACCEPT WS-QUANTIDADE

           OPEN I-O ESTOQUE-FILE
           PERFORM ATUALIZAR-QUANTIDADE
           IF WS-FILE-UPDATED = "N"
               DISPLAY "Produto nao encontrado!"
           ELSE
               DISPLAY "Quantidade adicionada com sucesso!"
           END-IF
           CLOSE ESTOQUE-FILE
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.

       ATUALIZAR-QUANTIDADE.
           MOVE "N" TO WS-FILE-UPDATED
           REWRITE-ESTOQUE.
           OPEN INPUT ESTOQUE-FILE
           PERFORM READ-ESTOQUE UNTIL WS-END-OF-FILE = "Y"
               UNSTRING ESTOQUE-DADOS DELIMITED BY ","
                   INTO CODIGO NOME QUANTIDADE PRECO-UNIDADE 
                   COD-FORNECEDOR VALOR-TOTAL
               IF CODIGO = WS-CODIGO-PRODUTO
                   ADD WS-QUANTIDADE TO QUANTIDADE
                   MOVE SPACES TO ESTOQUE-DADOS
                   STRING CODIGO DELIMITED BY SIZE "," 
                          NOME DELIMITED BY SIZE "," 
                          QUANTIDADE DELIMITED BY SIZE "," 
                          PRECO-UNIDADE DELIMITED BY SIZE "," 
                          COD-FORNECEDOR DELIMITED BY SIZE "," 
                          VALOR-TOTAL DELIMITED BY SIZE
                          INTO ESTOQUE-DADOS
                   REWRITE ESTOQUE-RECORD
                   MOVE "Y" TO WS-FILE-UPDATED
               END-IF
           END-PERFORM
           CLOSE ESTOQUE-FILE.

       REMOVER-PRODUTO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              REMOVER PRODUTO                    "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do Produto: " WITH NO ADVANCING
           ACCEPT WS-CODIGO-PRODUTO
           DISPLAY "Digite a quantidade a remover: " WITH NO ADVANCING
           ACCEPT WS-QUANTIDADE

           OPEN I-O ESTOQUE-FILE
           PERFORM DECREMENTAR-QUANTIDADE
           IF WS-FILE-UPDATED = "N"
               DISPLAY "Produto nao encontrado!"
           ELSE
               DISPLAY "Quantidade removida com sucesso!"
           END-IF
           CLOSE ESTOQUE-FILE
           DISPLAY "Pressione Enter para continuar..."
           ACCEPT WS-OPTION.

       DECREMENTAR-QUANTIDADE.
           MOVE "N" TO WS-FILE-UPDATED
           OPEN INPUT ESTOQUE-FILE
           PERFORM READ-ESTOQUE UNTIL WS-END-OF-FILE = "Y"
               UNSTRING ESTOQUE-DADOS DELIMITED BY ","
                   INTO CODIGO NOME QUANTIDADE PRECO-UNIDADE 
                   COD-FORNECEDOR VALOR-TOTAL
               IF CODIGO = WS-CODIGO-PRODUTO
                   SUBTRACT WS-QUANTIDADE FROM QUANTIDADE
                   IF QUANTIDADE < 0
                       DISPLAY "Quantidade insuficiente!"
                   ELSE
                       MOVE SPACES TO ESTOQUE-DADOS
                       STRING CODIGO DELIMITED BY SIZE "," 
                              NOME DELIMITED BY SIZE "," 
                              QUANTIDADE DELIMITED BY SIZE "," 
                              PRECO-UNIDADE DELIMITED BY SIZE "," 
                              COD-FORNECEDOR DELIMITED BY SIZE "," 
                              VALOR-TOTAL DELIMITED BY SIZE
                              INTO ESTOQUE-DADOS
                       REWRITE ESTOQUE-RECORD
                       MOVE "Y" TO WS-FILE-UPDATED
                   END-IF
               END-IF
           END-PERFORM
           CLOSE ESTOQUE-FILE.

       RETORNAR.
           DISPLAY "Retornando ao menu principal..."
           MOVE 9 TO WS-OPTION.

