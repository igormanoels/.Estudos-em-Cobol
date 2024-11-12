       IDENTIFICATION DIVISION.
       PROGRAM-ID. menu.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPTION          PIC 9 VALUE 0.
       01 WS-SAIR-OPCAO      PIC X(1).
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM DISPLAY-MENU
           PERFORM UNTIL WS-OPTION = 9
               DISPLAY "Escolha uma opcao: " WITH NO ADVANCING
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM OPTION-1
                   WHEN 2
                       PERFORM OPTION-2
                   WHEN 3
                       PERFORM OPTION-3
                   WHEN 4
                       PERFORM OPTION-4
                   WHEN 9
                       PERFORM EXIT-PROGRAM
                   WHEN OTHER
                       DISPLAY "Opcao invalida, tente novamente."
               END-EVALUATE
               PERFORM DISPLAY-MENU
           END-PERFORM
           STOP RUN.
       
       DISPLAY-MENU.
           CALL "clearScreen"
           DISPLAY "================================================="
           DISPLAY "                 MENU PRINCIPAL                  "
           DISPLAY "================================================="
           DISPLAY "1 - Gestao de Funcionario"
           DISPLAY "2 - Gestao de Fornecedor"
           DISPLAY "3 - Cadastro de Endereco"
           DISPLAY "4 - Gestao de Estoque"
           DISPLAY "9 - Sair"
           DISPLAY "=================================================".
       
       OPTION-1.
           CALL 'clearScreen'.
           CALL 'gestaoFuncionario'.
       
       OPTION-2.
           CALL 'clearScreen'.
           CALL 'gestaoFornecedor'.
       
       OPTION-3.
           CALL 'clearScreen'.
           CALL 'cadastrarEndereco'.
       OPTION-4.
           CALL 'clearScreen'.
           CALL 'gestaoEstoque'.
       
       EXIT-PROGRAM.
           DISPLAY "Deseja sair mesmo? (S/N): " WITH NO ADVANCING
           ACCEPT WS-SAIR-OPCAO
           IF WS-SAIR-OPCAO = "S" OR WS-SAIR-OPCAO = "s"
               DISPLAY "Saindo do programa. Obrigado!"
               DISPLAY "Pressione Enter para fechar o programa..."
               ACCEPT WS-SAIR-OPCAO
               STOP RUN
           ELSE
               IF WS-SAIR-OPCAO = "N" OR WS-SAIR-OPCAO = "n"
                   DISPLAY "Voltando ao menu principal."
               ELSE
                   DISPLAY "Escolha inválida."
               END-IF
           END-IF
           MOVE SPACES TO WS-SAIR-OPCAO.
           MOVE ZEROES TO WS-OPTION.

