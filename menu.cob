       IDENTIFICATION DIVISION.
       PROGRAM-ID. menu.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPTION          PIC 9 VALUE 0.

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
                   WHEN 9
                       PERFORM EXIT-PROGRAM
                   WHEN OTHER
                       DISPLAY "Opcao invalida, tente novamente."
               END-EVALUATE
               PERFORM DISPLAY-MENU
           END-PERFORM
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "=============================="
           DISPLAY "       MENU PRINCIPAL         "
           DISPLAY "=============================="
           DISPLAY "1 - Gestao de Funcionario"
           DISPLAY "2 - Gestao de Fornecedor"
           DISPLAY "9 - Sair"
           DISPLAY "==============================".

       OPTION-1.
           CALL 'gestaoFuncionario'.

       OPTION-2.
           CALL 'gestaoFornecedor'.

       EXIT-PROGRAM.
           DISPLAY "Saindo do programa. Obrigado!".
           STOP RUN.
