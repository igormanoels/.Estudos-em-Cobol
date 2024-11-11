       IDENTIFICATION DIVISION.
       PROGRAM-ID. gestaoFuncionario.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPTION             PIC 9 VALUE 0.
       01 FUNCIONARIO.
           05 CODIGO            PIC 9(5).
           05 NOME              PIC X(30).
           05 TIPO-SALARIO      PIC X(1).
           05 SALARIO-BASE      PIC 9(6)V99.
           05 NUM-FILHOS        PIC 9(2).
           05 DEPARTAMENTO      PIC 9.
           05 FUNCAO            PIC X(1).
           05 SALARIO-BRUTO     PIC 9(7)V99.
           05 INSS              PIC 9(6)V99.
           05 IMPOSTO-RENDA     PIC 9(6)V99.
           05 SALARIO-FAMILIA   PIC 9(6)V99.
           05 SALARIO-LIQUIDO   PIC 9(7)V99.

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
                       PERFORM EXCLUIR-FUNCIONARIO
                   WHEN 9
                       PERFORM RETORNAR
                   WHEN OTHER
                       DISPLAY "Opcao invalida, tente novamente."
               END-EVALUATE
               PERFORM TELA-PRINCIPAL
           END-PERFORM
           STOP RUN.

       TELA-PRINCIPAL.
           DISPLAY "====================================="
           DISPLAY "       GESTAO DE FUNCIONARIO         "
           DISPLAY "====================================="
           DISPLAY "1 - Cadastrar Funcionario"
           DISPLAY "2 - Consultar Funcionario"
           DISPLAY "3 - Atualizar Funcionario"
           DISPLAY "4 - Deletar Funcionario"
           DISPLAY "9 - Voltar ao menu principal"
           DISPLAY "=====================================".

       CADASTRAR-FUNCIONARIO.
           DISPLAY "Digite o codigo do funcionario: " WITH NO ADVANCING
           ACCEPT CODIGO
           DISPLAY "Digite o nome do funcionario: " WITH NO ADVANCING
           ACCEPT NOME
           DISPLAY "Tipo de Salario (H - Horista, D - Diarista, "
           DISPLAY "M - Mensalista): " WITH NO ADVANCING
           ACCEPT TIPO-SALARIO
           DISPLAY "Digite o salario base: " WITH NO ADVANCING
           ACCEPT SALARIO-BASE
           DISPLAY "Digite o numero de filhos: " WITH NO ADVANCING
           ACCEPT NUM-FILHOS
           DISPLAY "Digite o departamento (1-10): " WITH NO ADVANCING
           ACCEPT DEPARTAMENTO
           DISPLAY "Digite a funcao (A - V): " WITH NO ADVANCING
           ACCEPT FUNCAO

           PERFORM CALCULAR-SALARIO-BRUTO
           PERFORM CALCULAR-INSS
           PERFORM CALCULAR-IMPOSTO-RENDA
           PERFORM CALCULAR-SALARIO-FAMILIA
           PERFORM CALCULAR-SALARIO-LIQUIDO

           DISPLAY "Funcionario cadastrado com sucesso!".

       CALCULAR-SALARIO-BRUTO.
           EVALUATE TIPO-SALARIO
               WHEN 'H' COMPUTE SALARIO-BRUTO = SALARIO-BASE * 220
               WHEN 'D' COMPUTE SALARIO-BRUTO = SALARIO-BASE * 30
               WHEN 'M' COMPUTE SALARIO-BRUTO = SALARIO-BASE
               WHEN OTHER
                   DISPLAY "Tipo de salario invalido." 
           END-EVALUATE.

       CALCULAR-INSS.
           IF SALARIO-BRUTO <= 2500
               COMPUTE INSS = SALARIO-BRUTO * 0.08
           ELSE IF SALARIO-BRUTO <= 6300
               COMPUTE INSS = SALARIO-BRUTO * 0.09
           ELSE
               COMPUTE INSS = SALARIO-BRUTO * 0.10
           END-IF.

       CALCULAR-IMPOSTO-RENDA.
           IF SALARIO-BRUTO <= 5000
               MOVE 0 TO IMPOSTO-RENDA
           ELSE IF SALARIO-BRUTO <= 12000
               COMPUTE IMPOSTO-RENDA = (SALARIO-BRUTO - INSS) * 0.05
           ELSE
               COMPUTE IMPOSTO-RENDA = (SALARIO-BRUTO - INSS) * 0.10
           END-IF.

       CALCULAR-SALARIO-FAMILIA.
           COMPUTE SALARIO-FAMILIA = NUM-FILHOS * 20.00.

       CALCULAR-SALARIO-LIQUIDO.
           COMPUTE SALARIO-LIQUIDO = SALARIO-BRUTO + SALARIO-FAMILIA.
           COMPUTE SALARIO-LIQUIDO = SALARIO-LIQUIDO - INSS.
           COMPUTE SALARIO-LIQUIDO = SALARIO-LIQUIDO - IMPOSTO-RENDA.

       CONSULTAR-FUNCIONARIO.
           DISPLAY "Consulta de funcionario em desenvolvimento.".

       ATUALIZAR-FUNCIONARIO.
           DISPLAY "Atualizacao de funcionario em desenvolvimento.".

       EXCLUIR-FUNCIONARIO.
           DISPLAY "Exclusao de funcionario em desenvolvimento.".

       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           GOBACK.
