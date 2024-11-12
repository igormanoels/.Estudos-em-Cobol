       IDENTIFICATION DIVISION.
       PROGRAM-ID. gestaoFuncionario.

       ENVIRONMENT DIVISION.
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
       01 FUNCIONARIO-ENCONTRADO    PIC X(3) VALUE "NAO".

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
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              GESTAO DE FUNCIONARIO              "
           DISPLAY "================================================="
           DISPLAY "1 - Cadastrar Funcionario"
           DISPLAY "2 - Consultar Funcionario"
           DISPLAY "3 - Atualizar Funcionario"
           DISPLAY "4 - Deletar Funcionario"
           DISPLAY "9 - Voltar ao menu principal"
           DISPLAY "=================================================".

       CADASTRAR-FUNCIONARIO.
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              CADASTRAR FUNCIONARIO              "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do funcionario: " WITH NO ADVANCING
           ACCEPT CODIGO
           DISPLAY "Digite o nome do funcionario: " WITH NO ADVANCING
           ACCEPT NOME
           DISPLAY "Tipo de Salario "
           DISPLAY "(H-Hora, D-Diario, M-Mensal): " WITH NO ADVANCING
           ACCEPT TIPO-SALARIO
           DISPLAY "Digite o salario base: " WITH NO ADVANCING
           ACCEPT SALARIO-BASE
           DISPLAY "Digite o numero de filhos: " WITH NO ADVANCING
           ACCEPT NUM-FILHOS
           DISPLAY "Digite nome do departamento: " WITH NO ADVANCING
           ACCEPT DEPARTAMENTO
           DISPLAY "Digite a funcao: " WITH NO ADVANCING
           ACCEPT FUNCAO

           PERFORM CALCULAR-SALARIO-BRUTO
           PERFORM CALCULAR-INSS
           PERFORM CALCULAR-IMPOSTO-RENDA
           PERFORM CALCULAR-SALARIO-FAMILIA
           PERFORM CALCULAR-SALARIO-LIQUIDO

           MOVE SPACES TO FUNCIONARIO-DADOS.
           STRING CODIGO DELIMITED BY SPACE "," 
                  NOME DELIMITED BY SPACE "," 
                  TIPO-SALARIO DELIMITED BY SPACE "," 
                  SALARIO-BASE DELIMITED BY SPACE "," 
                  NUM-FILHOS DELIMITED BY SPACE "," 
                  DEPARTAMENTO DELIMITED BY SPACE "," 
                  FUNCAO DELIMITED BY SPACE "," 
                  SALARIO-BRUTO DELIMITED BY SPACE "," 
                  INSS DELIMITED BY SPACE "," 
                  IMPOSTO-RENDA DELIMITED BY SPACE "," 
                  SALARIO-FAMILIA DELIMITED BY SPACE "," 
                  SALARIO-LIQUIDO DELIMITED BY SPACE 
                  INTO FUNCIONARIO-DADOS.

           OPEN OUTPUT FUNCIONARIO-FILE.
           WRITE FUNCIONARIO-RECORD FROM FUNCIONARIO-DADOS.
           CLOSE FUNCIONARIO-FILE.

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
           CALL 'clearScreen'.
           DISPLAY "================================================="
           DISPLAY "              CONSULTAR FUNCIONARIO              "
           DISPLAY "================================================="
           DISPLAY "Digite o codigo do funcionario: " WITH NO ADVANCING
           ACCEPT WS-OPTION

           OPEN INPUT FUNCIONARIO-FILE
           PERFORM BUSCAR-FUNCIONARIO
           CLOSE FUNCIONARIO-FILE
           IF FUNCIONARIO-ENCONTRADO = "SIM"
               DISPLAY "Funcionario encontrado:"
               DISPLAY "Codigo: " CODIGO
               DISPLAY "Nome: " NOME
               DISPLAY "Tipo de Salario: " TIPO-SALARIO
               DISPLAY "Salario Base: " SALARIO-BASE
               DISPLAY "Numero de Filhos: " NUM-FILHOS
               DISPLAY "Departamento: " DEPARTAMENTO
               DISPLAY "Funcao: " FUNCAO
               DISPLAY "Salario Bruto: " SALARIO-BRUTO
               DISPLAY "INSS: " INSS
               DISPLAY "Imposto de Renda: " IMPOSTO-RENDA
               DISPLAY "Salario Familia: " SALARIO-FAMILIA
               DISPLAY "Salario Liquido: " SALARIO-LIQUIDO
           ELSE
               DISPLAY "Funcionario nao encontrado."
           END-IF.
           DISPLAY "Pressione ENTER para continuar."
           ACCEPT WS-OPTION.

       BUSCAR-FUNCIONARIO.
           MOVE "NAO" TO FUNCIONARIO-ENCONTRADO
           READ FUNCIONARIO-FILE INTO FUNCIONARIO-RECORD
               AT END
                   DISPLAY "Arquivo de funcionarios chegou ao fim."
               NOT AT END
                   UNSTRING FUNCIONARIO-RECORD DELIMITED BY "," 
                       INTO CODIGO NOME TIPO-SALARIO SALARIO-BASE 
                           NUM-FILHOS DEPARTAMENTO FUNCAO SALARIO-BRUTO 
                           INSS IMPOSTO-RENDA SALARIO-FAMILIA 
                           SALARIO-LIQUIDO
                   IF CODIGO = WS-OPTION
                       MOVE "SIM" TO FUNCIONARIO-ENCONTRADO
                   END-IF
           END-READ.
       
       ATUALIZAR-FUNCIONARIO.
           DISPLAY "Exclusao de funcionario em desenvolvimento.".


       EXCLUIR-FUNCIONARIO.
           DISPLAY "Exclusao de funcionario em desenvolvimento.".

       CLEAR-SYSTEM.
           CALL 'clearScreen'.

       RETORNAR.
           DISPLAY "Voltando ao menu principal."
           GOBACK.

