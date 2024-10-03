       IDENTIFICATION DIVISION. *> Contém informações sobre o programa.
       PROGRAM-ID. CADFOLHA.
      ****************************************************************
      * AUTHOR. @igormanoels.
      * INSTALLATION. FATEC ZONA LESTE.
      * DATA-WRITTEN. 08/09/2024.
      * DATA-COMPILED. 08/09/2024.
      ****************************************************************
      * 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
      * 
      ****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION. *> Seção onde se define variáveis      
       01 CODIGO PIC 9(6).
       01 NOME PIC X(50).
       01 NUMERO-FILHOS PIC 9(2).
       01 DEPARTAMENTO PIC 9(2).
       01 FUNCAO PIC X(1).
       01 TIPO-SALARIO PIC X(1).
       01 SALARIO-BASE PIC 9(6)V99.
       01 SALARIO-BRUTO PIC 9(6)V99.
       01 INSS PIC 9(6)V99.
       01 IMPOSTO-RENDA PIC 9(6)V99.
       01 SALARIO-FAMILIA PIC 9(6)V99.
       01 SALARIO-LIQUIDO PIC 9(6)V99.

       PROCEDURE DIVISION.
           PERFORM INICIO.
           PERFORM ENTRADA.
           PERFORM CALCULAR.
           PERFORM DADOS.
           STOP RUN.

       INICIO.
           DISPLAY (02, 25) 
           "______________CADASTRO DE FUNCIONARIOS______________". 

           DISPLAY (06, 10) 
           "Digite o codigo do funcionario ou zero para encerrar: ".           
           DISPLAY (08, 10) 
           "Digite o nome do funcionario: ".           
           DISPLAY (10, 10) 
           "Informe a quantidade de filhos do funcionario: ".
           DISPLAY (12, 10) 
           "Informe o departamento do funcionario: ".           
           DISPLAY (14, 10) 
           "Informe a funcao do funcionario: ".
           DISPLAY (16, 10) 
           "Informe o tipo de salario do funcionario: ".
           DISPLAY (18, 10) 
           "Informe o salario base do funcionario: ".

       ENTRADA.
      *> Recebe os valores dos dados
           ACCEPT (06, 65) CODIGO.
           IF CODIGO = "" OR SPACES
               GO TO ENTRADA.
           IF CODIGO = 0
               DISPLAY (20, 10) "PROGRAMA ENCERRADO."
               STOP RUN.
           ACCEPT (08, 41) NOME.
           ACCEPT (10, 58) NUMERO-FILHOS.
           ACCEPT (12, 50) DEPARTAMENTO.
           ACCEPT (14, 44) FUNCAO.
           ACCEPT (16, 53) TIPO-SALARIO.
           ACCEPT (18, 50) SALARIO-BASE.

       CALCULAR.
      *> Calcular salário de acordo com o seu tipo
           IF TIPO-SALARIO = 'D'
               COMPUTE SALARIO-BRUTO = SALARIO-BASE * 30
           ELSE IF TIPO-SALARIO = 'H'
               COMPUTE SALARIO-BRUTO = SALARIO-BASE * 220
           ELSE IF TIPO-SALARIO = 'M'
               COMPUTE SALARIO-BRUTO = SALARIO-BASE
           ELSE
               DISPLAY "Tipo de salario invalido, tente novamente."
               GO TO ENTRADA-DADOS
           END-IF.

           *> Calcular o INSS de acordo com o salário bruto
           IF SALARIO-BRUTO <= 2500
               COMPUTE INSS = SALARIO-BRUTO / 100 * 8
           ELSE IF SALARIO-BRUTO <= 6300
               COMPUTE INSS = SALARIO-BRUTO / 100 * 8
           ELSE
               COMPUTE INSS = SALARIO-BRUTO / 100 * 8
           END-IF.

           *> Calcular o imposto de renda
           IF (SALARIO-BRUTO - INSS) <= 5000
               COMPUTE IMPOSTO-RENDA = 0
           ELSE IF (SALARIO-BRUTO - INSS) <= 12000
               COMPUTE IMPOSTO-RENDA = (SALARIO-BRUTO - INSS) / 100 * 5
           ELSE
               COMPUTE IMPOSTO-RENDA = (SALARIO-BRUTO - INSS) / 100 * 10
           END-IF.
           *> Calcular o salário familia
           COMPUTE SALARIO-FAMILIA = NUMERO-FILHOS * 20
           *> Calcular o salário liquido
           COMPUTE SALARIO-LIQUIDO = SALARIO-BRUTO - INSS 
           - IMPOSTO-RENDA + SALARIO-FAMILIA
           
           
       DADOS.
           DISPLAY (2, 25) 
           "____________FOLHA DE PAGAMENTO DO FUNCIONARIO____________".
           
           DISPLAY (06, 10) 
           "Codigo do funcionario: " CODIGO.
           DISPLAY (08, 10) 
           "Nome do funcionario: " NOME.
           DISPLAY (10, 10) 
           "Quantidade de filhos do funcionario: " NUMERO-FILHOS.
           DISPLAY (12, 10) 
           "Departamento do funcionario: " DEPARTAMENTO.
           DISPLAY (14, 10)
           "Função do funcionario: " FUNCAO.
           DISPLAY (16, 10) 
           "Tipo de salario do funcionario: " TIPO-SALARIO.
           DISPLAY (18, 10) 
           "Salario base do funcionario: " SALARIO-BASE.
           
           DISPLAY (20, 10) 
           "VENCIMENTOS".
           DISPLAY (22, 10) 
           "Salario bruto do funcionario: " SALARIO-BRUTO.
           DISPLAY (24, 10) 
           "Salario Familia do funcionario: " SALARIO-FAMILIA.

           DISPLAY (26, 10) 
           "DESCONTOS".
           DISPLAY (28, 10) 
           "Desconto de INSS: " INSS.
           DISPLAY (30, 10) 
           "Desconto de Imposto de Renda: " IMPOSTO-RENDA.
           
           DISPLAY (34, 20) "SALARIO LIQUIDO: " SALARIO-LIQUIDO.
           
           DISPLAY (36, 10) "Enter para continuar."
           ACCEPT CODIGO.
           IF CODIGO = SPACES
               GO TO INICIO.

       