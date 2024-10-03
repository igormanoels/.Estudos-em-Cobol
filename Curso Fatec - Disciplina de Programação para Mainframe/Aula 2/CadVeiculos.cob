       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADVEICULO.
      **************************************
      * AUTHOR. IGOR MANOEL 
      * GITHUB. IGORMANOELS
      *
      **************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                           DECIMAL-POINT IS COMMA.
      *
      *--------------------------------------
       DATA DIVISION.
      *
      *--------------------------------------
       WORKING-STORAGE SECTION.
       01 PLACA    PIC X(12) VALUE "".
       01 MARCA    PIC X(12) VALUE "".
       01 MODELO   PIC X(15) VALUE "".
      *--------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY (02, 25) "CADASTRO DE VEICULOS".
           DISPLAY (06, 10) "PLACA: ".
           DISPLAY (08, 10) "MARCA: ".
           DISPLAY (10, 10) "MODELO: ".
           STOP " ".
       CADASTRAR-VEICULO.
           ACCEPT (06, 20) PLACA.
           IF PLACA = SPACES
               GO TO CADASTRAR-VEICULO.
           ACCEPT (08, 20) MARCA.
           ACCEPT (10, 20) MODELO.
           STOP " ".
       EXIBIR-CADASTRO.
           DISPLAY (20, 10) "PLACA: " PLACA
           DISPLAY (22, 10) "MARCA: " MARCA
           DISPLAY (24, 10) "MODELO: " MODELO 
           STOP RUN.
       
