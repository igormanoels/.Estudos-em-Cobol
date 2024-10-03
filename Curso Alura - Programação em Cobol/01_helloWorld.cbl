      ******************************************************************
      *    IDENTIFICATION DIVISION                                     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01_helloWorld.
       AUTHOR. IGOR MANOEL DE SANTANA.
      *                                                                *
      * SISTEMA: MEU PRIMEIRO PROGRAMA EM COBOL                        *
      * OBJETIVO: APRENDIZADO DA LINGUAGEM                             *
      * ANALISTAS: N/A                                                 *
      * ESPECIFICADOR: N/A                                             *
      * PROGRAMADOR: IGOR MANOEL DE SANTANA                            *
      * DATA: 01/10/2024                                               *
      * LINGUAGEM: COBOL CICS/DB2                                      *
      * VERSÃO: 001                                                    *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM 01_helloWorld.
