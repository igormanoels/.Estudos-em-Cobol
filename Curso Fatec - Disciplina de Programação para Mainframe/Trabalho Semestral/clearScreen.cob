1       IDENTIFICATION DIVISION.
       PROGRAM-ID. clearScreen.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           CALL "SYSTEM" USING "cls".
           EXIT.
