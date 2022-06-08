      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. SUB-TUGAS-READ.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         SELECT Personal-IDX
          ASSIGN TO "C:\MAYBANK\COBOL\GNU PROJECT\data\personal.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          FILE STATUS IS FSTAT
          RECORD KEY IS F-NIK.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD Personal-IDX.
       01 F-Personal.
          05 F-NIK PIC 9(16).
          05 F-Nama PIC A(30).
          05 F-Jabatan PIC A(20).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 FSTAT PIC 99.
       01 PERSONAL-Z.
              05 NIK-Z PIC ZZZZ,ZZZZ,ZZZZ,ZZZZ.
      *-----------------------
       LINKAGE SECTION.
       01 WS-NIK-INPUT PIC 9(16).
       PROCEDURE DIVISION USING WS-NIK-INPUT.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
              OPEN I-O PERSONAL-IDX.
              MOVE WS-NIK-INPUT TO F-NIK
              MOVE F-NIK TO NIK-Z
               read PERSONAL-IDX
                   invalid
                     display "NIK : " NIK-Z " TIDAK ADA"
                   not invalid
                    display "NIK : " no advancing NIK-Z
                    display "NAMA : " no advancing F-NAMA
                    display "JABATAN : " no advancing F-JABATAN
                    DISPLAY " "
               END-READ
               CLOSE PERSONAL-IDX
               CALL "SYSTEM" USING "PAUSE"
            GOBACK.
      ** add other procedures here
       END PROGRAM SUB-TUGAS-READ.
