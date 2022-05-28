      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TUGAS-1.
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
          RECORD KEY IS NIK.

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD Personal-IDX.
       01 Personal.
          05 NIK PIC 9(10).
          05 Nama PIC X(30).
          05 Jabatan PIC X(20).
       WORKING-STORAGE SECTION.
          01 FSTAT PIC 99.

       PROCEDURE DIVISION.
           PERFORM CHECK-IF-FILE-EXIST.
           STOP RUN.

       CHECK-IF-FILE-EXIST.
          OPEN INPUT Personal-IDX.
          IF FSTAT = "35"
             DISPLAY "FILE NOT EXIST"
             CLOSE Personal-IDX
             OPEN OUTPUT Personal-IDX
             DISPLAY "FILE INDEX CREATED"
             CLOSE Personal-IDX
          ELSE
             DISPLAY "File Indexed sudah ada"
          END-IF
          CLOSE Personal-IDX.

      ** add other procedures here
       END PROGRAM TUGAS-1.
