      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TUGAS-INDIVIDU.
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
          05 F-Nama PIC X(30).
          05 F-Jabatan PIC X(20).
       WORKING-STORAGE SECTION.
       01 FSTAT PIC 99.
       01 WS-COMMAND PIC X.
       01 PERSONAL.
              05 NIK PIC 9(16).
              05 NAMA PIC X(30).
              05 JABATAN PIC X(20).
       01 WS-EOF PIC 9.
       01 WS-STATUS PIC 9.
       01 WS-NIK-INPUT PIC 9(16).
       01 PERSONAL-Z.
              05 NIK-Z PIC ZZZZ,ZZZZ,ZZZZ,ZZZZ.
       PROCEDURE DIVISION.
       MAIN-START.
              PERFORM CHECK-IF-FILE-EXIST.

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
           CLOSE PERSONAL-IDX.

           PERFORM EVALUATE-COMMAND UNTIL WS-Command = "Q".
           STOP RUN.

       EVALUATE-COMMAND.
           MOVE 0 TO WS-EOF
           OPEN I-O PERSONAL-IDX.
           DISPLAY "Command C/R/U/D/L/Q(uit) : " NO ADVANCING.
           ACCEPT WS-Command.
           EVALUATE TRUE
                  WHEN WS-Command = "C" OR WS-Command = "c"
                     PERFORM CREATE-RECORD
                  WHEN WS-Command = "R" OR WS-Command = "r"
                     PERFORM READ-RECORD
                  WHEN WS-COMMAND = "U" OR WS-COMMAND = "U"
                     PERFORM UPDATE-RECORD
                  WHEN WS-COMMAND = "d" OR WS-COMMAND = "D"
                     PERFORM DELETE-RECORD
                  WHEN WS-COMMAND = "L" OR WS-COMMAND = "l"
                  *> RECORD SORTING BY ASC
                     PERFORM LIST-RECORD
                  WHEN WS-COMMAND = "Q" OR WS-COMMAND = "q"
                     PERFORM QUIT-RECORD
           END-EVALUATE.
              CLOSE PERSONAL-IDX.
       CREATE-RECORD.
              DISPLAY "CREATE RECORD PRESENT"
              DISPLAY " "
              DISPLAY "MASUKAN NIK : " NO ADVANCING
              ACCEPT NIK
              DISPLAY "MASUKAN NAMA : " NO ADVANCING
              ACCEPT NAMA
              DISPLAY "MASUKAN JABATAN : " NO ADVANCING
              ACCEPT JABATAN
              MOVE PERSONAL TO F-Personal
              PERFORM WRITE-RECORD.
      *        CLOSE PERSONAL-IDX.
       READ-RECORD.
              DISPLAY "READ RECORD PRESENT".
              DISPLAY " "
              DISPLAY "NIK YANG AKAN DICARI : " NO ADVANCING
              ACCEPT WS-NIK-INPUT
              MOVE WS-NIK-INPUT TO F-NIK
               read PERSONAL-IDX
                   invalid
                     display "NIK : " F-NIK " TIDAK ADA"
                   not invalid
                    display "NIK : " no advancing F-NIK
                    display "NAMA : " no advancing F-NAMA
                    display "JABATAN : " no advancing F-JABATAN
                    DISPLAY " "
               END-READ
              .
      *        CLOSE PERSONAL-IDX.
       UPDATE-RECORD.
              DISPLAY "UPDATE RECORD PRESENT"
              DISPLAY " "
              DISPLAY "NIK YANG AKAN DISUNTING : " NO ADVANCING
              ACCEPT WS-NIK-INPUT
              MOVE WS-NIK-INPUT TO F-NIK
               read PERSONAL-IDX
                   invalid
                     display "NIK : " F-NIK " TIDAK ADA"
                   not invalid
                    display "NIK : " no advancing F-NIK
                    display "NAMA : " no advancing F-NAMA
                    display "JABATAN : " no advancing F-JABATAN
                    DISPLAY " "
                    DISPLAY "***********GANTI DATA********* "
                    PERFORM REWRITE-DATA
               END-READ.
              .
       REWRITE-DATA.
              DISPLAY "NAMA : " NO ADVANCING
              ACCEPT NAMA
              MOVE NAMA TO F-Nama

              DISPLAY "JABATAN : " NO ADVANCING
              ACCEPT JABATAN
              MOVE JABATAN TO F-JABATAN

              REWRITE F-Personal
               INVALID KEY DISPLAY "Record Not Updated"
               NOT INVALID
               DISPLAY "SUKSES SUNTING DATA"
              END-REWRITE.
              .
       DELETE-RECORD.
              DISPLAY "DELETE RECORD PRESENT".
              DISPLAY " "
              DISPLAY "NIK YANG AKAN DIHAPUS : " NO ADVANCING
              ACCEPT WS-NIK-INPUT
              MOVE WS-NIK-INPUT TO F-NIK
              read PERSONAL-IDX
                   invalid
                     display "NIK : " F-NIK " TIDAK ADA"
                   not invalid
                    display "NIK : " no advancing F-NIK
                    display "NAMA : " no advancing F-NAMA
                    display "JABATAN : " no advancing F-JABATAN
                    DISPLAY "YAKIN HAPUS? (Y/N) : " NO ADVANCING
                    ACCEPT WS-COMMAND
                    IF WS-COMMAND = "Y"
                        MOVE 1 TO WS-STATUS
                        DELETE Personal-IDX RECORD
                           INVALID KEY MOVE 0 TO WS-Status
                        END-DELETE
                        IF WS-Status = 1
                           DISPLAY "RECORD ", F-NIK," IS DELETED"
                        ELSE
                           DISPLAY "ERROR, RECORD CAN NOT BE DELETED"
                        END-IF
                    END-IF
               END-READ.
       LIST-RECORD.
              DISPLAY "LIST RECORD PRESENT"
              DISPLAY " "
              DISPLAY "       NIK            "
                            NO ADVANCING
                      DISPLAY "|" NO ADVANCING
                      DISPLAY "     NAMA                         "
                            NO ADVANCING
                      DISPLAY "|" NO ADVANCING
                      DISPLAY "        JABATAN           "
                            NO ADVANCING
                      DISPLAY "|"
              PERFORM UNTIL WS-EOF = "Y"
                    READ PERSONAL-IDX NEXT
                      AT END
                         MOVE "Y" TO WS-EOF
                      NOT AT END
                      MOVE F-NIK TO NIK-Z
                      DISPLAY "---------------------------------------",
                              "---------------------------------------",
                              "---------"
                      DISPLAY NIK-Z, "   | " NO ADVANCING
                      DISPLAY F-NAMA, "   | " NO ADVANCING
                      DISPLAY F-JABATAN, "     |"
                    END-READ
              END-PERFORM
              DISPLAY "---------------------------------------",
                              "---------------------------------------",
                              "---------"
              .
       QUIT-RECORD.
              DISPLAY "QUIT RECORD PRESENT"
              CLOSE PERSONAL-IDX.
              STOP RUN.
       WRITE-RECORD.
              WRITE F-Personal
                     INVALID KEY DISPLAY "ERROR, KEY DUPLICATES".
      ** add other procedures here
       END PROGRAM TUGAS-INDIVIDU.
