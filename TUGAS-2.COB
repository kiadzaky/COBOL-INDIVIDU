      ******************************************************************
      * Author: KIA DZAKY ERIYOKO
        *> BERUSAHA ADALAH KUNCI UTAMA
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
       SELECT PERSONAL-IDX-EXPORT
           ASSIGN TO "C:\MAYBANK\COBOL\GNU PROJECT\data\EKSPOR-DATA.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FSTAT
           .
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD Personal-IDX.
       01 F-Personal.
          05 F-NIK PIC 9(16).
          05 F-Nama PIC A(30).
          05 F-Jabatan PIC A(20).
       FD PERSONAL-IDX-EXPORT.
           01 EKS-PERSONAL.
                  05 EKS-NIK-Z PIC ZZZZ,ZZZZ,ZZZZ,ZZZZ.
                  05 FILLER PIC X VALUE ";".
                  05 EKS-NAMA PIC X(30).
                  05 FILLER PIC X VALUE ";".
                  05 EKS-JABATAN PIC X(20).
       WORKING-STORAGE SECTION.
       01 FSTAT PIC 99.
       01 WS-COMMAND PIC X.
       01 PERSONAL.
              05 NIK PIC 9(16).
              05 NAMA PIC A(30).
              05 JABATAN PIC A(20).
       01 WS-EOF PIC 9.
       01 WS-STATUS PIC 9.
       01 WS-NIK-INPUT PIC 9(16).
       01 WS-COUNT PIC 99 VALUE 1.
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
           CLOSE PERSONAL-IDX-EXPORT
           PERFORM EVALUATE-COMMAND UNTIL WS-Command = "Q".
           STOP RUN.

       EVALUATE-COMMAND.
           MOVE 0 TO WS-EOF
           OPEN I-O PERSONAL-IDX.
           DISPLAY "(C) CREATE-RECORD"
           DISPLAY "(R) READ-RECORD"
           DISPLAY "(U) UPDATE-RECORD"
           DISPLAY "(D) DELETE-RECORD"
           DISPLAY "(L) LIST-RECORD"
           DISPLAY "(Q) QUIT"
           DISPLAY "(S) SEARCH"
           DISPLAY "(E) EXPORT DATA"
           DISPLAY " "
           DISPLAY "Command C/R/U/D/L/Q/S : " NO ADVANCING.
           ACCEPT WS-Command.
           EVALUATE TRUE
                  WHEN WS-Command = "C" OR WS-Command = "c"
                     PERFORM CREATE-RECORD
                  WHEN WS-Command = "R" OR WS-Command = "r"
                     PERFORM READ-RECORD
                  WHEN WS-COMMAND = "U" OR WS-COMMAND = "u"
                     PERFORM UPDATE-RECORD
                  WHEN WS-COMMAND = "d" OR WS-COMMAND = "D"
                     PERFORM DELETE-RECORD
                  WHEN WS-COMMAND = "L" OR WS-COMMAND = "l"
                     PERFORM LIST-RECORD
                  WHEN WS-COMMAND = "Q" OR WS-COMMAND = "q"
                     PERFORM QUIT-RECORD
                  WHEN WS-COMMAND = "S" OR WS-COMMAND = "s"
                     PERFORM PROC-SEARCH
                  WHEN WS-COMMAND = "E" OR WS-COMMAND = "e"
                     PERFORM EXPORT-DATA
                  WHEN OTHER
                  PERFORM CLEAN-SCREEN
                     DISPLAY "SALAH INPUT"
           END-EVALUATE.
              CLOSE PERSONAL-IDX.


       EXPORT-DATA.
              OPEN output PERSONAL-IDX-EXPORT.
              MOVE "EKSPOR DATA" TO EKS-PERSONAL
              WRITE EKS-PERSONAL
              CLOSE PERSONAL-IDX-EXPORT.

              OPEN EXTEND PERSONAL-IDX-EXPORT
           PERFORM UNTIL WS-EOF = "Y"
                    READ PERSONAL-IDX NEXT
                      AT END
                         MOVE "Y" TO WS-EOF
                      NOT AT END
                      MOVE F-NIK TO EKS-NIK-Z
                      MOVE F-NAMA TO EKS-NAMA
                      MOVE F-Jabatan TO EKS-JABATAN
                      WRITE EKS-PERSONAL
                      DISPLAY EKS-PERSONAL
                    END-READ
           END-PERFORM
              CLOSE PERSONAL-IDX-EXPORT.
           .

       PROC-SEARCH.
              MOVE " " TO NAMA
              MOVE " " TO JABATAN
              DISPLAY "SEARCH BY (J) JATABAN, (N) NAMA"
              DISPLAY "INPUT J/N : " NO  ADVANCING
              ACCEPT WS-COMMAND
              MOVE FUNCTION UPPER-CASE(WS-COMMAND) TO WS-COMMAND
              IF WS-COMMAND = "J"
                     DISPLAY "MASUKAN JABATAN : " NO ADVANCING
                     ACCEPT JABATAN
                     PERFORM PROC-SEARCHING
              ELSE IF WS-COMMAND = "N"
                     DISPLAY "MASUKAN NAMA : " NO ADVANCING
                     ACCEPT NAMA
                     PERFORM PROC-SEARCHING
              ELSE
                     DISPLAY "MASUKAN SALAH"
              END-IF
           .

       PROC-SEARCHING.
              PERFORM CLEAN-SCREEN
              DISPLAY "SEARCH RECORD PRESENT"
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
                      MOVE FUNCTION UPPER-CASE (NAMA) TO NAMA
                      MOVE FUNCTION UPPER-CASE (JABATAN) TO JABATAN
           IF F-Jabatan = JABATAN or F-Nama = NAMA
                      DISPLAY "---------------------------------------",
                              "---------------------------------------",
                              "---------"
                      DISPLAY NIK-Z, "   | " NO ADVANCING
                      DISPLAY F-NAMA, "   | " NO ADVANCING
                      DISPLAY F-JABATAN, "     |"
           END-IF
                    END-READ
              END-PERFORM
              DISPLAY "---------------------------------------",
                              "---------------------------------------",
                              "---------"
           PERFORM PAUSE-SCREEN
           .
       CREATE-RECORD.
              DISPLAY "CREATE RECORD PRESENT"
              DISPLAY " "
              DISPLAY "MASUKAN NIK : " NO ADVANCING
              ACCEPT NIK
              DISPLAY "MASUKAN NAMA : " NO ADVANCING
              ACCEPT NAMA
              DISPLAY "MASUKAN JABATAN : " NO ADVANCING
              ACCEPT JABATAN
              IF NIK = ZERO OR NIK = SPACE OR NAMA = SPACE
                     OR NAMA = ZERO OR JABATAN = SPACE OR JABATAN = ZERO
                     DISPLAY "DATA JANGAN KOSONG DAN NIK HARUS ANGKA"
                     DISPLAY " "
                     GO CREATE-RECORD
              END-IF.
              IF NAMA IS NOT ALPHABETIC OR JABATAN IS NOT ALPHABETIC
                     DISPLAY "NAMA DAN JABATAN HARUS ALFABET"
                     DISPLAY " "
                     GO CREATE-RECORD
              ELSE
                     MOVE PERSONAL TO F-Personal
                     MOVE FUNCTION UPPER-CASE (F-Personal) TO F-Personal
                     PERFORM WRITE-RECORD
                     PERFORM PAUSE-SCREEN
              END-IF.

      *        CLOSE PERSONAL-IDX.
       READ-RECORD.
              PERFORM CLEAN-SCREEN
              DISPLAY "READ RECORD PRESENT".
              DISPLAY " "
              DISPLAY "NIK YANG AKAN DICARI : " NO ADVANCING
              ACCEPT WS-NIK-INPUT
              CALL "SUB-TUGAS-READ" using WS-NIK-INPUT
              CANCEL "SUB-TUGAS-READ"
              *> MOVE WS-NIK-INPUT TO F-NIK
              *> MOVE F-NIK TO NIK-Z
               *> read PERSONAL-IDX
                   *> invalid
                     *> display "NIK : " NIK-Z " TIDAK ADA"
                   *> not invalid
                    *> display "NIK : " no advancing NIK-Z
                    *> display "NAMA : " no advancing F-NAMA
                    *> display "JABATAN : " no advancing F-JABATAN
                    *> DISPLAY " "
               *> END-READ
              .
      *        CLOSE PERSONAL-IDX.
       UPDATE-RECORD.
              DISPLAY "UPDATE RECORD PRESENT"
              DISPLAY " "
              DISPLAY "NIK YANG AKAN DISUNTING : " NO ADVANCING
              ACCEPT WS-NIK-INPUT
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
                    DISPLAY "***********GANTI DATA********* "
                    PERFORM REWRITE-DATA
               END-READ
              .
       REWRITE-DATA.
              DISPLAY "NAMA : " NO ADVANCING
              ACCEPT NAMA
              MOVE NAMA TO F-Nama

              DISPLAY "JABATAN : " NO ADVANCING
              ACCEPT JABATAN
              MOVE JABATAN TO F-JABATAN
              MOVE FUNCTION UPPER-CASE (F-Personal) TO F-Personal
              IF NAMA IS NOT ALPHABETIC OR JABATAN IS NOT ALPHABETIC
                     DISPLAY "NAMA DAN JABATAN HARUS ALFABET"
                     DISPLAY " "
                     GO REWRITE-DATA
              ELSE
                     REWRITE F-Personal
                      INVALID KEY DISPLAY "Record Not Updated"
                      NOT INVALID
                      DISPLAY "SUKSES SUNTING DATA"
                     END-REWRITE
              END-IF
              .
       DELETE-RECORD.
              DISPLAY "DELETE RECORD PRESENT".
              DISPLAY " "
              DISPLAY "NIK YANG AKAN DIHAPUS : " NO ADVANCING
              ACCEPT WS-NIK-INPUT
              MOVE WS-NIK-INPUT TO F-NIK
              MOVE F-NIK TO NIK-Z
              read PERSONAL-IDX
                   invalid
                     display "NIK : " NIK-Z " TIDAK ADA"
                   not invalid
                    display "NIK : " no advancing NIK-Z
                    display "NAMA : " no advancing F-NAMA
                    display "JABATAN : " no advancing F-JABATAN
                    DISPLAY "YAKIN HAPUS? (Y/N) : " NO ADVANCING
                    ACCEPT WS-COMMAND
                    IF WS-COMMAND = "Y" OR WS-COMMAND = "y"
                        MOVE 1 TO WS-STATUS
                        DELETE Personal-IDX RECORD
                           INVALID KEY MOVE 0 TO WS-Status
                        END-DELETE
                        IF WS-Status = 1
                           DISPLAY "RECORD ", NIK-Z," IS DELETED"
                        ELSE
                           DISPLAY "ERROR, RECORD CAN NOT BE DELETED"
                        END-IF
                    END-IF
               END-READ.
       LIST-RECORD.
              PERFORM CLEAN-SCREEN
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
              PERFORM PAUSE-SCREEN
              .
       QUIT-RECORD.
              DISPLAY "QUIT RECORD PRESENT"
              CLOSE PERSONAL-IDX.
              STOP RUN.
       WRITE-RECORD.
              WRITE F-Personal
                     INVALID KEY DISPLAY "ERROR, KEY DUPLICATES"
                     NOT INVALID KEY
                     DISPLAY "SUKSES TAMBAH DATA".
       CLEAN-SCREEN.
              CALL "SYSTEM" USING "CLS"
           .
       PAUSE-SCREEN.
              CALL "SYSTEM" USING "PAUSE"
              .
      ** add other procedures here
       END PROGRAM TUGAS-INDIVIDU.
