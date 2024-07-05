       IDENTIFICATION DIVISION.
       PROGRAM-ID. PALABRA-CANTIDADES.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 CANORIGI   PIC 9(9)V99.
       01 CANENTER   PIC 9(9).
       01 CANCENTA   PIC 99.
       01 CANPALAB   PIC X(12) VALUE SPACES.
       01 UNIPESOS   PIC X(12) VALUE SPACES.
       01 DECPESOS   PIC X(12) VALUE SPACES.
       01 CIEPESOS   PIC X(12) VALUE SPACES.
       01 MILPESOS   PIC X(12) VALUE SPACES.
       01 CANTEMP1   PIC X(12) VALUE SPACES.
       01 CANTEMP2   PIC X(12) VALUE SPACES.
       01 CANTEMPO   PIC 9(9).
       01 UNIDAD     PIC 9     VALUE 0.
       01 DECENA     PIC 99    VALUE 0.
       01 CENTENA    PIC 999   VALUE 0.
       01 MILES      PIC 9(5)  VALUE 0.

       01 TABUNIDA.
          05 UNIVALUE OCCURS 21 TIMES PIC X(10).

       01 TABDECEN.
          05 DECVALUE OCCURS 8 TIMES PIC X(10).

       01 TABESPEC.
          05 ESPVALUE OCCURS 10 TIMES PIC X(10).

       01 TABCIENT.
          05 CIEVALUE OCCURS 10 TIMES PIC X(13).

       PROCEDURE DIVISION.
       INICIO.
           MOVE "CERO      " TO UNIVALUE(1)
           MOVE "UN        " TO UNIVALUE(2)
           MOVE "DOS       " TO UNIVALUE(3)
           MOVE "TRES      " TO UNIVALUE(4)
           MOVE "CUATRO    " TO UNIVALUE(5)
           MOVE "CINCO     " TO UNIVALUE(6)
           MOVE "SEIS      " TO UNIVALUE(7)
           MOVE "SIETE     " TO UNIVALUE(8)
           MOVE "OCHO      " TO UNIVALUE(9)
           MOVE "NUEVE     " TO UNIVALUE(10)
           MOVE "DIEZ      " TO UNIVALUE(11)
           MOVE "ONCE      " TO UNIVALUE(12)
           MOVE "DOCE      " TO UNIVALUE(13)
           MOVE "TRECE     " TO UNIVALUE(14)
           MOVE "CATORCE   " TO UNIVALUE(15)
           MOVE "QUINCE    " TO UNIVALUE(16)
           MOVE "DIECISEIS " TO UNIVALUE(17)
           MOVE "DIECISIETE" TO UNIVALUE(18)
           MOVE "DIECIOCHO " TO UNIVALUE(19)
           MOVE "DIECINUEVE" TO UNIVALUE(20)
           MOVE "VEINTE    " TO UNIVALUE(21)

           MOVE "VEINTE    " TO DECVALUE(1)
           MOVE "TREINTA   " TO DECVALUE(2)
           MOVE "CUARENTA  " TO DECVALUE(3)
           MOVE "CINCUENTA " TO DECVALUE(4)
           MOVE "SESENTA   " TO DECVALUE(5)
           MOVE "SETENTA   " TO DECVALUE(6)
           MOVE "OCHENTA   " TO DECVALUE(7)
           MOVE "NOVENTA   " TO DECVALUE(8)

           MOVE "CIEN         " TO CIEVALUE(1)
           MOVE "CIENTO       " TO CIEVALUE(2)
           MOVE "DOSCIENTOS   " TO CIEVALUE(3)
           MOVE "TRESCIENTOS  " TO CIEVALUE(4)
           MOVE "CUATROCIENTOS" TO CIEVALUE(5)
           MOVE "QUINIENTOS   " TO CIEVALUE(6)
           MOVE "SEISCIENTOS  " TO CIEVALUE(7)
           MOVE "SETECIENTOS  " TO CIEVALUE(8)
           MOVE "OCHOCIENTOS  " TO CIEVALUE(9)
           MOVE "NOVECIENTOS  " TO CIEVALUE(10)

           DISPLAY "CANTIDAD CON 2 DECIMALES: " ACCEPT CANORIGI
           IF CANORIGI > 999999.99
              DISPLAY "POR EL MOMENTO SOLO DEL 1 AL 999,999.99"
              STOP RUN
           END-IF
           COMPUTE CANENTER = FUNCTION INTEGER(CANORIGI)
           COMPUTE CANCENTA = FUNCTION MOD(CANORIGI * 100, 100)
           MOVE CANENTER TO CANTEMPO
           IF CANTEMPO = 0
               DISPLAY "CERO PESOS CON " CANCENTA " CENTAVOS"
               STOP RUN
           END-IF

           IF CANTEMPO >= 1 AND CANTEMPO <= 20
               MOVE UNIVALUE(CANTEMPO + 1) TO UNIPESOS
               DISPLAY FUNCTION TRIM(UNIPESOS) " PESOS CON "
                       CANCENTA " CENTAVOS"
               STOP RUN
           END-IF

           IF CANTEMPO >= 21 AND CANTEMPO <= 99
              DIVIDE CANTEMPO BY 10 GIVING DECENA REMAINDER UNIDAD
              IF UNIDAD <> 0
                 MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                 MOVE DECVALUE(DECENA - 1) TO DECPESOS
                 DISPLAY FUNCTION TRIM(DECPESOS) " Y " FUNCTION
                      TRIM(UNIPESOS) " PESOS CON " CANCENTA " CENTAVOS"
              ELSE
                 MOVE SPACE TO UNIPESOS
                 MOVE DECVALUE(DECENA - 1) TO DECPESOS
                 DISPLAY FUNCTION TRIM(DECPESOS) " " FUNCTION
                      TRIM(UNIPESOS) " PESOS CON " CANCENTA " CENTAVOS"
              END-IF
              STOP RUN
           END-IF

           IF CANTEMPO >= 100 AND CANTEMPO <= 999
              DIVIDE CANTEMPO BY 100 GIVING CENTENA REMAINDER DECENA
              IF DECENA = 0
                 IF CENTENA = 1
                    DISPLAY "CIEN PESOS CON " CANCENTA " CENTAVOS"
                 ElSE
                    MOVE CIEVALUE(CENTENA + 1) TO CIEPESOS
                    DISPLAY FUNCTION TRIM(CIEPESOS) " PESOS CON "
                            CANCENTA " CENTAVOS"
                 END-IF
              ELSE
                 MOVE DECENA TO CANTEMPO
                 MOVE CIEVALUE(CENTENA + 1) TO CIEPESOS
                 IF CANTEMPO >= 1 AND CANTEMPO <= 20
                    MOVE UNIVALUE(CANTEMPO + 1) TO UNIPESOS
                    DISPLAY FUNCTION TRIM(CIEPESOS) " "
                            FUNCTION TRIM(UNIPESOS) " PESOS CON "
                            CANCENTA " CENTAVOS"
                 END-IF
                 IF CANTEMPO >= 21 AND CANTEMPO <= 99
                    DIVIDE CANTEMPO BY 10 GIVING DECENA REMAINDER UNIDAD
                    IF UNIDAD <> 0
                       MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                       MOVE DECVALUE(DECENA - 1) TO DECPESOS
                       DISPLAY FUNCTION TRIM(CIEPESOS) " "
                   FUNCTION TRIM(DECPESOS) " Y " FUNCTION TRIM(UNIPESOS)
                       " PESOS CON " CANCENTA " CENTAVOS"
                    ELSE
                       MOVE SPACE TO UNIPESOS
                       MOVE DECVALUE(DECENA - 1) TO DECPESOS
                       DISPLAY FUNCTION TRIM(CIEPESOS) " "
                       FUNCTION TRIM(DECPESOS) " "
                       FUNCTION TRIM(UNIPESOS) " PESOS CON "
                       CANCENTA " CENTAVOS"
                    END-IF
                 END-IF
              END-IF
              STOP RUN
           END-IF

           IF CANTEMPO >= 1000 AND CANTEMPO <= 999999
              DIVIDE CANTEMPO BY 1000 GIVING MILES REMAINDER CENTENA
              IF CENTENA = 0
                 IF MILES >= 1 AND MILES <= 20
                    MOVE UNIVALUE(MILES + 1) TO UNIPESOS
                    DISPLAY FUNCTION TRIM(UNIPESOS) " MIL PESOS CON "
                            CANCENTA " CENTAVOS"
                 END-IF
                 IF MILES >= 21 AND MILES <= 99
                    DIVIDE MILES BY 10 GIVING DECENA REMAINDER UNIDAD
                    MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                    MOVE DECVALUE(DECENA - 1) TO DECPESOS
                    DISPLAY FUNCTION TRIM(CIEPESOS)
                    FUNCTION TRIM(DECPESOS) " Y "
                    FUNCTION TRIM(UNIPESOS) " MIL PESOS CON "
                    CANCENTA " CENTAVOS"
                 END-IF
                 IF MILES >= 100 AND MILES <= 999
                    DIVIDE MILES BY 100 GIVING CENTENA REMAINDER DECENA
                    IF DECENA = 0
                       IF CENTENA = 1
                          DISPLAY "CIEN MIL PESOS CON "
                                  CANCENTA " CENTAVOS"
                       ElSE
                          MOVE CIEVALUE(CENTENA + 1) TO CIEPESOS
                       DISPLAY FUNCTION TRIM(CIEPESOS) " MIL PESOS CON "
                                  CANCENTA " CENTAVOS"
                       END-IF
                    ELSE
                       MOVE DECENA TO CANTEMPO
                       MOVE CIEVALUE(CENTENA + 1) TO CIEPESOS
                       IF CANTEMPO >= 1 AND CANTEMPO <= 20
                          MOVE UNIVALUE(CANTEMPO + 1) TO UNIPESOS
                          DISPLAY FUNCTION TRIM(CIEPESOS) " "
                               FUNCTION TRIM(UNIPESOS) " MIL PESOS CON "
                                  CANCENTA " CENTAVOS"
                       END-IF
                       IF CANTEMPO >= 21 AND CANTEMPO <= 99
                          DIVIDE CANTEMPO BY 10 GIVING DECENA
                                 REMAINDER UNIDAD
                          IF UNIDAD <> 0
                             MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                             MOVE DECVALUE(DECENA - 1) TO DECPESOS
                             DISPLAY FUNCTION TRIM(CIEPESOS) " "
                                     FUNCTION TRIM(DECPESOS) " Y "
                               FUNCTION TRIM(UNIPESOS) " MIL PESOS CON "
                                     CANCENTA " CENTAVOS"
                          ELSE
                             MOVE SPACE TO UNIPESOS
                             MOVE DECVALUE(DECENA - 1) TO DECPESOS
                             DISPLAY FUNCTION TRIM(CIEPESOS) " "
                                     FUNCTION TRIM(DECPESOS) " "
                               FUNCTION TRIM(UNIPESOS) " MIL PESOS CON "
                                     CANCENTA " CENTAVOS"
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 IF MILES >= 1 AND MILES <= 20
                    MOVE UNIVALUE(MILES + 1) TO MILPESOS

                    IF CENTENA >= 1 AND CENTENA <= 20
                       MOVE UNIVALUE(CENTENA + 1) TO CIEPESOS
                       DISPLAY FUNCTION TRIM(MILPESOS) " MIL "
                       FUNCTION TRIM(CIEPESOS) " PESOS CON "
                       CANCENTA " CENTAVOS"
                    END-IF
                    IF CENTENA >= 21 AND CENTENA <= 99
                       DIVIDE CENTENA BY 10 GIVING DECENA
                                            REMAINDER UNIDAD
                       MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                       MOVE DECVALUE(DECENA - 1) TO DECPESOS
                       IF UNIDAD <> 0
                          DISPLAY FUNCTION TRIM(MILPESOS) " MIL "
                          FUNCTION TRIM(DECPESOS) " Y "
                          FUNCTION TRIM(UNIPESOS) " PESOS CON "
                          CANCENTA " CENTAVOS"
                       ELSE
                          MOVE SPACE TO UNIPESOS
                          MOVE DECVALUE(DECENA - 1) TO DECPESOS
                          DISPLAY FUNCTION TRIM(MILPESOS) " MIL "
                          FUNCTION TRIM(CIEPESOS)
                          FUNCTION TRIM(DECPESOS) " "
                          FUNCTION TRIM(UNIPESOS) "PESOS CON "
                          CANCENTA " CENTAVOS"
                       END-IF
                    ELSE
                       DIVIDE CENTENA BY 100 GIVING CENTENA
                                             REMAINDER DECENA
                       IF DECENA = 0
                          IF CENTENA = 1
                             DISPLAY FUNCTION TRIM(MILPESOS)
                             " MIL CIEN PESOS CON " CANCENTA " CENTAVOS"
                          ElSE
                             MOVE CIEVALUE(CENTENA + 1) TO CIEPESOS
                             DISPLAY FUNCTION TRIM(MILPESOS) " MIL "
                             FUNCTION TRIM(CIEPESOS) " PESOS CON "
                             CANCENTA " CENTAVOS"
                          END-IF
                       ELSE
                           MOVE DECENA TO CANTEMPO
                           IF CANTEMPO >= 21 AND CANTEMPO <= 99
                              DIVIDE CANTEMPO BY 10 GIVING DECENA
                                     REMAINDER UNIDAD
                              IF UNIDAD <> 0
                                 MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                                 MOVE DECVALUE(DECENA - 1) TO DECPESOS
                                DISPLAY FUNCTION TRIM(MILPESOS) " MIL "
                                 FUNCTION TRIM(CIEPESOS) " "
                                 FUNCTION TRIM(DECPESOS) " Y "
                                 FUNCTION TRIM(UNIPESOS) " PESOS CON "
                                 CANCENTA " CENTAVOS"
                              ELSE
                                 MOVE SPACE TO UNIPESOS
                                 MOVE DECVALUE(DECENA - 1) TO DECPESOS
                                DISPLAY FUNCTION TRIM(MILPESOS) " MIL "
                                 FUNCTION TRIM(CIEPESOS) " "
                                 FUNCTION TRIM(DECPESOS) " "
                                 FUNCTION TRIM(UNIPESOS) " PESOS CON "
                                 CANCENTA " CENTAVOS"
                              END-IF
                           END-IF
                       END-IF
                    END-IF
                 END-IF
                 IF MILES >= 21 AND MILES <= 99
                    DIVIDE MILES BY 10 GIVING DECENA REMAINDER UNIDAD
                    MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                    MOVE DECVALUE(DECENA - 1) TO DECPESOS
                    IF CENTENA >= 1 AND CENTENA <= 20
                       MOVE UNIVALUE(CENTENA + 1) TO CIEPESOS
                       DISPLAY FUNCTION TRIM(DECPESOS) " Y "
                       FUNCTION TRIM(UNIPESOS) " MIL "
                       FUNCTION TRIM(CIEPESOS) " PESOS CON "
                       CANCENTA " CENTAVOS"
                    END-IF
                    IF CENTENA >= 21 AND CENTENA <= 99
                       DIVIDE MILES BY 10 GIVING MILES
                              REMAINDER CANTEMPO
                       IF CANTEMPO <> 0
                          MOVE UNIVALUE(CANTEMPO + 1) TO UNIPESOS
                          MOVE DECVALUE(MILES - 1) TO DECPESOS
                          MOVE DECPESOS TO CANTEMP1
                          MOVE UNIPESOS TO CANTEMP2

                       ELSE
                          MOVE SPACE TO UNIPESOS
                          MOVE DECVALUE(MILES - 1) TO DECPESOS
                          MOVE DECPESOS TO CANTEMP1
                          MOVE UNIPESOS TO CANTEMP2
                       END-IF
                       DIVIDE CENTENA BY 10 GIVING DECENA
                              REMAINDER UNIDAD
                       MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                       MOVE DECVALUE(DECENA - 1) TO DECPESOS
                       IF UNIDAD <> 0
                          DISPLAY FUNCTION TRIM(CANTEMP1) " Y "
                          FUNCTION TRIM(CANTEMP2) " MIL "
                          FUNCTION TRIM(MILPESOS) " "
                          FUNCTION TRIM(DECPESOS) " Y "
                          FUNCTION TRIM(UNIPESOS) " PESOS CON "
                          CANCENTA " CENTAVOS"
                       ELSE
                          MOVE SPACE TO UNIPESOS
                          MOVE DECVALUE(DECENA - 1) TO DECPESOS
                          DISPLAY FUNCTION TRIM(CANTEMP1) " "
                          FUNCTION TRIM(CANTEMP2) " MIL "
                          FUNCTION TRIM(MILPESOS) " "
                          FUNCTION TRIM(CIEPESOS)
                          FUNCTION TRIM(DECPESOS) " "
                          FUNCTION TRIM(UNIPESOS) "PESOS CON "
                          CANCENTA " CENTAVOS"
                       END-IF
                    END-IF
                 END-IF
                 IF MILES >= 100 AND MILES <= 999
                    DIVIDE MILES BY 100 GIVING MILES REMAINDER CANTEMPO
                    IF CANTEMPO = 0
                       IF MILES = 1
                          IF CENTENA >= 1 AND CENTENA <= 20
                             MOVE UNIVALUE(CENTENA + 1) TO UNIPESOS
                             DISPLAY "CIEN MIL "
                             FUNCTION TRIM(UNIPESOS) " PESOS CON "
                             CANCENTA " CENTAVOS"

                          END-IF
                          IF CENTENA >= 21 AND CENTENA <= 99
                             DIVIDE CENTENA BY 10 GIVING DECENA
                             REMAINDER UNIDAD
                             IF UNIDAD <> 0
                                 MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                                 MOVE DECVALUE(DECENA - 1) TO DECPESOS
                                 DISPLAY "CIEN MIL "
                                 FUNCTION TRIM(DECPESOS) " Y "
                                 FUNCTION TRIM(UNIPESOS) " PESOS CON "
                                 CANCENTA " CENTAVOS"
                             ELSE
                                 MOVE SPACE TO UNIPESOS
                                 MOVE DECVALUE(DECENA - 1) TO DECPESOS
                                 DISPLAY "CIEN MIL "
                                 FUNCTION TRIM(DECPESOS) " "
                                 FUNCTION TRIM(UNIPESOS) " PESOS CON "
                                 CANCENTA " CENTAVOS"
                             END-IF
                          ELSE
                             DIVIDE CENTENA BY 100 GIVING CENTENA
                                    REMAINDER DECENA
                             IF DECENA = 0
                                IF CENTENA = 1
                                   DISPLAY "CIEN MIL CIEN PESOS CON "
                                   CANCENTA " CENTAVOS"
                                ElSE
                                  MOVE CIEVALUE(CENTENA + 1) TO CIEPESOS
                                   DISPLAY "CIEN MIL "
                                   FUNCTION TRIM(CIEPESOS) " PESOS CON "
                                   CANCENTA " CENTAVOS"
                                END-IF
                             ELSE
                                MOVE DECENA TO CANTEMPO
                                MOVE CIEVALUE(CENTENA + 1) TO CIEPESOS
                                IF CANTEMPO >= 1 AND CANTEMPO <= 20
                                 MOVE UNIVALUE(CANTEMPO + 1) TO UNIPESOS
                                   DISPLAY "CIEN MIL "
                                   FUNCTION TRIM(CIEPESOS) " "
                                   FUNCTION TRIM(UNIPESOS) " PESOS CON "
                                   CANCENTA " CENTAVOS"
                                END-IF
                                IF CANTEMPO >= 21 AND CANTEMPO <= 99
                                   DIVIDE CANTEMPO BY 10 GIVING DECENA
                                          REMAINDER UNIDAD
                                   IF UNIDAD <> 0
                                   MOVE UNIVALUE(UNIDAD + 1) TO UNIPESOS
                                   MOVE DECVALUE(DECENA - 1) TO DECPESOS
                                   DISPLAY "CIEN MIL "
                                   FUNCTION TRIM(CIEPESOS) " "
                                   FUNCTION TRIM(DECPESOS) " Y "
                                   FUNCTION TRIM(UNIPESOS) " PESOS CON "
                                   CANCENTA " CENTAVOS"
                                   ELSE
                                      MOVE SPACE TO UNIPESOS
                                   MOVE DECVALUE(DECENA - 1) TO DECPESOS
                                      DISPLAY "CIEN MIL "
                                      FUNCTION TRIM(CIEPESOS) " "
                                      FUNCTION TRIM(DECPESOS)
                                      " PESOS CON " CANCENTA " CENTAVOS"
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       ElSE
                          MOVE CIEVALUE(MILES + 1) TO CIEPESOS
                          IF CENTENA >= 1 AND CENTENA <= 20
                             MOVE UNIVALUE(CENTENA + 1) TO UNIPESOS
                             DISPLAY FUNCTION TRIM(CIEPESOS) " MIL "
                             FUNCTION TRIM(UNIPESOS) " PESOS CON "
                             CANCENTA " CENTAVOS"
                          END-IF

                       END-IF
                    END-IF
                 END-IF
              END-IF
              STOP RUN
           END-IF
           .

       END PROGRAM PALABRA-CANTIDADES.
