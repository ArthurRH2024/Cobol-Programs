       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESGLOSE.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  CANTIDAD-ORIGINAL  PIC 9(8)V99.
       01  CANTIDAD           PIC 9(8)V99.
       01  BILLETE500         PIC 9(4).
       01  BILLETE200         PIC 9(4).
       01  BILLETE100         PIC 9(4).
       01  BILLETE50          PIC 9(4).
       01  BILLETE20          PIC 9(4).
       01  MONEDA10           PIC 9(4).
       01  MONEDA5            PIC 9(4).
       01  MONEDA2            PIC 9(4).
       01  MONEDA1            PIC 9(4).
       01  CENTAVOS           PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       INICIO.
           DISPLAY "Ingrese la cantidad (con 2 decimales): ".
           ACCEPT CANTIDAD-ORIGINAL.

           MOVE CANTIDAD-ORIGINAL TO CANTIDAD.

           COMPUTE BILLETE500 = FUNCTION INTEGER(CANTIDAD / 500)
           COMPUTE CANTIDAD = CANTIDAD - (BILLETE500 * 500)

           COMPUTE BILLETE200 = FUNCTION INTEGER(CANTIDAD / 200)
           COMPUTE CANTIDAD = CANTIDAD - (BILLETE200 * 200)

           COMPUTE BILLETE100 = FUNCTION INTEGER(CANTIDAD / 100)
           COMPUTE CANTIDAD = CANTIDAD - (BILLETE100 * 100)

           COMPUTE BILLETE50 = FUNCTION INTEGER(CANTIDAD / 50)
           COMPUTE CANTIDAD = CANTIDAD - (BILLETE50 * 50)

           COMPUTE BILLETE20 = FUNCTION INTEGER(CANTIDAD / 20)
           COMPUTE CANTIDAD = CANTIDAD - (BILLETE20 * 20)

           COMPUTE MONEDA10 = FUNCTION INTEGER(CANTIDAD / 10)
           COMPUTE CANTIDAD = CANTIDAD - (MONEDA10 * 10)

           COMPUTE MONEDA5 = FUNCTION INTEGER(CANTIDAD / 5)
           COMPUTE CANTIDAD = CANTIDAD - (MONEDA5 * 5)

           COMPUTE MONEDA2 = FUNCTION INTEGER(CANTIDAD / 2)
           COMPUTE CANTIDAD = CANTIDAD - (MONEDA2 * 2)

           COMPUTE MONEDA1 = FUNCTION INTEGER(CANTIDAD / 1)
           COMPUTE CANTIDAD = CANTIDAD - (MONEDA1 * 1)

           COMPUTE Centavos = Cantidad * 100

           DISPLAY "Desglose de la cantidad:"
           DISPLAY "Billetes de 500: " BILLETE500
           DISPLAY "Billetes de 200: " BILLETE200
           DISPLAY "Billetes de 100: " BILLETE100
           DISPLAY "Billetes de 50:  " BILLETE50
           DISPLAY "Billetes de 20:  " BILLETE20
           DISPLAY "Moneda de 10:    " MONEDA10
           DISPLAY "Moneda de 5:     " MONEDA5
           DISPLAY "Moneda de 2:     " MONEDA2
           DISPLAY "Moneda de 1:     " MONEDA1
           DISPLAY "Centavos:        " Centavos

           STOP RUN.
       END PROGRAM DESGLOSE.
