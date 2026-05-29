       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQREAD.
       AUTHOR. SOMESH MATH.

      *    OMP$COB3 CHALLENGE 9.2.1 AND 9.2.2
      *    CODE PERTAINING TO 9.2.1 ARE COMMENTED OUT TO ENABLE 9.2.2.
      *    CHALLENGE 9.2.1 - SEQUENTIAL FILE READ & DISPLAY ONE BELOW OTHER
      *    CHALLENGE 9.2.2 - SEQUENTIAL FILE READ AND DISPLAY IN A REPORT FORMAT
      *    SEQUENTIAL FILE: Z84549.COVID19.PS
      *    JCL TO COMPILE AND RUN THIS PROGRAM: Z84549.JCL.COB33CVJ.
      *    -------------------------------------------------------------
      *    This program reads a sequential file containing Covid-19 data
      *    and displays the data on the console. The input file is a physical
      *    sequential file that contains records of global country wise Covid-19
      *    data. PS file is loaded from a csv file on local machine using
      *    IND$FILE SEND command in TSO.
      *    The comma-separated values (CSV) file  is with the following format:
      *    Sl No, Country, Date, Cases, Deaths, Recovered, Active
      *    The program reads each record from the input file, parses the data,
      *    and displays the relevant information on the console. The program
      *    also handles end-of-file conditions & displays appropriate messages.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO COVIDATA
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO PRTLINE
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD PIC X(80).

       FD  OUTPUT-FILE RECORDING MODE IS F.
      *     RECORD CONTAINS 80 CHARACTERS
      *     LABEL RECORDS ARE OMITTED
      *     DATA RECORD IS OUTPUT-RECORD.

       01  OUTPUT-RECORD.
           02  OUT-SLNO        PIC ZZ9.
           02  OUT-FILLER1      PIC X(4) VALUE SPACES.
           02  OUT-COUNTRY     PIC X(32).
           02  OUT-CASES       PIC ZZ,ZZZ,ZZ9.
           02  OUT-DEATHS      PIC Z,ZZZ,ZZ9.
           02  OUT-FILLER2      PIC X(2) VALUE SPACES.
           02  OUT-RECOVERED   PIC Z,ZZZ,ZZ9.
           02  OUT-FILLER3      PIC X(6) VALUE SPACES.
           02  OUT-ACTIVE      PIC Z,ZZZ,ZZ9.


       WORKING-STORAGE SECTION.

       01  COV-RECORD.
           02   COV-SLNO        PIC 9(3).
              02   COV-SLNO-EDITED PIC ZZ9.
           02   COV-DATE        PIC X(16).
           02   COV-COUNTRY     PIC X(32).
           02   COV-CASES       PIC 9(8).
              02   COV-CASES-EDITED PIC ZZ,ZZZ,ZZ9.
           02   COV-DEATHS      PIC 9(6).
             02   COV-DEATHS-EDITED PIC Z,ZZZ,ZZ9.
           02   COV-RECOVERED   PIC 9(7).
               02   COV-RECOVERED-EDITED PIC Z,ZZZ,ZZ9.
           02   COV-ACTIVE-STRING  PIC X(8).
           02   COV-ACTIVE       PIC 9(8).
               02   COV-ACTIVE-EDITED PIC Z,ZZZ,ZZ9.





       01  FILLER1         PIC X(20) VALUE ALL '*'.

       01 FLAGS.
           05 LASTREC         PIC X VALUE SPACE.
      *
       01  HEADER-1.
           05  FILLER         PIC X(25) VALUE SPACES.
           05  FILLER         PIC X(25) VALUE 'Global Covid Data 2021'.


      *
       01  HEADER-2.
            05  FILLER         PIC X(25) VALUE 'Source: Github'.
            05  FILLER         PIC X(15) VALUE SPACES.
            05  FILLER         PIC X(15) VALUE 'Last Updated:'.
            05  FILLER         PIC X(20) VALUE '23-04-2021 04:20:00'.

      *
       01  HEADER-3.
           05  FILLER         PIC X(05) VALUE 'Sl No'.
           05  FILLER         PIC X(06) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE 'Country '.
           05  FILLER         PIC X(18) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE 'Total Cases'.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE 'Deaths'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE 'Recovered'.
           05  FILLER         PIC X(07) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'Active'.

       01  HEADER-4.
           05  FILLER         PIC X(05) VALUE '-----'.
           05  FILLER         PIC X(06) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X(18) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE '-----------'.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '------'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '----------'.
           05  FILLER         PIC X(07) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE '-------'.

      *HEADER -- structures for report or column headers,
      *that need to be setup in WORKING-STORAGE so they can be used
      *in the PROCEDURE DIVISION

       01  WS-WORK-AREAS.
           05  WS-FILE-STATUS      PIC XX VALUE SPACES.
           05  WS-EOF-FLAG         PIC X(01) VALUE 'N'.
               88  END-OF-FILE               VALUE 'Y'.

       PROCEDURE DIVISION.

       0000-START.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE.
      *     PERFORM 1000-INIT-HEADERS
      *     PERFORM 2000-MAIN-LOGIC.



       1000-INIT-HEADERS.
      *     DISPLAY HEADER-1
      *     DISPLAY HEADER-2
            WRITE OUTPUT-RECORD FROM HEADER-1
            WRITE OUTPUT-RECORD FROM HEADER-2
            WRITE OUTPUT-RECORD FROM HEADER-3
            WRITE OUTPUT-RECORD FROM HEADER-4.


       2000-MAIN-LOGIC.

      *     OPEN INPUT INPUT-FILE
      *     IF WS-FILE-STATUS NOT EQUAL TO '00'
      *         DISPLAY 'ERROR OPENING IN FILE. STATUS:' WS-FILE-STATUS
      *         STOP RUN
      *     END-IF.
      *     OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-AND-DISPLAY-PARA UNTIL END-OF-FILE.

      *     CLOSE INPUT-FILE.
      *     CLOSE OUTPUT-FILE.


           MOVE 'FILE PROCESSING COMPLETE.' TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

       READ-AND-DISPLAY-PARA.
           READ INPUT-FILE
              AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
           READ INPUT-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
               MOVE ZEROES TO COV-ACTIVE IN COV-RECORD
               UNSTRING INPUT-RECORD DELIMITED BY ',' INTO
                   COV-SLNO
                   COV-COUNTRY
                   COV-DATE
                   COV-CASES
                   COV-DEATHS
                   COV-RECOVERED
                   COV-ACTIVE-STRING

                END-UNSTRING
      *          DISPLAY FILLER1
                MOVE COV-SLNO TO COV-SLNO-EDITED
                MOVE COV-CASES TO COV-CASES-EDITED
                MOVE COV-DEATHS TO COV-DEATHS-EDITED
                MOVE COV-RECOVERED TO COV-RECOVERED-EDITED
                COMPUTE COV-ACTIVE = FUNCTION NUMVAL(COV-ACTIVE-STRING)
                MOVE COV-ACTIVE TO COV-ACTIVE-EDITED


      *          DISPLAY 'SLNO:' COV-SLNO-EDITED
      *          DISPLAY 'LAST UPDATE:' COV-DATE
      *          DISPLAY 'COUNTRY:' COV-COUNTRY
      *          DISPLAY 'CASES:' COV-CASES
      *          DISPLAY 'DEATHS:' COV-DEATHS-EDITED
      *          DISPLAY 'RECOVERED:' COV-RECOVERED-EDITED
      *          DISPLAY 'ACTIVE:' COV-ACTIVE

      *             DISPLAY INPUT-RECORD
      *     END-READ
           MOVE COV-SLNO-EDITED TO OUT-SLNO
                MOVE SPACES TO OUT-FILLER1
           MOVE COV-COUNTRY TO OUT-COUNTRY
              MOVE COV-CASES-EDITED TO OUT-CASES
                MOVE COV-DEATHS-EDITED TO OUT-DEATHS
                MOVE SPACES TO OUT-FILLER2
                MOVE COV-RECOVERED-EDITED TO OUT-RECOVERED
                MOVE SPACES TO OUT-FILLER3
                MOVE COV-ACTIVE-EDITED TO OUT-ACTIVE
           WRITE OUTPUT-RECORD
           END-READ
           END-PERFORM.
