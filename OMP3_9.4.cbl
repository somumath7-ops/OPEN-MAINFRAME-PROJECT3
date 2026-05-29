       IDENTIFICATION DIVISION.
       PROGRAM-ID. HACKER.
       AUTHOR. SOMESH MATH.

      *OMP$COB3 CHALLENGE 9.4: HACKER NEW RANKINGS FOR MAINFRAME/
      *COBOL POSTS.
      *
      *This program reads the input csv file from a Zos dataset:
      *ZOS.PUBLIC.HACKER.NEWS.D250518. The file mentioned in the
      *challenge i.e. ZOS.PUBLIC.HACKER.NEWS was ignored since it had
      *only two Mainframe/Cobol posts.
      *
      *JCL TO COMPILE AND RUN/SORT THIS PROGRAM:Z84549.JCL.COBHAC1J.jcl.
      *-------------------------------------------------------------
      *This program reads a physical sequential file mentiond above
      *from the Zos environment and checks for various forms of strings
      *Cobol and Mainframes. This is accomplished by converting entire
      *Title string to lower case and looking for those words. Data was
      *parsed using Unstring statement and further the Date field was
      *parsed to separate date and time fields. Ranking score was
      *calculated as per the given formula. Junk data was revmoVed by
      *first unstringiNg into a string field and then using NUMVAL to get
      * the numeric values of date time.
      *
      *The outputted data was loaded in a PS file: Z84549.HACK.OUT.PS
      *and the sorted output with the score in descending order using
      *ICETOOL was printed in PRINTTLINE SYSOUT. Display statements
      *were used to debug the program during run time.




       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO HACKFILE
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO PRTLINE
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE IS F
           RECORD CONTAINS 143 CHARACTERS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS HACK-IN-FIELDS.
       01  HACK-IN-FIELDS  PIC X(143).



       FD  OUTPUT-FILE RECORDING MODE IS F
      *     RECORD CONTAINS 161 CHARACTERS
      *     LABEL RECORDS ARE OMITTED
           DATA RECORD IS HACK-OUT-FIELDS.


       01  HACK-OUT-FIELDS.
           05  FILLER                PIC X(1).
           05 HACK-OUT-ID            PIC X(8).
           05  OUT-FILLER1           PIC X(2) VALUE SPACES.
           05 HACK-OUT-TITLE         PIC X(96).
           05 HACK-OUT-POINTS        PIC ZZZ9.
           05  OUT-FILLER1           PIC X(2) VALUE SPACES.
           05 HACK-OUT-COMMENTS      PIC ZZZ9.
           05  OUT-FILLER1           PIC X(4) VALUE SPACES.
           05 HACK-OUT-AUTHOR        PIC X(15).
           05 HACK-OUT-TIME          PIC X(5).
           05  OUT-FILLER1           PIC X(2) VALUE SPACES.
      *     05 HACK-OUT-CREATE-DT     PIC X(16).
      *     05 HACK-OUT-CREATE-DT     PIC X(10).
           05 HACK-OUT-RANKING-SCORE-EDITED PIC Z9.9999 VALUE ZERO.

       WORKING-STORAGE SECTION.

       01  WS-HACK-OUT-RANKING-SCORE PIC S99V9999 COMP-3 VALUE ZERO.


       01  WS-HACK-IN-FILE-RECORD PIC X(143).

       01  WS-HACK-IN-FIELDS.

           05 WS-HACK-IN-ID           PIC X(8).
           05 WS-HACK-IN-TITLE        PIC X(96).
           05 WS-HACK-IN-POINTS       PIC 9(4).
           05 WS-HACK-IN-POINTS-EDITED PIC ZZZ9.
           05 WS-HACK-IN-COMMENTS     PIC 9(4).
           05 WS-HACK-IN-COMMENTS-EDITED PIC ZZZ9.
           05 WS-HACK-IN-AUTHOR       PIC X(15).
           05 WS-HACK-IN-CREATE-DT    PIC X(16).


       01  FILLER1         PIC X(20) VALUE ALL '*'.

       01  FLAGS.
           05 LASTREC         PIC X VALUE SPACE.
      *
       01  HEADER-1.
           05  FILLER        PIC X(25) VALUE SPACES.
           05  FILLER        PIC X(25) VALUE 'Hacker News Rankings for'.
           05  FILLER        PIC X(22) VALUE 'Mainframe/COBOL Posts'.

       01  HEADER-2.
            05  FILLER         PIC X(25) VALUE 'Source: Github'.
            05  FILLER         PIC X(15) VALUE SPACES.
            05  FILLER         PIC X(08) VALUE 'Period:'.
            05  FILLER         PIC X(10) VALUE '2015-2016'.

      *
       01  HEADER-3.
           05  FILLER         PIC X(08) VALUE 'ID'.
           05  FILLER         PIC X(06) VALUE SPACES.
           05  FILLER         PIC X(90) VALUE 'TITLE '.
           05  FILLER         PIC X VALUE SPACES.
           05  FILLER         PIC X(06) VALUE 'Points'.
           05  FILLER         PIC X VALUE SPACES.
           05  FILLER         PIC X(08) VALUE 'Comments'.
           05  FILLER         PIC X VALUE SPACES.
           05  FILLER         PIC X(10) VALUE 'Author'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'TIME'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'Score'.

       01  HEADER-4.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X(06) VALUE SPACES.
           05  FILLER         PIC X(90) VALUE '--------'.
           05  FILLER         PIC X VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '------'.
           05  FILLER         PIC X VALUE SPACES.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '------'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE '-----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE '-----'.

      *HEADER -- structures for report or column headers,
      *that need to be setup in WORKING-STORAGE so they can be used
      *in the PROCEDURE DIVISION


       01 WS-INPUT-TIME.
           05 WS-DATE          PIC X(10) VALUE SPACES.
           05 WS-TIME          PIC X(5) VALUE SPACES.
           05 WS-WORK-DATE     PIC X(10) VALUE SPACES.
           05 WS-WORK-TIME     PIC X(5) VALUE SPACES.
           05 WS-HH-STRING     PIC X(02) VALUE SPACES.
           05 WS-MM-STRING     PIC X(02) VALUE SPACES.
           05 WS-HH            PIC 9(02) VALUE ZEROES.
           05 WS-MM            PIC 9(02) VALUE ZEROES.

       01  WS-DECIMAL-HOURS     PIC 99V99 COMP-3 VALUE ZERO.
       01  WS-AGE-HOURS        PIC 99V99 COMP-3 VALUE ZERO.
       01  WS-NUMERATOR          PIC S9999V9999 COMP-3 VALUE ZERO.
       01  WS-DENOMINATOR        PIC S9999V9999 COMP-3 VALUE 1.


       01  WS-WORK-AREAS.
           05 WS-LOWER-CASE-TITLE  PIC X(96) VALUE SPACES.
           05  WS-COUNT             PIC 9(3) VALUE ZERO.
           05 WS-RECORD-COUNT      PIC 9(6) VALUE ZERO.
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
            WRITE HACK-OUT-FIELDS FROM HEADER-1
            WRITE HACK-OUT-FIELDS FROM HEADER-2
            WRITE HACK-OUT-FIELDS FROM HEADER-3
            WRITE HACK-OUT-FIELDS FROM HEADER-4.


       2000-MAIN-LOGIC.

      *     OPEN INPUT INPUT-FILE
      *     IF WS-FILE-STATUS NOT EQUAL TO '00'
      *         DISPLAY 'ERROR OPENING IN FILE. STATUS:' WS-FILE-STATUS
      *         STOP RUN
      *     END-IF.
      *     OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-AND-PRINT-PARA.

      *     CLOSE INPUT-FILE.
      *     CLOSE OUTPUT-FILE.


           MOVE 'FILE PROCESSING COMPLETE.' TO HACK-OUT-FIELDS
           WRITE HACK-OUT-FIELDS
           DISPLAY WS-EOF-FLAG
           DISPLAY 'TOTAL RECORDS PROCESSED:' WS-RECORD-COUNT
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

       READ-AND-PRINT-PARA.
           READ INPUT-FILE
      *        AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
           READ INPUT-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END


               MOVE HACK-IN-FIELDS TO WS-HACK-IN-FILE-RECORD
               INSPECT WS-HACK-IN-FILE-RECORD REPLACING FIRST ',' BY
               ' ' AFTER INITIAL '"'

               UNSTRING WS-HACK-IN-FILE-RECORD  DELIMITED BY ',' INTO
                   WS-HACK-IN-ID
                   WS-HACK-IN-TITLE
                   WS-HACK-IN-POINTS
                   WS-HACK-IN-COMMENTS
                   WS-HACK-IN-AUTHOR
                   WS-HACK-IN-CREATE-DT
               END-UNSTRING

            MOVE FUNCTION LOWER-CASE (WS-HACK-IN-TITLE)
            TO WS-LOWER-CASE-TITLE

             INSPECT WS-LOWER-CASE-TITLE TALLYING WS-COUNT
             FOR ALL 'cobol'

             INSPECT WS-LOWER-CASE-TITLE TALLYING WS-COUNT
             FOR ALL 'mainframe'

             INSPECT WS-LOWER-CASE-TITLE TALLYING WS-COUNT
             FOR ALL 'mainframes'


              IF WS-COUNT > 0

      *        DISPLAY WS-HACK-IN-FILE-RECORD

              MOVE SPACES TO HACK-OUT-FIELDS

              PERFORM COMPUTE-RANKING-SCORE-PARA

                  MOVE WS-HACK-IN-POINTS TO WS-HACK-IN-POINTS-EDITED
                  MOVE WS-HACK-IN-COMMENTS TO WS-HACK-IN-COMMENTS-EDITED
      *

      *
                  MOVE WS-HACK-IN-ID TO HACK-OUT-ID
                  MOVE WS-HACK-IN-TITLE TO HACK-OUT-TITLE
                  MOVE WS-HACK-IN-POINTS-EDITED TO HACK-OUT-POINTS
                  MOVE WS-HACK-IN-COMMENTS-EDITED TO HACK-OUT-COMMENTS
                  MOVE WS-HACK-IN-AUTHOR TO HACK-OUT-AUTHOR
                  MOVE WS-WORK-TIME TO HACK-OUT-TIME
                ADD 1 TO WS-RECORD-COUNT
      *         WRITE HACK-OUT-FIELDS AFTER ADVANCING 1 LINE
                WRITE HACK-OUT-FIELDS
               END-IF
           END-READ
           MOVE ZERO TO WS-COUNT
           END-PERFORM.

       COMPUTE-RANKING-SCORE-PARA.

           DISPLAY 'I HAVE ARRIVED AT COMPUTE-RANKING-SCORE-PARA'

            UNSTRING WS-HACK-IN-CREATE-DT DELIMITED BY ' ' INTO
            WS-DATE
            WS-TIME
           END-UNSTRING
      *     DISPLAY WS-DATE
      *     DISPLAY WS-TIME
           MOVE SPACES TO WS-HH-STRING
           MOVE SPACES TO WS-MM-STRING
                UNSTRING WS-TIME DELIMITED BY ':' INTO
                WS-HH-STRING
                WS-MM-STRING
                END-UNSTRING
            COMPUTE WS-MM = FUNCTION NUMVAL(WS-MM-STRING)
            COMPUTE WS-HH = FUNCTION NUMVAL(WS-HH-STRING)
            DISPLAY WS-HH
            DISPLAY WS-MM


            COMPUTE WS-DECIMAL-HOURS = (WS-HH + WS-MM / 60).
            DISPLAY WS-DECIMAL-HOURS.
            COMPUTE WS-AGE-HOURS = 24 - WS-DECIMAL-HOURS.
            COMPUTE WS-NUMERATOR = (WS-HACK-IN-POINTS - 1) ** 0.8.
            DISPLAY WS-NUMERATOR.

            ADD 2 TO WS-AGE-HOURS.
            DISPLAY WS-AGE-HOURS.

            COMPUTE WS-DENOMINATOR = (WS-AGE-HOURS) ** 1.8.
            DISPLAY WS-DENOMINATOR.
            COMPUTE WS-HACK-OUT-RANKING-SCORE =
                                         WS-NUMERATOR / WS-DENOMINATOR.
           MOVE WS-HACK-OUT-RANKING-SCORE TO
           HACK-OUT-RANKING-SCORE-EDITED.

           STRING WS-HH DELIMITED BY SIZE ':' WS-MM DELIMITED BY SIZE
           INTO WS-WORK-TIME


           DISPLAY WS-WORK-TIME.




