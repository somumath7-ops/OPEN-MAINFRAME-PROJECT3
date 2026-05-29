       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNEMPKSD.
       AUTHOR. SOMESH MATH.

      *OMPCOB3 CHALLENGE 9.3
      *---------------------
      *CHALLENGE 9.3.1: Missouri Unemployment Claims by Age data
      *was loaded into a CSV file on the PC. This in turn was transferred
      *from local machine CSV to MVS PS by using the command IND$File in
      *the TSO command section.

      *CHALLENGE 9.3.2: The data in PS was first sorted using PGM=SORT
      *in JCL and loaded into A KSDS using IDCAMS.

      *MVS PS: Z84549.UNEMP.CLAIMS.BYAGE.PS
      *VSAM KSDS: Z84549.UNEMP.CLAIMS.BYAGE
      *JCL to load VSAM KSDS from PS: Z84549.JCL.LOADVSAM
      *
      *This program reads this VSAM KSDS and outputs in PRINTLINE SYSOUT
      *as a formatted report. This is a sequential read and display of
      *a VSAM KSDS file.

      *JCL to compile and run this program: Z84549.JCL.COBAGEJ

      *CHALLENGE 9.3.3: This challenge is to interactively inquire details
      *of a record by entering the RECORD ID. This is achiEved in the next
      *COBOL program: COBSUB1.




       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNEMP-CLAIMS-AGE ASSIGN TO UNEMPAGE
              ORGANIZATION IS INDEXED
              ACCESS MODE IS SEQUENTIAL
              RECORD KEY IS RECORD-ID
              FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  UNEMP-CLAIMS-AGE.
       01  UNEMP-CLAIMS-AGE-RECORD.
           05 RECORD-ID    PIC X(8).
           05 UNEMP-CLAIMS-AGE-DATA PIC X(72).


       WORKING-STORAGE SECTION.
       01  WS-UNEMP-CLAIMS-AGE-REC.
           05  WS-RECORD-ID       PIC X(8).
           05  WS-RECORD-DATA     PIC X(72).
           05  WS-STR1             PIC X(1).
           05  WS-RECORD-DATE     PIC X(10).
           05  WS-RECORD-INA      PIC 9(2).
           05  WS-AGE-0-22        PIC 9(5).
              05  WS-AGE-0-22-EDITED PIC ZZZ,ZZ9.
           05  WS-AGE-22-24       PIC 9(5).
              05  WS-AGE-22-24-EDITED PIC ZZZ,ZZ9.
           05  WS-AGE-25-34       PIC 9(5).
                05  WS-AGE-25-34-EDITED PIC ZZZ,ZZ9.
           05  WS-AGE-35-44       PIC 9(5).
                05  WS-AGE-35-44-EDITED PIC ZZZ,ZZ9.
           05  WS-AGE-45-54       PIC 9(5).
                05  WS-AGE-45-54-EDITED PIC ZZZ,ZZ9.
           05  WS-AGE-55-59       PIC 9(5).
                05  WS-AGE-55-59-EDITED PIC ZZZ,ZZ9.
           05  WS-AGE-60-64       PIC 9(5).
                05  WS-AGE-60-64-EDITED PIC ZZZ,ZZ9.
           05  WS-AGE-65-PLUS-STRING     PIC X(5).
           05  WS-AGE-65-PLUS     PIC 9(5).
                05  WS-AGE-65-PLUS-EDITED PIC ZZZ,ZZ9.


      *
       01  HEADER-1.
           05  FILLER         PIC X(25) VALUE SPACES.
           05  FILLER         PIC X(50) VALUE
           'Missouri Monthly Unemployment Claims by Age'.


      *
       01  HEADER-2.
            05  FILLER         PIC X(25) VALUE 'Source: Github'.
       01  HEADER-2A           PIC X(100) VALUE ' '.




      *
       01  HEADER-3.
           05  FILLER         PIC X(09) VALUE 'Record ID'.
           05  FILLER         PIC X(01) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '   Date '.
           05  FILLER         PIC X(01) VALUE SPACES.
           05  FILLER         PIC X(03) VALUE 'INA'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE 'AGE<22'.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '22-24'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '25-34'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '35-44'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '45-54'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '55-59'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '60-64'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'Over 65'.


       01  HEADER-4.
           05  FILLER         PIC X(09) VALUE '---------'.
           05  FILLER         PIC X(01) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '--------- '.
           05  FILLER         PIC X(01) VALUE SPACES.
           05  FILLER         PIC X(03) VALUE '---'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '------'.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '-----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '-----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '-----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '-----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE '-----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE '-----'.

         01  WS-WORK-AREA.
           05 WS-FILE-STATUS          PIC X(02).
           05  END-OF-VSAM-FILE      PIC XX VALUE 'N'.
              88  SUCCESSFUL-OPERATION  VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
                DISPLAY HEADER-1.
                DISPLAY HEADER-2.
           INSPECT HEADER-2A REPLACING ALL ' ' BY '-'.
                DISPLAY HEADER-2A.
                DISPLAY HEADER-3.
                DISPLAY HEADER-4.
           OPEN INPUT UNEMP-CLAIMS-AGE.
           PERFORM READ-AND-DISPLAY-PARA UNTIL END-OF-VSAM-FILE = 'Y'.
           CLOSE UNEMP-CLAIMS-AGE.
           DISPLAY 'FILE CLOSED. PROGRAM ENDED.'.
           STOP RUN.

       READ-AND-DISPLAY-PARA.
           READ UNEMP-CLAIMS-AGE NEXT RECORD
              AT END
                  MOVE 'Y' TO END-OF-VSAM-FILE
              NOT AT END
                  MOVE RECORD-ID TO WS-RECORD-ID
                  MOVE UNEMP-CLAIMS-AGE-DATA TO WS-RECORD-DATA
      *            DISPLAY WS-RECORD-ID, ":  ", WS-RECORD-DATA
               MOVE ZEROES TO WS-AGE-65-PLUS IN WS-UNEMP-CLAIMS-AGE-REC
                  UNSTRING WS-RECORD-DATA DELIMITED BY ','
                      INTO WS-STR1, WS-RECORD-DATE, WS-RECORD-INA,
                      WS-AGE-0-22, WS-AGE-22-24, WS-AGE-25-34,
                      WS-AGE-35-44, WS-AGE-45-54, WS-AGE-55-59,
                      WS-AGE-60-64, WS-AGE-65-PLUS-STRING
                  END-UNSTRING
                    MOVE WS-AGE-0-22 TO WS-AGE-0-22-EDITED
                    MOVE WS-AGE-22-24 TO WS-AGE-22-24-EDITED
                    MOVE WS-AGE-25-34 TO WS-AGE-25-34-EDITED
                    MOVE WS-AGE-35-44 TO WS-AGE-35-44-EDITED
                    MOVE WS-AGE-45-54 TO WS-AGE-45-54-EDITED
                    MOVE WS-AGE-55-59 TO WS-AGE-55-59-EDITED
                    MOVE WS-AGE-60-64 TO WS-AGE-60-64-EDITED
                    COMPUTE WS-AGE-65-PLUS = FUNCTION
                    NUMVAL(WS-AGE-65-PLUS-STRING)
                    MOVE WS-AGE-65-PLUS TO WS-AGE-65-PLUS-EDITED
                    DISPLAY WS-RECORD-ID, "  ",
                    WS-RECORD-DATE, "  ",
                            WS-RECORD-INA, "  ",
                            WS-AGE-0-22-EDITED, "  ",
                            WS-AGE-22-24-EDITED, "  ",
                            WS-AGE-25-34-EDITED, "  ",
                            WS-AGE-35-44-EDITED, "  ",
                            WS-AGE-45-54-EDITED, "  ",
                            WS-AGE-55-59-EDITED, "  ",
                            WS-AGE-60-64-EDITED, "  ",
                            WS-AGE-65-PLUS-EDITED
           END-READ.

