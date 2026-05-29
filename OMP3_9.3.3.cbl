       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBSUB1.
       AUTHOR. SOMESH MATH.

      *OMPCOB3 CHALLENGE 9.3.3

      *This program reads the unemployment claims by age data from a VSAM file
      *     based on the key received from a map in CICS region and then
      *     fetched record from a KSDS file for that record key field.

      *    BMS MAPSET: MAPSET3
      *    SOURCE LIBRARY: Z84549.MAPS.SRCLIB
      *    COPY LIBRARY: Z84549.MAPS.COPYLIB
      *    LOAD LIBRARY: &SYSUID..CICS.PROD.DFHLOAD

      *    JCL TO COMPILE BMS MAPSET: Z84549.JCL.COMPMAPS
      *
      *    VSAM KSDS: Z84549.UNEMP.CLAIMS.BYAGE
      *    CICS COPYBOOK:DFH620.CICS.SDFHCOB

      *    JCL TO COMPILE THIS PROGRAM: Z84549.JCL.CICSCOB

      *    SAMPLE VALID RECORD IDs to test: 10012011,10012012,10012013,
      *    10012014,10012015,10012017,10012018,10012020,10012021,10012022

       ENVIRONMENT DIVISION.
      * INPUT-OUTPUT SECTION.
      * FILE-CONTROL.
      *     SELECT UNEMP-CLAIMS-AGE ASSIGN TO UNEMPAGE
      *        ORGANIZATION IS INDEXED
      *        ACCESS MODE IS RANDOM
      *        RECORD KEY IS RECORD-ID
      *        FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
      * FD  UNEMP-CLAIMS-AGE.
      * 01  UNEMP-CLAIMS-AGE-RECORD.
      *     05 RECORD-ID    PIC X(8).
      *     05 UNEMP-CLAIMS-AGE-DATA PIC X(72).


       WORKING-STORAGE SECTION.

       01  WS-UNEMP-CLAIMS-AGE-RECORD.
           05  WS-RECORD-ID          PIC X(8).
           05  WS-RECORD-DATA        PIC X(72).

       01  WS-UNEMP-CLAIMS-AGE-REC.
           05  WS-RECORD-ID          PIC X(8).
           05  WS-STR1               PIC X(1).
           05  WS-RECORD-DATE        PIC X(10).
           05  WS-RECORD-INA         PIC 9(2).
           05  WS-AGE-0-22           PIC 9(5).
           05  WS-AGE-22-24          PIC 9(5).
           05  WS-AGE-25-34          PIC 9(5).
           05  WS-AGE-35-44          PIC 9(5).
           05  WS-AGE-45-54          PIC 9(5).
           05  WS-AGE-55-59          PIC 9(5).
           05  WS-AGE-60-64          PIC 9(5).
           05  WS-AGE-65-PLUS-STRING PIC X(5).
           05  WS-AGE-65-PLUS        PIC 9(5).


       01  WS-UNEMP-CLAIMS-EDITED-REC.
           05  WS-RECORD-ID-EDITED   PIC X(8).
           05  WS-RECORD-DATE-EDITED PIC X(10).
           05  WS-RECORD-INA-EDITED  PIC 9(2).
           05  WS-AGE-0-22-EDITED    PIC ZZZ,ZZ9.
           05  WS-AGE-22-24-EDITED   PIC ZZZ,ZZ9.
           05  WS-AGE-25-34-EDITED   PIC ZZZ,ZZ9.
           05  WS-AGE-35-44-EDITED   PIC ZZZ,ZZ9.
           05  WS-AGE-45-54-EDITED   PIC ZZZ,ZZ9.
           05  WS-AGE-55-59-EDITED   PIC ZZZ,ZZ9.
           05  WS-AGE-60-64-EDITED   PIC ZZZ,ZZ9.
           05  WS-AGE-65-PLUS-EDITED PIC ZZZ,ZZ9.



       01  WS-WORK-AREA.
           05 RESPONSE-CODE          PIC S9(08).
           05 END-OF-SESSION        PIC X(30) VALUE 'End of Session'.
           05 COMMUNICATION-AREA     PIC X.


       COPY MAPSET3.

       COPY DFHAID.


       LINKAGE SECTION.
       01  DFHCOMMAREA                  PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      *     PERFORM SEND-MAP-PARA.
      *     PERFORM RECEIVE-MAP-PARA.
           MOVE DFHCOMMAREA TO COMMUNICATION-AREA.

           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                 MOVE LOW-VALUES TO MAP3O
      *           MOVE -1 TO RECIDLL IN MAP3I
                 PERFORM SEND-MAP-PARA
               WHEN EIBAID = DFHENTER
                  PERFORM RECEIVE-MAP-AND-READ-PARA
               WHEN EIBAID = DFHPF3 OR DFHPF12
                  PERFORM CICS-RETURN-PARA
      *         WHEN DFHAID = DFHPF5
      *            MOVE LOW-VALUES TO MAP3O
      *            PERFORM SEND-MAP-PARA
               WHEN OTHER
                  MOVE 'Invalid Key Pressed' TO ERMSGVO
                  PERFORM SEND-DATA-TO-MAP-PARA
           END-EVALUATE.



      *     STOP RUN.


       SEND-MAP-PARA.
           MOVE LOW-VALUES TO MAP3O.
           EXEC CICS
              SEND MAP('MAP3')
              MAPSET('MAPSET3')
              CURSOR (503)
              ERASE
              END-EXEC.

      * RECEIVE-MAP-PARA.
      *        EXEC CICS
      *            RECEIVE MAP('MAP3')
      *            MAPSET('MAPSET3')
      *           INTO (MAP3I)
      *          END-EXEC.

       RECEIVE-MAP-AND-READ-PARA.
            EXEC CICS
              RECEIVE MAP('MAP3')
              MAPSET('MAPSET3')
                INTO (MAP3I)
            END-EXEC.
           MOVE RECIDVI TO WS-RECORD-ID OF WS-UNEMP-CLAIMS-AGE-RECORD.
           EXEC CICS
           READ DATASET ('UNEMPAGE')
              INTO (WS-UNEMP-CLAIMS-AGE-RECORD)
              RIDFLD (WS-RECORD-ID OF WS-UNEMP-CLAIMS-AGE-RECORD)
              RESP (RESPONSE-CODE)
           END-EXEC.
           EVALUATE RESPONSE-CODE
              WHEN DFHRESP(NORMAL)
                   DISPLAY 'ALL IS WELL'
                   PERFORM PROCESS-RECORD-PARA
                   PERFORM SEND-DATA-TO-MAP-PARA
              WHEN DFHRESP(NOTFND)
                   DISPLAY 'SOMETHING WENT WRONG'
                   MOVE LOW-VALUES TO MAP3O
                   MOVE 'Record not found' TO ERMSGVO
                   PERFORM SEND-DATA-TO-MAP-PARA
              WHEN OTHER
                   DISPLAY 'Error occurred while reading the file'
                   MOVE LOW-VALUES TO MAP3O
                   MOVE 'Error reading file' TO ERMSGVO
                   PERFORM SEND-DATA-TO-MAP-PARA
           END-EVALUATE.
      *     IF WS-FILE-STATUS IS EQUAL TO 'NORMAL'
      *          DISPLAY 'ALL IS WELL'
      *          PERFORM PROCESS-RECORD-PARA
      *          PERFORM SEND-DATA-TO-MAP-PARA
      *        ELSE
      *          DISPLAY 'SOMETHING WENT WRONG'
      *          MOVE LOW-VALUES TO MAP3O
      *          MOVE 'Record not found' TO ERMSGVO
      *          PERFORM SEND-DATA-TO-MAP-PARA
      *     END-IF.

      *     PERFORM PROCESS-RECORD-PARA
      *     PERFORM SEND-DATA-TO-MAP-PARA.

       CICS-RETURN-PARA.
              EXEC CICS
                SEND TEXT FROM  (END-OF-SESSION)
                ERASE
                FREEKB
                END-EXEC.
      *    TERMINATE THE CICS TRANSACTION
              EXEC CICS
                RETURN
              END-EXEC.

       PROCESS-RECORD-PARA.

               MOVE WS-RECORD-ID OF WS-UNEMP-CLAIMS-AGE-RECORD
                TO WS-RECORD-ID OF WS-UNEMP-CLAIMS-AGE-REC
               MOVE ZEROES TO WS-AGE-65-PLUS IN
                   WS-UNEMP-CLAIMS-AGE-REC
               UNSTRING WS-RECORD-DATA DELIMITED BY ','
                      INTO WS-STR1, WS-RECORD-DATE, WS-RECORD-INA,
                      WS-AGE-0-22, WS-AGE-22-24, WS-AGE-25-34,
                      WS-AGE-35-44, WS-AGE-45-54, WS-AGE-55-59,
                      WS-AGE-60-64, WS-AGE-65-PLUS-STRING
                  END-UNSTRING
                  INITIALIZE WS-UNEMP-CLAIMS-EDITED-REC REPLACING
                  NUMERIC DATA BY ZERO ALPHANUMERIC DATA BY SPACES
                    MOVE WS-RECORD-ID OF WS-UNEMP-CLAIMS-AGE-REC
                       TO WS-RECORD-ID-EDITED
                    MOVE WS-RECORD-DATE TO WS-RECORD-DATE-EDITED
                    MOVE WS-RECORD-INA TO WS-RECORD-INA-EDITED
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

      *    Fields in the edited record to be sent to map

                    MOVE WS-RECORD-ID-EDITED TO RECIDVO
                    MOVE WS-RECORD-DATE-EDITED TO DATEVO
                    MOVE WS-RECORD-INA-EDITED TO INAVO
                    MOVE WS-AGE-0-22-EDITED TO AGE22VO
                    MOVE WS-AGE-22-24-EDITED TO AGE24VO
                    MOVE WS-AGE-25-34-EDITED TO AGE34VO
                    MOVE WS-AGE-35-44-EDITED TO AGE44VO
                    MOVE WS-AGE-45-54-EDITED TO AGE54VO
                    MOVE WS-AGE-55-59-EDITED TO AGE59VO
                    MOVE WS-AGE-60-64-EDITED TO AGE64VO
                    MOVE WS-AGE-65-PLUS-EDITED TO AGE65VO.



       SEND-DATA-TO-MAP-PARA.
              EXEC CICS
                 SEND MAP('MAP3')
                 MAPSET('MAPSET3')
                 FROM (MAP3O)
              END-EXEC.




