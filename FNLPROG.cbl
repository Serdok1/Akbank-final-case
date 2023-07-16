       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNLPROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP-FILE ASSIGN TO INPFILE
                             STATUS ST-INP-FILE.
           SELECT OUT-FILE   ASSIGN TO OUTFILE
                             STATUS ST-OUT-FILE.
       DATA DIVISION.
       FILE SECTION.
       FD  INP-FILE RECORDING MODE F.
         01  INP-REC.
           05 INP-FUNC          PIC X(1).
           05 INP-ID            PIC 9(5).
           05 INP-DVZ           PIC 9(3).
       FD  OUT-FILE RECORDING MODE F.
         01  OUT-REC.
           05 REC-ID-HEADER        PIC X(04).
           05 REC-ID-O             PIC 9(05).
           05 REC-DVZ-HEADER       PIC X(06).
           05 REC-DVZ-O            PIC 9(03).
           05 REC-RC-HEADER        PIC X(05).
           05 REC-RC-O             PIC 9(02).
           05 REC-NAMEF-HEADER     PIC X(11).
           05 REC-NAME-FROM-O      PIC X(15).
           05 REC-NAMET-HEADER     PIC X(11).
           05 REC-NAME-TO-O        PIC X(15).
           05 REC-SURNF-HEADER     PIC X(14).
           05 REC-SURNAME-FROM-O   PIC X(15).
           05 REC-SURNT-HEADER     PIC X(14).
           05 REC-SURNAME-TO-O     PIC X(15).
           05 REC-COMM-O           PIC X(50).

       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 WS-FINALSUB        PIC X(08)       VALUE 'FINALSUB'.
           05 ST-INP-FILE       PIC 9(2).
              88 INP-FILE-EOF                   VALUE 10.
              88 INP-SUCCESS                    VALUE 00 97.
           05 ST-OUT-FILE       PIC 9(2).
              88 OUT-SUCCESS                    VALUE 00 97.
           05 WS-ISLEM-TIPI     PIC 9(01).
              88 WS-ISLEM-TIPI-VALID  VALUE 1 THRU 9.
         01 WS-SUB-AREA.
           05 WS-SUB-FUNC       PIC 9(01).
              88 WS-FUNC-OPEN                VALUE 1.
              88 WS-FUNC-READ                VALUE 2.
              88 WS-FUNC-UPDATE              VALUE 3.
              88 WS-FUNC-WRITE               VALUE 4.
              88 WS-FUNC-DELETE              VALUE 8.
              88 WS-FUNC-CLOSE               VALUE 9.
           05 WS-SUB-ID         PIC 9(05).
           05 WS-SUB-DVZ        PIC 9(03).
           05 WS-SUB-RC         PIC 9(02).
           05 WS-SUB-DATA.
              10 WS-SUB-NAME-TO           PIC X(15).
              10 WS-SUB-NAME-FROM         PIC X(15).
              10 WS-SUB-SURNAME-TO        PIC X(15).
              10 WS-SUB-SURNAME-FROM      PIC X(15).
              10 WS-SUB-DATE              PIC 9(08).
              10 WS-SUB-BALANCE           PIC 9(15).
              10 WS-SUB-COMM              PIC X(50).
      *--------------------
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H150-CONTROL-INP UNTIL INP-FILE-EOF.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT INP-FILE.
            IF NOT INP-SUCCESS
              DISPLAY "UNABLE TO OPEN INP-FILE"
              PERFORM H999-PROGRAM-EXIT
           END-IF.
           OPEN OUTPUT OUT-FILE.
           IF NOT OUT-SUCCESS
              DISPLAY "UNABLE TO OPEN OUT-FILE"
              PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ INP-FILE.
       H100-END. EXIT.

       H150-CONTROL-INP.
           COMPUTE WS-ISLEM-TIPI = FUNCTION NUMVAL(INP-FUNC).
           IF NOT WS-ISLEM-TIPI-VALID
            MOVE 'PROCESS TYPE IS NOT VALID' TO WS-SUB-COMM
            PERFORM H160-HEADER-FILL
            WRITE OUT-REC
            ELSE
            DISPLAY 'PROCESSING...'
            MOVE INP-ID  TO WS-SUB-ID
            MOVE INP-DVZ TO WS-SUB-DVZ
            MOVE ZEROES  TO WS-SUB-RC
            MOVE SPACES  TO WS-SUB-DATA
            PERFORM H160-HEADER-FILL
            EVALUATE WS-ISLEM-TIPI
              WHEN 1
                PERFORM H200-OPEN-SUB
              WHEN 2
                PERFORM H210-READ-SUB
              WHEN 3
                PERFORM H220-UPDATE-SUB
              WHEN 4
                PERFORM H230-WRITE-SUB
              WHEN 8
                PERFORM H240-DELETE-SUB
              WHEN 9
                PERFORM H250-CLOSE-SUB
             END-EVALUATE
           END-IF.
           READ INP-FILE.
       H150-END. EXIT.

       H160-HEADER-FILL.
           MOVE 'ID: ' TO REC-ID-HEADER.
           MOVE ' DVZ: ' TO REC-DVZ-HEADER.
           MOVE ' RC: ' TO REC-RC-HEADER.
           MOVE ' OLD NAME: ' TO REC-NAMEF-HEADER.
           MOVE ' NEW NAME: ' TO REC-NAMET-HEADER.
           MOVE ' OLD SURNAME: ' TO REC-SURNF-HEADER.
           MOVE ' NEW SURNAME: ' TO REC-SURNT-HEADER.
       H160-END. EXIT.

       H200-OPEN-SUB.
           SET  WS-FUNC-OPEN TO TRUE.
           CALL WS-FINALSUB USING WS-SUB-AREA.
           MOVE WS-SUB-COMM TO REC-COMM-O.
           MOVE WS-SUB-RC TO REC-RC-O.
           MOVE WS-SUB-ID TO REC-ID-O.
           MOVE WS-SUB-DVZ TO REC-DVZ-O.
           MOVE WS-SUB-COMM TO REC-COMM-O.
           MOVE WS-SUB-RC TO REC-RC-O.
           MOVE WS-SUB-NAME-FROM TO REC-NAME-FROM-O.
           MOVE WS-SUB-NAME-TO TO REC-NAME-TO-O.
           MOVE WS-SUB-SURNAME-FROM TO REC-SURNAME-FROM-O.
           MOVE WS-SUB-SURNAME-TO TO REC-SURNAME-TO-O.
           WRITE OUT-REC.
       H200-END. EXIT.

       H210-READ-SUB.
           SET  WS-FUNC-READ TO TRUE.
           CALL WS-FINALSUB USING WS-SUB-AREA.
           MOVE WS-SUB-ID TO REC-ID-O.
           MOVE WS-SUB-DVZ TO REC-DVZ-O.
           MOVE WS-SUB-COMM TO REC-COMM-O.
           MOVE WS-SUB-RC TO REC-RC-O.
           MOVE WS-SUB-NAME-FROM TO REC-NAME-FROM-O.
           MOVE WS-SUB-NAME-TO TO REC-NAME-TO-O.
           MOVE WS-SUB-SURNAME-FROM TO REC-SURNAME-FROM-O.
           MOVE WS-SUB-SURNAME-TO TO REC-SURNAME-TO-O.
           WRITE OUT-REC.
       H210-END. EXIT.
     *
       H220-UPDATE-SUB.
           SET  WS-FUNC-UPDATE TO TRUE.
           CALL WS-FINALSUB USING WS-SUB-AREA.
           MOVE WS-SUB-ID TO REC-ID-O.
           MOVE WS-SUB-DVZ TO REC-DVZ-O.
           MOVE WS-SUB-COMM TO REC-COMM-O.
           MOVE WS-SUB-RC TO REC-RC-O.
           MOVE WS-SUB-NAME-FROM TO REC-NAME-FROM-O.
           MOVE WS-SUB-NAME-TO TO REC-NAME-TO-O.
           MOVE WS-SUB-SURNAME-FROM TO REC-SURNAME-FROM-O.
           MOVE WS-SUB-SURNAME-TO TO REC-SURNAME-TO-O.
           WRITE OUT-REC.
       H220-END. EXIT.
     *
       H230-WRITE-SUB.
           SET  WS-FUNC-WRITE TO TRUE.
           CALL WS-FINALSUB USING WS-SUB-AREA.
           MOVE WS-SUB-ID TO REC-ID-O.
           MOVE WS-SUB-DVZ TO REC-DVZ-O.
           MOVE WS-SUB-COMM TO REC-COMM-O.
           MOVE WS-SUB-RC TO REC-RC-O.
           MOVE WS-SUB-NAME-FROM TO REC-NAME-FROM-O.
           MOVE WS-SUB-NAME-TO TO REC-NAME-TO-O.
           MOVE WS-SUB-SURNAME-FROM TO REC-SURNAME-FROM-O.
           MOVE WS-SUB-SURNAME-TO TO REC-SURNAME-TO-O.
           WRITE OUT-REC.
       H230-END. EXIT.
     *
       H240-DELETE-SUB.
           SET  WS-FUNC-DELETE TO TRUE.
           CALL WS-FINALSUB USING WS-SUB-AREA.
           MOVE WS-SUB-ID TO REC-ID-O.
           MOVE WS-SUB-DVZ TO REC-DVZ-O.
           MOVE WS-SUB-COMM TO REC-COMM-O.
           MOVE WS-SUB-RC TO REC-RC-O.
           MOVE WS-SUB-NAME-FROM TO REC-NAME-FROM-O.
           MOVE WS-SUB-NAME-TO TO REC-NAME-TO-O.
           MOVE WS-SUB-SURNAME-FROM TO REC-SURNAME-FROM-O.
           MOVE WS-SUB-SURNAME-TO TO REC-SURNAME-TO-O.
           WRITE OUT-REC.
       H240-END. EXIT.
     *
       H250-CLOSE-SUB.
           SET  WS-FUNC-CLOSE TO TRUE.
           CALL WS-FINALSUB USING WS-SUB-AREA.
       H250-END. EXIT.
     *
       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           PERFORM H250-CLOSE-SUB.
           STOP RUN.
       H999-END. EXIT.

      *
