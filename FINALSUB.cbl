       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALSUB.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS RANDOM
                             RECORD KEY IDX-KEY
                             STATUS ST-IDX-FILE.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
         01  IDX-REC.
           05 IDX-KEY.
              10 IDX-ID              PIC S9(05) COMP-3.
              10 IDX-DVZ             PIC S9(03) COMP.
           05 IDX-NAME               PIC X(15).
           05 IDX-SURNAME            PIC X(15).
           05 IDX-DATE               PIC S9(07) COMP-3.
           05 IDX-BALANCE            PIC S9(15) COMP-3.

       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 ST-IDX-FILE            PIC 9(02).
              88 IDX-SUCCESS                   VALUE 00 97.
           05 WS-INT-DATE            PIC 9(07).
           05 WS-GREG-DATE           PIC 9(08).
           05 WS-NEW-NAME            PIC X(15) VALUE 'S I N A        '.
           05 WS-NEW-SURNAME         PIC X(15) VALUE 'OZBAYRAM       '.
           05 WS-READ-VALID          PIC 9(01).
           05 WS-OPEN-VALID          PIC 9(01) VALUE 0.
           05 WS-INDEX-I             PIC 9(15).
           05 WS-INDEX-J             PIC 9(15).

        LINKAGE SECTION.
         01 LS-SUB-AREA.
           05 LS-SUB-FUNC            PIC 9(01).
              88 LS-FUNC-OPEN                VALUE 1.
              88 LS-FUNC-READ                VALUE 2.
              88 LS-FUNC-UPDATE              VALUE 3.
              88 LS-FUNC-WRITE               VALUE 4.
              88 LS-FUNC-DELETE              VALUE 8.
              88 LS-FUNC-CLOSE               VALUE 9.
           05 LS-SUB-ID              PIC 9(05).
           05 LS-SUB-DVZ             PIC 9(03).
           05 LS-SUB-RC              PIC 9(02).
           05 LS-SUB-DATA.
              10 LS-SUB-NAME-TO           PIC X(15).
              10 LS-SUB-NAME-FROM         PIC X(15).
              10 LS-SUB-SURNAME-TO        PIC X(15).
              10 LS-SUB-SURNAME-FROM      PIC X(15).
              10 LS-SUB-DATE              PIC 9(08).
              10 LS-SUB-BALANCE           PIC 9(15).
              10 LS-SUB-COMM              PIC X(50).
      *--------------------
       PROCEDURE DIVISION USING LS-SUB-AREA.
       0000-MAIN.
           EVALUATE LS-SUB-FUNC
               WHEN 1
                  PERFORM H110-FUNC-OPEN
               WHEN 2
                  PERFORM H120-FUNC-READ
                WHEN 3
                  PERFORM H130-FUNC-UPDATE
                WHEN 4
                  PERFORM H140-FUNC-WRITE
                WHEN 8
                  PERFORM H150-FUNC-DELETE
                WHEN 9
                  PERFORM H300-FUNC-CLOSE
           END-EVALUATE.
       0000-END. EXIT.

       H110-FUNC-OPEN.
           IF NOT WS-OPEN-VALID = 1
            PERFORM H200-OPEN-FILE
            IF WS-OPEN-VALID = 1
             MOVE 'FILE OPENED SUCCESSFULLY.' TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
            ELSE
             MOVE 'ERROR OCCURRED WHEN OPENING THE FILE.' TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
           ELSE
            MOVE 'FILE ALREADY OPEN.' TO LS-SUB-COMM
           END-IF.
           GOBACK.
       H110-END. EXIT.

       H120-FUNC-READ.
           IF NOT WS-OPEN-VALID = 1
            PERFORM H200-OPEN-FILE
            IF NOT WS-OPEN-VALID = 1
             MOVE 'ERROR OCCURRED WHILE OPENING THE FILE FOR READ.'
      -        TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
           END-IF.
           PERFORM H210-READ-FILE.
           IF WS-READ-VALID = 1
             MOVE 'FILE READ SUCCESSFULLY COMPLETED.' TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
           ELSE
             MOVE 'NO RECORD FOUND IN FILE.' TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
           END-IF.
           GOBACK.
       H120-END. EXIT.

       H130-FUNC-UPDATE.
           IF NOT WS-OPEN-VALID = 1
            PERFORM H200-OPEN-FILE
            IF NOT WS-OPEN-VALID = 1
             MOVE 'ERROR OCCURRED WHILE OPENING THE FILE FOR UPDATE.'
      -        TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
           END-IF.
            PERFORM H210-READ-FILE.
            IF WS-READ-VALID = 1
             PERFORM H131-UPDATE-NAME
             PERFORM H132-UPDATE-SURNAME
             PERFORM H133-REWRITE-CONTROL
            ELSE
             MOVE 'NO RECORD FOUND IN FILE.' TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
            END-IF.
            GOBACK.
       H130-END. EXIT.

       H131-UPDATE-NAME.
           COMPUTE WS-INDEX-I = 0
           COMPUTE WS-INDEX-J = 0
           MOVE IDX-NAME TO LS-SUB-NAME-FROM
           PERFORM UNTIL WS-INDEX-I > LENGTH OF IDX-NAME
            IF IDX-NAME(WS-INDEX-I:1) NOT = SPACE
               MOVE IDX-NAME(WS-INDEX-I:1)
      -         TO LS-SUB-NAME-TO(WS-INDEX-J:1)
               ADD 1 TO WS-INDEX-J
            END-IF
            ADD 1 TO WS-INDEX-I
           END-PERFORM.
           MOVE LS-SUB-NAME-TO TO IDX-NAME.
       H131-END. EXIT.

       H132-UPDATE-SURNAME.
           MOVE IDX-SURNAME TO LS-SUB-SURNAME-FROM
           INSPECT IDX-SURNAME REPLACING ALL 'E' BY 'I'
           INSPECT IDX-SURNAME REPLACING ALL 'E' BY 'I'
           MOVE IDX-SURNAME TO LS-SUB-SURNAME-TO.
       H132-END. EXIT.

       H133-REWRITE-CONTROL.
           REWRITE IDX-REC
           IF LS-SUB-NAME-FROM = LS-SUB-NAME-TO
             MOVE 'THERE IS NO SPACES IN THE NAME.' TO LS-SUB-COMM
           ELSE
             MOVE 'UPDATE SUCCESSFULLY COMPLETED.' TO LS-SUB-COMM
           END-IF.
       H133-END. EXIT.

       H140-FUNC-WRITE.
           IF NOT WS-OPEN-VALID = 1
            PERFORM H200-OPEN-FILE
            IF NOT WS-OPEN-VALID = 1
             MOVE 'ERROR OCCURRED WHILE OPENING THE FILE FOR WRITING.'
      -        TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
           END-IF.
            PERFORM H210-READ-FILE.
            IF WS-READ-VALID = 1
             MOVE IDX-ID TO LS-SUB-ID
             MOVE IDX-DVZ TO LS-SUB-DVZ
             MOVE ST-IDX-FILE TO LS-SUB-RC
             MOVE IDX-NAME TO LS-SUB-NAME-FROM
             MOVE WS-NEW-NAME TO LS-SUB-NAME-TO
             MOVE WS-NEW-NAME TO IDX-NAME
             MOVE IDX-SURNAME TO LS-SUB-SURNAME-FROM
             MOVE WS-NEW-SURNAME TO LS-SUB-SURNAME-TO
             MOVE WS-NEW-SURNAME TO IDX-SURNAME
             MOVE IDX-DATE TO LS-SUB-DATE
             MOVE IDX-BALANCE TO LS-SUB-BALANCE
             REWRITE IDX-REC
             MOVE 'WRITE SUCCESSFULLY COMPLETED.' TO LS-SUB-COMM
            ELSE
             MOVE 'NO RECORD FOUND IN FILE.' TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
            END-IF.
            GOBACK.
       H140-END. EXIT.

       H150-FUNC-DELETE.
           IF NOT WS-OPEN-VALID = 1
            PERFORM H200-OPEN-FILE
            IF NOT WS-OPEN-VALID = 1
             MOVE 'ERROR OCCURRED WHILE OPENING THE FILE FOR DELETE.'
      -        TO LS-SUB-COMM
             MOVE  ST-IDX-FILE TO LS-SUB-RC
           END-IF.
            PERFORM H210-READ-FILE.
            IF WS-READ-VALID = 1
             DELETE IDX-FILE
             MOVE 'RECORD SUCCESSFULLY DELETED.'
      -        TO LS-SUB-COMM
            END-IF.
            GOBACK.
       H150-END. EXIT.

       H200-OPEN-FILE.
           COMPUTE WS-OPEN-VALID = 0.
           DISPLAY 'OPENING...'
           OPEN I-O  IDX-FILE.
           IF IDX-SUCCESS
             COMPUTE WS-OPEN-VALID = 1
           END-IF.
       H200-END. EXIT.

       H210-READ-FILE.
           COMPUTE WS-READ-VALID = 0.
           MOVE LS-SUB-ID TO IDX-ID.
           MOVE LS-SUB-DVZ TO IDX-DVZ.
           READ IDX-FILE KEY IS IDX-KEY
           INVALID KEY PERFORM H220-INVALIDKEY
           NOT INVALID KEY PERFORM H230-VALIDREC.
       H210-END. EXIT.

       H220-INVALIDKEY.
           DISPLAY 'INVALID KEY :' IDX-ID IDX-DVZ.
       H220-END. EXIT.

       H230-VALIDREC.
           MOVE IDX-ID TO LS-SUB-ID
           MOVE IDX-DVZ TO LS-SUB-DVZ
           MOVE ST-IDX-FILE TO LS-SUB-RC.
           MOVE IDX-NAME TO LS-SUB-NAME-FROM
           MOVE SPACES TO LS-SUB-NAME-TO
           MOVE IDX-SURNAME TO LS-SUB-SURNAME-FROM
           MOVE SPACES TO LS-SUB-SURNAME-TO
           MOVE IDX-DATE TO LS-SUB-DATE
           MOVE IDX-BALANCE TO LS-SUB-BALANCE
           COMPUTE WS-READ-VALID = 1.
       H230-END. EXIT.
      *
       H300-FUNC-CLOSE.
           CLOSE IDX-FILE.
           MOVE ST-IDX-FILE TO LS-SUB-RC.
           MOVE 'FILE SUCCESSFULLY CLOSED.' TO LS-SUB-COMM.
           GOBACK.
       H300-END. EXIT.
