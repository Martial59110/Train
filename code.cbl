       IDENTIFICATION DIVISION.
       PROGRAM-ID. code.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F1 ASSIGN TO "train1.dat"
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL.
           SELECT F2 ASSIGN TO "train3.dat"
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD F1
       RECORDING MODE IS V
           RECORD IS VARYING IN SIZE FROM 27 TO 37 CHARACTERS DEPENDING
           ON VARIABLE01.
       COPY 'cop.cpy'. 
       FD F2
       RECORDING MODE IS V
           RECORD IS VARYING IN SIZE FROM 27 TO 39 CHARACTERS DEPENDING
           ON VARIABLE01.
         01 ALLY.
           03 RECORD-TYPE2       PIC XXX.
           88 TGV     VALUE 'TGV'.
           88 CORAIL  VALUE 'COR'.
           88 TER     VALUE 'TER'.
           03 STATION-DEPART2    PIC X(18).
           03 TRAIN-TIME2.
           05 TRAIN-TIME-HH2  PIC 99.
           05 TRAIN-TIME-MM2  PIC 99.
           03 TRAIN-NBRE-HEURES2 PIC 99.   
           03 TRAIN-HALTS2 PIC 99.
           03 TRAIN-HALT-FLAG2   PIC X OCCURS 10 TIMES.
           88 TRAIN-STOPS-HERE VALUE 'H'.
           88 TRAIN-SERVICE    VALUE 'S'.
           88 TRAIN-FRETE      VALUE 'F'.
       WORKING-STORAGE SECTION.
       01  COMPTEUR PIC 99 VALUE 0.
       01  COMPTEUR-H PIC 99.
       01  LONGUEUR PIC 99.
       01  HH PIC 99.
       01  MM PIC 99.
       01  TRAHET PIC 99.
       01  VARIABLE01 PIC 99.
       01  WS-IDX PIC 99.
       01  WS-TRAIN.
           02 ARRAY OCCURS 46 TIMES.
           03 WS-RECORDS PIC X(31).
           03 WS-ARRET PIC X(10).

       PROCEDURE DIVISION.
           OPEN INPUT F1
           OPEN OUTPUT F2.
    
           PERFORM UNTIL WS-IDX NOT LESS THAN 1
           READ F1 INTO TRAIN-PLANNING
            AT END
            EXIT PERFORM
            NOT AT END
            PERFORM PROCESS-FILE
            PERFORM PROCESS-RECORD
            END-READ
            
           END-PERFORM.
    
           CLOSE F1.
           

       PROCESS-FILE.
           
           MOVE TRAIN-PLANNING(1:27) TO WS-TRAIN(1:27)
            MOVE TRAIN-PLANNING(29:9) TO WS-TRAIN(29:9)
            INSPECT WS-TRAIN(29:9) TALLYING COMPTEUR-H FOR ALL "H"
            MOVE COMPTEUR-H TO WS-TRAIN(27:2)
            DISPLAY COMPTEUR-H.
            WRITE ALLY FROM WS-TRAIN
            SET COMPTEUR-H TO 0.
           
       PROCESS-RECORD.
           ADD 1 TO COMPTEUR.
           WRITE ALLY FROM COMPTEUR.


           DISPLAY "Le nombre total d'enregistrements est : " COMPTEUR.
           
           
          
          
          
           
           