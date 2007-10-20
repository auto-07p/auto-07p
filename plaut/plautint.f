C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Interface Subroutines for Calls to External Graphics Package
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
       SUBROUTINE DFREV
C      DEFINES THE SCREEN SIZE OF THE GRAPHICS PACKAGE
       COMMON /REVTN/ P10DX,P10DY,P10DXY,PDX,PDY,PDXY
C
         P10DX  =1023 
         P10DY  = 780
         P10DXY = SQRT(P10DX**2 + P10DY**2)
C
        RETURN
        END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   CALLS PLOTTING PACKAGE ROUTINES
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
       SUBROUTINE PLCMDS(ICMDNO)
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       INTEGER NVX(2),SPLPT
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /IO/ IWRITE,ITERM,ISAVE
       COMMON /NORPAK/ NXOLD,NYOLD,NXNEW,NYNEW
C
       IF(ICMDNO.GT.10)RETURN
       GO TO (1,2,3,4,5,6,7,8,9,10) ICMDNO

 1     CALL INITT(120,5,IWRITE,0,0)
       RETURN
 2     CALL MOVEA(IX,IY)
       RETURN
 3     CALL DRAWA(IX,IY)
       RETURN
 4     CALL WINCT(X,Y,X1,Y1)
       CALL MOVEA(X1,Y1)
       RETURN
 5     CALL WINCT(X,Y,X1,Y1)
       CALL DRAWA(X1,Y1)
       RETURN
 6     CALL REVCT(IX,IY,X,Y)
       RETURN
 7     CALL WINCT(X,Y,IX,IY)
       RETURN
 8     CALL ANMODE
       RETURN
 9     CALL HOME
       RETURN
 10    CALL WINCT(X,Y,X1,Y1)
       CALL POINTA(X1,Y1)
       RETURN
       END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      PRINTS TITLE AND COORDINATE NUMBERS
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
       SUBROUTINE PLCTNM(ICODE)
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       CHARACTER*1 CHONE,CHTWO*2,CHFIF*15,ADJ*80
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /IO/ IWRITE,ITERM,ISAVE
       COMMON /XYLAB/ XYNUM
       COMMON /PTITLE/ ADJ,NCHR
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
C
        CALL PLCMDS(2)
        CALL PLCMDS(8)
        IF (ICODE.EQ.1) THEN
          IF (NCHR.EQ.30) THEN
            WRITE(IWRITE,1) (ADJ(I:I),I=1,NCHR)
          ELSE
            WRITE(IWRITE,2) (ADJ(I:I),I=1,NCHR)
          END IF
        ELSE IF (ICODE.EQ.22) THEN
          IF (INDXCH.EQ.1) THEN
            WRITE(IWRITE,3) CHONE
          ELSE
            WRITE(IWRITE,4) CHTWO
          END IF
        ELSE IF (ICODE.EQ.23) THEN
          WRITE(IWRITE,5) (CHFIF(I:I),I=1,INDXCH)
       END IF
  1     FORMAT(30A1,$)
  2     FORMAT(60A1,$)
  3     FORMAT(A1,$)
  4     FORMAT(A2,$)
  5     FORMAT(15A1,$)
C
       RETURN
       END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
       SUBROUTINE PLCHDW(ICODE)   
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
       INTEGER ATNO,ADDO,EO,CRO,ATNOF,ADDOF,EOF,CROF,IESC(12)
       COMMON /IO/ IWRITE,ITERM,ISAVE
       DATA ATNO,ADDO,EO,CRO /27,65,69,13/
       DATA ATNOF,ADDOF,EOF,CROF /27,65,70,13/
       DATA IESC /27,60,27,91,48,59,48,72,27,91,50,74/
C
C----------------------------------------------------------------------
C      THIS BLOCK FOR PLOT-10 PLOTTER AND TO CLEAR ALFA MODE
C----------------------------------------------------------------------
C
        GO TO (1,2,3) ICODE
C                                     *TURN ON PLOTTER
  1     WRITE(ITERM,4) ATNO,ADDO,EO,CRO
        RETURN
C                                     *TURN OFF PLOTTER
  2     WRITE(ITERM,4) ATNOF,ADDOF,EOF,CROF
        RETURN
C                                     *CLEAR ALFA MODE
  3     CONTINUE
C      
  4     FORMAT(1X,4A1)
C
       RETURN
       END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      PRINTS STRING CHARACTERS ON SCREEN
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
       SUBROUTINE PLCCAL(ICODE)
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       CHARACTER*1 CHONE,CHTWO*2,CHFIF*15,ADJ*80
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /PTITLE/ ADJ,NCHR
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
       COMMON /REVTN/ P10DX,P10DY,P10DXY,PDX,PDY,PDXY
C       RETURN
       END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
