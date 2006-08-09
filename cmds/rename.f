      PARAMETER (MXLN=25)
      CHARACTER*80 fn
C=============================================
10    READ(10,*,END=20)fn
      DO i=1,80
        IF(fn(i:i).EQ." ")THEN
          ln=i
          GOTO 11
        ENDIF
      ENDDO
11    CONTINUE
      WRITE(13,*)"mv ",fn(1:ln),"  c.",fn(3:ln)
      GOTO 10
C=============================================
20    READ(11,*,END=30)fn
      DO i=1,80
        IF(fn(i:i).EQ." ")THEN
          ln=i
          GOTO 21
        ENDIF
      ENDDO
21    CONTINUE
      WRITE(13,*)"mv ",fn(1:ln)," b.",fn(3:ln)
      GOTO 20
C=============================================
30    READ(12,*,END=99)fn
      DO i=1,80
        IF(fn(i:i).EQ." ")THEN
          ln=i
          GOTO 31
        ENDIF
      ENDDO
31    CONTINUE
      WRITE(13,*)"mv ",fn(1:ln)," s.",fn(3:ln)
      GOTO 30
C=============================================
99    STOP
      END