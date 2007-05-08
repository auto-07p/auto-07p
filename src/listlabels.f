C======================================================================
C======================================================================
C         Utility Program for Relabeling Labeled Solutions
C                        in AUTO97 Data Files
C======================================================================
C======================================================================
C
      PARAMETER (MXLB=10000)
      CHARACTER*1 CMD
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      LOGICAL CHCKLB
C
       OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
       OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
C
       CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
       CALL LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
C
      STOP
      END
C
C     ---------- ------
      SUBROUTINE RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      LOGICAL EOF
C
       REWIND 28
       NLB=0
 1     CONTINUE
         READ(28,*,END=2)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NSKIP
         IF(NLB.GE.MXLB)THEN
           WRITE(6,101)MXLB
           STOP
         ENDIF
         NLB=NLB+1
         LBR(NLB)=IABS(IBR)
         LPT(NLB)=IABS(NTOT)
	 IF(ITP.LT.0)THEN
           LTY(NLB)=-MOD(IABS(ITP),10)
	 ELSE
           LTY(NLB)= MOD(ITP,10)
	 ENDIF
         LLB(NLB)=LAB
         LNL(NLB)=LAB
         CALL SKIP(28,NSKIP,EOF)
         IF(EOF)GOTO 2
       GOTO 1
C
 101  FORMAT(' ERROR : Maximum number of labels (',I6,') exceeded.',
     *     /,' Increase MXLB in auto/97/src/relabel.f and recompile.')
C
 2    RETURN
      END
C
C     ------- -------- ------
      LOGICAL FUNCTION INLIST(MXLB,LAB,NL,LFR,LTO)
C
      DIMENSION LFR(MXLB),LTO(MXLB)
C
       INLIST=.FALSE.
       DO 1 I=1,NL
         IF(LAB.GE.LFR(I).AND.LAB.LE.LTO(I))THEN
           INLIST=.TRUE.
           RETURN
         ENDIF
 1     CONTINUE
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      CHARACTER*2 TYPE
      LOGICAL FIRST,INLIST
C
       IF(NLB.EQ.0)THEN
          WRITE(6,101)
       ELSE
         FIRST=.TRUE.
         DO 1 I=1,NLB
          IF(NL.EQ.0 .OR. INLIST(MXLB,LLB(I),NL,LFR,LTO))THEN
            IF(FIRST)THEN
              WRITE(6,102)
              FIRST=.FALSE.
            ENDIF
            WRITE(6,103)LBR(I),LPT(I),TYPE(LTY(I)),LLB(I)
          ENDIF
 1       CONTINUE
       ENDIF
C
 101   FORMAT(/,' Empty solutions file')
 102   FORMAT(/,'  BR    PT  TY LAB')
 103   FORMAT(I4,I6,2X,A2,I4,I5)
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE SKIP(IUNIT,NSKIP,EOF)
C
C Skips the specified number of lines on fort.IUNIT.
C
      LOGICAL EOF
C
       EOF=.FALSE.
       DO 1 I=1,NSKIP
         READ(IUNIT,*,END=2)
 1     CONTINUE
       RETURN
C
 2     CONTINUE
        EOF=.TRUE.
      RETURN
      END
C
C     ----------- -------- ----
      CHARACTER*2 FUNCTION TYPE(ITP)
C
       IF(MOD(ITP,10).EQ.1)THEN
         TYPE='BP'
       ELSE IF(MOD(ITP,10).EQ.2)THEN
         TYPE='LP'
       ELSE IF(MOD(ITP,10).EQ.3)THEN
         TYPE='HB'
       ELSE IF(MOD(ITP,10).EQ.4)THEN
         TYPE='  '
       ELSE IF(MOD(ITP,10).EQ.-4)THEN
         TYPE='UZ'
       ELSE IF(MOD(ITP,10).EQ.5)THEN
         TYPE='LP'
       ELSE IF(MOD(ITP,10).EQ.6)THEN
         TYPE='BP'
       ELSE IF(MOD(ITP,10).EQ.7)THEN
         TYPE='PD'
       ELSE IF(MOD(ITP,10).EQ.8)THEN
         TYPE='TR'
       ELSE IF(MOD(ITP,10).EQ.9)THEN
         TYPE='EP'
       ELSE IF(MOD(ITP,10).EQ.-9)THEN
         TYPE='MX'
       ELSE
         TYPE='  '
       ENDIF
C
      RETURN
      END
C======================================================================
C======================================================================
