!======================================================================
!======================================================================
!         Utility Program for Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM RELABELF
  USE UTILITY
  PARAMETER (MXLB=10000)
  CHARACTER*1 CMD
  DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
  DIMENSION LFR(MXLB),LTO(MXLB)
  CHARACTER*80 INB,OUTB,INS,OUTS

  N=COMMAND_ARGUMENT_COUNT()
  IF (N==4) THEN
     CALL GET_COMMAND_ARGUMENT(1,INB)
     CALL GET_COMMAND_ARGUMENT(2,INS)
     CALL GET_COMMAND_ARGUMENT(3,OUTB)
     CALL GET_COMMAND_ARGUMENT(4,OUTS)
  ELSE
     INB='fort.27'
     INS='fort.28'
     OUTB='fort.37'
     OUTS='fort.38'
  ENDIF
  OPEN(27,FILE=INB,STATUS='old',ACCESS='sequential')
  OPEN(28,FILE=INS,STATUS='old',ACCESS='sequential')
  OPEN(37,FILE=OUTB,STATUS='unknown',ACCESS='sequential')
  OPEN(38,FILE=OUTS,STATUS='unknown',ACCESS='sequential')

  CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,28)

  CALL RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO)
  CALL WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
  CALL WRFILE8(LNL)

  STOP
END PROGRAM RELABELF
!======================================================================
!======================================================================
