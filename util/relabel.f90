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

  OPEN(27,FILE='fort.27',STATUS='old',ACCESS='sequential')
  OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
  OPEN(37,FILE='fort.37',STATUS='unknown',ACCESS='sequential')
  OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')

  CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)

  CALL RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO)
  CALL WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
  CALL WRFILE8(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)

  STOP
END PROGRAM RELABELF
!======================================================================
!======================================================================
