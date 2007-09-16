!======================================================================
!======================================================================
!         Utility Program for Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM LISTLABELS
  USE UTILITY

  PARAMETER (MXLB=10000)
  CHARACTER*1 CMD
  DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
  DIMENSION LFR(MXLB),LTO(MXLB)
  LOGICAL CHCKLB
!
  OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
  OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
!
  CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
  CALL LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO,.FALSE.)
!
  STOP
END PROGRAM LISTLABELS
