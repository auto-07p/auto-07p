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
  CALL RDFILE8(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,5)
  CALL LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO,.FALSE.)
!
  STOP
END PROGRAM LISTLABELS
