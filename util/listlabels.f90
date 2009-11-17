!======================================================================
!======================================================================
!         Utility Program for Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM LISTLABELS
  USE UTILITY

  INTEGER, ALLOCATABLE :: LBR(:),LPT(:),LTY(:),LLB(:),LNL(:),LFR(:),LTO(:)
  TYPE(LABEL), POINTER :: LIST
!
  NULLIFY(LIST)
  CALL RDFILE8(LIST,NLB,5)
  ALLOCATE(LBR(NLB),LPT(NLB),LTY(NLB),LLB(NLB),LNL(NLB),LFR(NLB),LTO(NLB))
  CALL GTFILE8(LIST,NLB,LBR,LPT,LTY,LLB,LNL)
  CALL LISTLB(NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO,.FALSE.)
!
  STOP
END PROGRAM LISTLABELS
