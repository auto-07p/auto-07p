C======================================================================
C======================================================================
C         Utility Program for Relabeling Labeled Solutions
C                        in AUTO97 Data Files
C======================================================================
C======================================================================
C
      PROGRAM LISTLABELS
      USE UTILITY

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
       CALL LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO,.FALSE.)
C
      STOP
      END
