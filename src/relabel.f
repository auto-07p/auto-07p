C======================================================================
C======================================================================
C         Utility Program for Relabeling Labeled Solutions
C                        in AUTO97 Data Files
C======================================================================
C======================================================================
C
      PROGRAM RELABELF
      USE UTILITY
      PARAMETER (MXLB=10000)
      CHARACTER*1 CMD
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
C
       OPEN(27,FILE='fort.27',STATUS='old',ACCESS='sequential')
       OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
       OPEN(37,FILE='fort.37',STATUS='unknown',ACCESS='sequential')
       OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
C
       CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
       CALL RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO)
       CALL WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
       CALL WRFILE8(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
      STOP
      END
C======================================================================
C======================================================================
