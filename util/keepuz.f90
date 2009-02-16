!======================================================================
!======================================================================
! Utility Program for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM KEEPUZ
  USE UTILITY
  LOGICAL, EXTERNAL :: DELETEFN
  CALL KEEPMAIN(DELETEFN)
END PROGRAM KEEPUZ

LOGICAL FUNCTION DELETEFN(I,ITP)
  INTEGER I,ITP
  DELETEFN = ITP/=-4
END FUNCTION DELETEFN
