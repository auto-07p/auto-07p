!======================================================================
!======================================================================
! Utility Program for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM KEEPBP
  USE UTILITY
  LOGICAL, EXTERNAL :: DELETEFN
  CALL KEEPMAIN(DELETEFN)
END PROGRAM KEEPBP

LOGICAL FUNCTION DELETEFN(I,ITP)
  INTEGER I,ITP
  DELETEFN = (ITP/=1 .AND. ITP/=6)
END FUNCTION DELETEFN
