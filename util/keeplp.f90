!======================================================================
!======================================================================
! Utility Program for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM KEEPLP
  USE UTILITY
  LOGICAL, EXTERNAL :: DELETEFN
  CALL KEEPMAIN(DELETEFN)
END PROGRAM KEEPLP

LOGICAL FUNCTION DELETEFN(I,ITP)
  INTEGER I,ITP
  DELETEFN = (ITP/=2 .AND. ITP/=5)
END FUNCTION DELETEFN
