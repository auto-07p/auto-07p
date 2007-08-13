!======================================================================
!======================================================================
! Utility Program for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM DELETELP
  USE UTILITY
  LOGICAL, EXTERNAL :: DELETEFN
  CALL KEEPMAIN(DELETEFN)
END PROGRAM DELETELP

LOGICAL FUNCTION DELETEFN(I,ITP)
  INTEGER I,ITP
  DELETEFN = (ITP==2 .OR. ITP==5)
END FUNCTION DELETEFN

