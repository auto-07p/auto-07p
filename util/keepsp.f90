!======================================================================
!======================================================================
! Utility Program for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM KEEPSP
  USE UTILITY
  LOGICAL, EXTERNAL :: DELETEFN
  CALL KEEPMAIN(DELETEFN)
END PROGRAM KEEPSP

LOGICAL FUNCTION DELETEFN(I,ITP)
  INTEGER I,ITP
  DELETEFN = (ITP==4 .OR. ITP==-4)
END FUNCTION DELETEFN
