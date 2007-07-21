C======================================================================
C======================================================================
C Utility Program for Listing, Deleting, Relabeling Labeled Solutions
C                        in AUTO97 Data Files
C======================================================================
C======================================================================
C
      PROGRAM KEEPLP
      USE UTILITY
      LOGICAL, EXTERNAL :: DELETEFN
      CALL KEEPMAIN(DELETEFN)
      END

      LOGICAL FUNCTION DELETEFN(I,ITP)
      INTEGER I,ITP
      DELETEFN = (ITP/=2 .AND. ITP/=5)
      END FUNCTION DELETEFN
