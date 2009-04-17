!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!       ops :    Optimization of periodic solutions 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION x,y,z,p1,p2,p3,p4
      INTEGER I,J

       x=U(1)
       y=U(2)
       z=U(3)

       p1=PAR(1)
       p2=PAR(2)
       p3=PAR(3)
       p4=PAR(4)

       F(1)=( -p4*(x**3/3-x) + (z-x)/p2 - y ) / p1  
       F(2)=x-p3 
       F(3)=-(z-x)/p2 

      IF(IJAC.EQ.0)RETURN

       DFDU(1,1)=( -p4*(x**2-1) - 1/p2 ) /p1
       DFDU(1,2)=-1/p1
       DFDU(1,3)=1/(p2*p1)

       DFDU(2,1)=1
       DFDU(2,2)=0
       DFDU(2,3)=0

       DFDU(3,1)=1/p2
       DFDU(3,2)=0
       DFDU(3,3)=-1/p2

      IF(IJAC.EQ.1)RETURN

!      *Parameter derivatives
       DO I=1,3
         DO J=1,9
           DFDP(I,J)=0.d0
         ENDDO
       ENDDO

       DFDP(1,1)=-( -p4*(x**3/3-x) + (z-x)/p2 - y )/p1**2
       DFDP(1,2)=-(z-x)/(p2**2*p1)
       DFDP(1,3)=0
       DFDP(1,4)=-(x**3/3-x)/p1

       DFDP(2,1)=0
       DFDP(2,2)=0
       DFDP(2,3)=-1
       DFDP(2,4)=0

       DFDP(3,1)=0
       DFDP(3,2)=(z-x)/p2**2
       DFDP(3,3)=0
       DFDP(3,4)=0

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION p1,p2,p3,p4

      p1=0.5
      p2=4
      p3=0.9
      p4=2.

      U(1)=p3 
      U(2)=-p4*(p3**3/3-p3)
      U(3)=p3 

      PAR(1)=p1 
      PAR(2)=p2
      PAR(3)=p3 
      PAR(4)=p4

      END SUBROUTINE STPNT

      SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: FS
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM),DFDP(*)

      INTEGER I

       FS=PAR(3)

      IF(IJAC.EQ.0)RETURN

       DO I=1,NDIM
         DFDU(I)=0.d0
       ENDDO

      IF(IJAC.EQ.1)RETURN

!      *Parameter derivatives
       DO I=1,9
         DFDP(I)=0.d0
       ENDDO

       DFDP(3)=1.d0

      END SUBROUTINE FOPT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
