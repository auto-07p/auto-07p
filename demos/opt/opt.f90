!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   opt :    A model algebraic optimization problem
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION X1,X2,X3,X4,X5

       X1=U(1) 
       X2=PAR(1) 
       X3=PAR(2) 
       X4=PAR(3) 
       X5=PAR(4) 

       F(1)=X1*X1 + X2*X2 + X3*X3 + X4*X4 + X5*X5 - 1 

      IF(IJAC.EQ.0)RETURN 

       DFDU(1,1)=2*X1 

      IF(IJAC.EQ.1)RETURN 

!      *Parameter derivatives
       DFDP(1,1)=2*X2 
       DFDP(1,2)=2*X3 
       DFDP(1,3)=2*X4 
       DFDP(1,4)=2*X5 

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION X1,X2,X3,X4,X5

       X1=1.0 
       X2=0.0 
       X3=0.0 
       X4=0.0 
       X5=0.0 

       U(1)=X1 

       PAR(1)=X2 
       PAR(2)=X3 
       PAR(3)=X4 
       PAR(4)=X5 

      END SUBROUTINE STPNT

      SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: FS
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM),DFDP(*)

      DOUBLE PRECISION X1,X2,X3,X4,X5

       X1=U(1) 
       X2=PAR(1) 
       X3=PAR(2) 
       X4=PAR(3) 
       X5=PAR(4) 

       FS=X1 + X2 + X3 + X4 + X5 

      IF(IJAC.EQ.0)RETURN 

       DFDU(1)=1.0 

      IF(IJAC.EQ.1)RETURN 

!      *Parameter derivatives
       DFDP(1)=1.0 
       DFDP(2)=1.0 
       DFDP(3)=1.0 
       DFDP(4)=1.0 

      END SUBROUTINE FOPT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
