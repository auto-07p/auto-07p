!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   dd2 :    Basic computations for discrete dynamical systems
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

       F(1)=PAR(1)*U(1)*(1-U(1)) - PAR(2)*U(1)*U(2) 
       F(2)=(1-PAR(3))*U(2) + PAR(2)*U(1)*U(2) 

      IF(IJAC.EQ.0)RETURN 

       DFDU(1,1)=PAR(1)*(1-2*U(1))-PAR(2)*U(2) 
       DFDU(1,2)=-PAR(2)*U(1) 
       DFDU(2,1)=PAR(2)*U(2) 
       DFDU(2,2)=1-PAR(3) + PAR(2)*U(1) 

      IF(IJAC.EQ.1)RETURN 

       DFDP(1,1)=U(1)*(1-U(1)) 
       DFDP(2,1)=0.0 
       DFDP(1,2)=-U(1)*U(2) 
       DFDP(2,2)= U(1)*U(2) 

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=0.0 
       PAR(2)=0.2 
       PAR(3)=0.1 

       U(1)=0.0 
       U(2)=0.0 

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
