!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   Example 3: The A->B->C chemical reaction
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
!----------------------------------------------------------------------

 SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!---------- ---- 

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
   DOUBLE PRECISION, INTENT(OUT) :: F(NDIM), DFDU(NDIM,*), DFDP(NDIM,*)

   DOUBLE PRECISION x1,x2,x3,d,alpha,beta,b,s,e,x1c

   x1 = U(1)
   x2 = U(2)
   x3 = U(3)

   d = PAR(1)
   alpha = PAR(2)
   beta = PAR(3)
   b = PAR(4)
   s = PAR(5)

   e = dexp(X3)
   x1c = 1 - X1

   F(1) = -x1 + d*x1c*e
   F(2) = -x2 + d*e*(x1c - s*x2)
   F(3) = -x3 - beta*x3 + d*b*e*(x1c + alpha*s*x2)

   IF(IJAC.EQ.0)RETURN 

   DFDU(1,1) = -1.D0 - d*e
   DFDU(1,2) = 0.D0
   DFDU(1,3) = d*x1c*e

   DFDU(2,1) = -d*e
   DFDU(2,2) = -1.D0 - d*e*s
   DFDU(2,3) = d*e*(x1c - s*x2)

   DFDU(3,1) = -d*b*e
   DFDU(3,2) = d*b*e*alpha*s
   DFDU(3,3) = -1.D0 - beta + d*b*e*(x1c + alpha*s*x2)

   IF(IJAC.EQ.1)RETURN

   DFDP(1,1) = x1c*e
   DFDP(1,2) = 0.D0
   DFDP(1,3) = 0.D0
   DFDP(1,4) = 0.D0
   DFDP(1,5) = 0.D0
   DFDP(2,1) = e*(x1c - s*x2)
   DFDP(2,2) = 0.D0
   DFDP(2,3) = 0.D0
   DFDP(2,4) = 0.D0
   DFDP(2,5) = -d*e*x2
   DFDP(3,1) = b*e*(x1c + alpha*s*x2)
   DFDP(3,2) = d*b*e*s*x2
   DFDP(3,3) = -x3
   DFDP(3,4) = d*e*(x1c + alpha*s*x2)
   DFDP(3,5) = d*b*e*alpha*x2

 END SUBROUTINE FUNC

 SUBROUTINE STPNT(NDIM,U,PAR,T) 
!---------- ----- 

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(IN) :: T
   DOUBLE PRECISION, INTENT(OUT) :: U(NDIM), PAR(*)

   PAR(1:5)=(/0.0,1.0,1.55,8.,0.04/)

   U(1:3)=0.

 END SUBROUTINE STPNT

 SUBROUTINE BCND 
 END SUBROUTINE BCND

 SUBROUTINE ICND 
 END SUBROUTINE ICND

 SUBROUTINE FOPT 
 END SUBROUTINE FOPT

 SUBROUTINE PVLS
 END SUBROUTINE PVLS
