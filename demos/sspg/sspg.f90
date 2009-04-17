!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   Demo sspg: Size-structured plant growth
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
!----67--0---------0---------3---------4---------5---------6---------7---

 SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!--------- ---- 

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
   DOUBLE PRECISION, INTENT(OUT) :: F(NDIM), DFDU(NDIM,*), DFDP(NDIM,*)

   DOUBLE PRECISION m,s

!  Parameters
   m=PAR(1)
   s=PAR(2)

!  Equations
   F(1) = -U(2)
   F(2) = -U(1)*U(2)-m*U(2)

   IF(IJAC.EQ.0)RETURN 

   DFDU(1,1) = 0.0
   DFDU(1,2) = -1.0
   DFDU(2,1) = -U(2)
   DFDU(2,2) = -U(1)-m

   IF(IJAC.EQ.1)RETURN 

   DFDP(1,1) = 0.0
   DFDP(1,2) = 0.0
   DFDP(2,1) = -U(2)
   DFDP(2,2) = 0.0

 END SUBROUTINE FUNC

 SUBROUTINE STPNT(NDIM,U,PAR,T) 
!---------- ----- 

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(IN) :: T
   DOUBLE PRECISION, INTENT(OUT) :: U(NDIM), PAR(*)

!  Parameters initialization
   PAR(1:2)=(/1.0,4.0/)

!  Trivial solution
   U(1:2)=0.0

 END SUBROUTINE STPNT

 SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!---------- ---- 

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
   DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
   DOUBLE PRECISION, INTENT(OUT) :: FB(NBC), DBC(NBC,*)

   DOUBLE PRECISION m,s

!  Parameters
   m=PAR(1)
   s=PAR(2)

!  Equations
   FB(1) = U1(1)
   FB(2) = -U0(2)+s*U0(1)

   IF(IJAC.EQ.0)RETURN 

   DBC(1,1) = 0.0
   DBC(1,2) = 0.0
   DBC(1,3) = 1.0
   DBC(1,4) = 0.0
   DBC(2,1) = s
   DBC(2,2) = -1.0
   DBC(2,3) = 0.0
   DBC(2,4) = 0.0

   IF(IJAC.EQ.1)RETURN 

   DBC(1,5) = 0.0
   DBC(1,6) = 0.0
   DBC(2,5) = 0.0
   DBC(2,6) = U0(1)

 END SUBROUTINE BCND

 SUBROUTINE ICND
 END SUBROUTINE ICND

 SUBROUTINE FOPT 
 END SUBROUTINE FOPT

 SUBROUTINE PVLS
 END SUBROUTINE PVLS
