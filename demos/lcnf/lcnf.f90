!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   Demo LCNF: The limit cycle BP normal form
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

   DOUBLE PRECISION pi

   pi=4*ATAN(1.d0)

! Equations
   F(1)=-U(1)*(U(1)**2+U(2)**2-(PAR(1)+PAR(2))*               &
        SQRT(U(1)**2+U(2)**2)+PAR(1)*PAR(2)+PAR(3))-2*pi*U(2)
   F(2)=-U(2)*(U(1)**2+U(2)**2-(PAR(1)+PAR(2))*               &
        SQRT(U(1)**2+U(2)**2)+PAR(1)*PAR(2)+PAR(3))+2*pi*U(1)

   IF(IJAC.EQ.0)RETURN

   DFDU(1,1)=-(U(1)**2+U(2)**2-(PAR(1)+PAR(2))*               &
        SQRT(U(1)**2+U(2)**2)+PAR(1)*PAR(2)+PAR(3))-U(1)*     &
        (2*U(1)-U(1)*(PAR(1)+PAR(2))/SQRT(U(1)**2+U(2)**2))
   DFDU(1,2)=-U(1)*(2*U(2)-U(2)*(PAR(1)+PAR(2))/              &
        SQRT(U(1)**2+U(2)**2))-2*pi
   DFDU(2,1)=-U(2)*(2*U(1)-U(1)*(PAR(1)+PAR(2))/              &
        SQRT(U(1)**2+U(2)**2))+2*pi
   DFDU(2,2)=-(U(1)**2+U(2)**2-(PAR(1)+PAR(2))*               &
        SQRT(U(1)**2+U(2)**2)+PAR(1)*PAR(2)+PAR(3))-U(2)*     &
        (2*U(2)-U(2)*(PAR(1)+PAR(2))/SQRT(U(1)**2+U(2)**2))

   IF(IJAC.EQ.1)RETURN

   DFDP(1,1)=-U(1)*(-SQRT(U(1)**2+U(2)**2)+PAR(2))
   DFDP(1,2)=-U(1)*(-SQRT(U(1)**2+U(2)**2)+PAR(1))
   DFDP(1,3)=-U(1)
   DFDP(2,1)=-U(2)*(-SQRT(U(1)**2+U(2)**2)+PAR(2))
   DFDP(2,2)=-U(2)*(-SQRT(U(1)**2+U(2)**2)+PAR(1))
   DFDP(2,3)=-U(2)

 END SUBROUTINE FUNC

 SUBROUTINE STPNT(NDIM,U,PAR,T)
!---------- -----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(IN) :: T
   DOUBLE PRECISION, INTENT(OUT) :: U(NDIM), PAR(*)

   DOUBLE PRECISION pi

   pi=4*ATAN(1.d0)

!  Parameters initialization
   PAR(1:3)=(/1.0,2.0,0.0/)
   PAR(11)=1.0

   U(1)=PAR(1)*COS(2*pi*T)
   U(2)=PAR(1)*SIN(2*pi*T)

 END SUBROUTINE STPNT

 SUBROUTINE BCND
 END SUBROUTINE BCND

 SUBROUTINE ICND
 END SUBROUTINE ICND

 SUBROUTINE FOPT
 END SUBROUTINE FOPT

 SUBROUTINE PVLS
 END SUBROUTINE PVLS
