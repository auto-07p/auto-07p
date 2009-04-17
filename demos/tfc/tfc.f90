!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   Demo tfc
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

   DOUBLE PRECISION t1,t2,t3,t4,t5,t7,t8,t9,t10,t12,t13,t15,t16,t17,t19
   DOUBLE PRECISION t22,t27,t28,t29,t33,t34,t36,t39
   DOUBLE PRECISION t42,t43,t44,t50,t51,t54,t56,t58,t64,t65,t66,t68,t69
   DOUBLE PRECISION t74,t86

   t1 = PAR(2)
   t2 = U(1)
   t3 = exp(t2)
   t4 = t1*t3
   t5 = t3**2
   t7 = PAR(1)
   t8 = U(2)
   t9 = exp(t8)
   t10 = t7*t9
   t12 = 1.D0+t4
   t13 = 1.D0/t12
   F(1) = -(-1.D0-t4+t3+t1*t5+t10)*t13
   t15 = t7*t3
   t16 = PAR(5)
   t17 = t16*t9
   t19 = PAR(3)
   t22 = t19*t1
   t27 = PAR(4)
   t28 = U(3)
   t29 = t27*t28
   t33 = 1.D0+t17
   t34 = 1.D0/t33
   F(2) = (t15+t15*t17-t19-t19*t16*t9-t22*t3-t22*t3*t16*t9-t29-t29*t4)*t13*t34
   t36 = PAR(6)
   t39 = t27*t9-t36-t36*t16*t9
   F(3) = t28*t39*t34

   IF(IJAC.EQ.0)RETURN

   t42 = t1**2
   t43 = 2.D0*t2
   t44 = exp(t43)
   t50 = t12**2
   t51 = 1.D0/t50
   DFDU(1,1) = t3*(-1.D0-2.D0*t4-t42*t44+t1*t7*t9)*t51
   DFDU(1,2) = -t10*t13
   DFDU(1,3) = 0.D0
   t54 = exp(t2+t8)
   t56 = t3+t16*t54
   t58 = t51*t34
   DFDU(2,1) = t7*t56*t58
   t64 = t33**2
   t65 = 1.D0/t64
   t66 = (t9+t1*t54)*t13*t65
   DFDU(2,2) = t16*t27*t28*t66
   DFDU(2,3) = -t27*t34
   DFDU(3,1) = 0.D0
   t68 = t28*t9
   t69 = t27*t65
   DFDU(3,2) = t68*t69
   DFDU(3,3) = t39*t34

   IF(IJAC.EQ.1)RETURN

   DFDP(1,1) = -t9*t13
   DFDP(1,2) = t54*t7*t51
   DFDP(1,3) = 0.D0
   DFDP(1,4) = 0.D0
   DFDP(1,5) = 0.D0
   DFDP(1,6) = 0.D0
   DFDP(2,1) = t56*t13*t34
   t74 = exp(t43+t8)
   DFDP(2,2) = -t7*(t44+t74*t16)*t58
   DFDP(2,3) = -(1.D0+t17+t4+t1*t16*t54)*t13*t34
   DFDP(2,4) = -t28*t34
   DFDP(2,5) = t29*t66
   DFDP(2,6) = 0.D0
   DFDP(3,1) = 0.D0
   DFDP(3,2) = 0.D0
   DFDP(3,3) = 0.D0
   DFDP(3,4) = t68*t34
   t86 = exp(2.D0*t8)
   DFDP(3,5) = -t28*t86*t69
   DFDP(3,6) = -t28

 END SUBROUTINE FUNC

 SUBROUTINE STPNT(NDIM,U,PAR,T)
!---------- -----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(IN) :: T
   DOUBLE PRECISION, INTENT(OUT) :: U(NDIM), PAR(*)

   PAR(1)=5.0         ! a_1
   PAR(2)=3.0         ! b_1
   PAR(3)=1.10830E+00 ! d_1
   PAR(4)=0.1         ! a_2
   PAR(5)=2.0         ! b_2
   PAR(6)=0.01        ! d_2

   U(1:3)=(/LOG(0.66163D0),LOG(0.20199D0),0.0D0/)

 END SUBROUTINE STPNT

 SUBROUTINE BCND
 END SUBROUTINE BCND

 SUBROUTINE ICND
 END SUBROUTINE ICND

 SUBROUTINE FOPT
 END SUBROUTINE FOPT

 SUBROUTINE PVLS(NDIM,U,PAR)
!---------- ---- 

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM
  DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
  DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
  DOUBLE PRECISION GETP

  PAR(7)=EXP(GETP('MIN',1,U))
  PAR(8)=EXP(GETP('MIN',2,U))

 END SUBROUTINE PVLS
