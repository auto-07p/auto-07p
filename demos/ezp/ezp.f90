!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   ezp :    Complex bifurcation in a boundary value problem
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----
!
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 PAR(*)
      DIMENSION U(NDIM),F(NDIM)
!
       U1=U(1)
       U2=U(2)
!
       RL=PAR(1)
!
       E=CDEXP(U1)
       F(1)=U2
       F(2)=-RL*E
!
      RETURN
      END
!
      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----
!
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 PAR(*)
      DIMENSION U(NDIM)
!
       U(1)=0
       U(2)=0
!
      RETURN
      END
!
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!     ---------- ----
!
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 PAR(*)
      DIMENSION ICP(*),U0(NDIM),U1(NDIM),F(NBC)
!
       F(1)=U0(1)
       F(2)=U1(1)
!
      RETURN
      END
!
      SUBROUTINE ICND
      RETURN
      END
!
      SUBROUTINE FOPT
      RETURN
      END
! 
      SUBROUTINE PVLS
      RETURN 
      END 
