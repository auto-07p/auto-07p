!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   tim :    A test problem for timing AUTO
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!
       NDIM2=NDIM/2
       DO I=1,NDIM2
         I1=2*(I-1)+1
         I2=I1+1
         E=FEXP(U(I1))
         F(I1)=U(I2)
         F(I2)=-PAR(1)*E
       ENDDO
!
      RETURN
      END
!----------------------------------------------------------------------
!
      DOUBLE PRECISION FUNCTION FEXP(U)
!     ------ --------- -------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
       NTERMS=25
       FEXP=1.d0
       TRM=FEXP
       DO K=1,NTERMS
        TRM=TRM*U/K
        FEXP=FEXP + TRM
       ENDDO
!
      RETURN
      END
!----------------------------------------------------------------------
!
      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
!
      DO I=1,NDIM
        U(I)=0.0
      ENDDO
!
      RETURN
      END
!----------------------------------------------------------------------
!
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
!
       NDIM2=NDIM/2
       DO I=1,NDIM2
         I1=2*(I-1)+1
         I2=I1+1
         FB(I1)=U0(I1)
         FB(I2)=U1(I1)
       ENDDO
!
      RETURN
      END
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
