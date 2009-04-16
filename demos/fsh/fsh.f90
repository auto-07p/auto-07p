!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   fsh :     Heteroclinic orbits : a saddle-node copnnection
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Parameter assignment:
!
!           PAR(1)           :                   (unused)
!           PAR(2)           : c                 (wave speed)
!           PAR(4)           : eps-1        1    (radius)
!           PAR(11)          : period
!           PAR(12)          : mu-1              (eigenvalue  at 1)
!           PAR(13) , PAR(14): v(1)    , v(2)    (eigenvector at 1)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!
       CALL FFFF(2,U,ICP,PAR,IJAC,F,DUMMY)
       PERIOD=PAR(11)
       DO I=1,NDIM
         F(I)=PERIOD*F(I)
       ENDDO
!
      RETURN
      END
!
      SUBROUTINE FFFF(NDM,U,ICP,PAR,IJAC,F,DFDU)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDM),PAR(*),F(NDM),DFDU(NDM,NDM)
!
       C=PAR(2)
       F(1)= U(2)
       F(2)= C*U(2) - U(1) * (1-U(1))
!
      IF(IJAC.EQ.0)RETURN
!
       DFDU(1,1)= 0
       DFDU(1,2)= 1
!
       DFDU(2,1)= -1 + 2*U(1)
       DFDU(2,2)= C
!
      RETURN
      END
!
      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /FRST/ IFRST
      DIMENSION U(NDIM),PAR(*)
!
      IF(IFRST.NE.1)THEN
        IFRST=1
!       Set the starting period, wave speed, and radius
        PERIOD=0.01
        C=11.
        EP1=0.001
        D = DSQRT(C**2+4)
        PAR(2)= C
        PAR(4)= EP1
        PAR(11)= PERIOD
        PAR(12)= (C-D)/2
        PAR(13) =    1./DSQRT(1+PAR(12)**2)
        PAR(14)=PAR(12)/DSQRT(1+PAR(12)**2)
       ENDIF
!
       C     =PAR(2)
       EP1   =PAR(4)
       PERIOD=PAR(11)
       D=DSQRT(C**2+4)
       RMU1= (C-D)/2
       V11 =  1./DSQRT(1+RMU1**2)
       V12 =RMU1/DSQRT(1+RMU1**2)
!
       U(1)=1-EP1*V11
       U(2)= -EP1*V12
!
      RETURN
      END
!
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
! Local
      PARAMETER (NDM=2)
      DIMENSION V1(NDM),G1(NDM),DGDU1(NDM,NDM)
!
      V1(1)=U1(1) + PAR(4)*PAR(13)
      V1(2)=U1(2) + PAR(4)*PAR(14)
!
      CALL FFFF(NDM,V1,ICP,PAR,1,G1,DGDU1)
!
      FB(1)= DGDU1(1,1)*PAR(13) + DGDU1(1,2)*PAR(14)- PAR(12)*PAR(13)
      FB(2)= DGDU1(2,1)*PAR(13) + DGDU1(2,2)*PAR(14)- PAR(12)*PAR(14)
      FB(3)= PAR(13)**2 + PAR(14)**2 -1
      FB(4)= G1(1)
      FB(5)= G1(2)
!
      RETURN
      END
!
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DIMENSION FI(NINT),ICP(*),PAR(*)
! Local
      PARAMETER (NDM=2)
      DIMENSION F(NDM),F0(NDM),DFDU(NDM,NDM)
!
      CALL FFFF(NDM,U   ,ICP,PAR,1,F ,DFDU)
      CALL FFFF(NDM,UOLD,ICP,PAR,0,F0,DUMMY)
!
       FI(1)= ( F(1) - F0(1) ) * ( DFDU(1,1)*F(1) + DFDU(1,2)*F(2) ) &
            + ( F(2) - F0(2) ) * ( DFDU(2,1)*F(1) + DFDU(2,2)*F(2) )
!
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
