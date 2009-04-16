!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   pen :    Coupled pendula (or rotations in coupled Josephson junctions)
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!
       GAMMA = PAR(1)
       EPS   = PAR(2)
       RI    = PAR(3)
!
       PH1=U(1)
       PH2=U(2)
       PS1=U(3)
       PS2=U(4)
!
       F(1)= PS1
       F(2)= PS2
       F(3)= -EPS*PS1 - SIN(PH1) + RI + GAMMA*(PH2-PH1)
       F(4)= -EPS*PS2 - SIN(PH2) + RI + GAMMA*(PH1-PH2)
!
      RETURN
      END
!         
      SUBROUTINE STPNT(NDIM,U,PAR)
!     ---------- -----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
!
       GAMMA=0.175
       EPS=0.1
       RI=0.4
       PAR(1)=GAMMA
       PAR(2)=EPS
       PAR(3)=RI
!
! Set the actual period (since the data in pen.dat have scaled time variable)
         PAR(11)=1.5738797205
!
      RETURN
      END
!
      SUBROUTINE BCND
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
