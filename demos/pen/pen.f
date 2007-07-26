C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C   pen :    Coupled pendula (or rotations in coupled Josephson junctions)
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
       GAMMA = PAR(1)
       EPS   = PAR(2)
       RI    = PAR(3)
C
       PH1=U(1)
       PH2=U(2)
       PS1=U(3)
       PS2=U(4)
C
       F(1)= PS1
       F(2)= PS2
       F(3)= -EPS*PS1 - SIN(PH1) + RI + GAMMA*(PH2-PH1)
       F(4)= -EPS*PS2 - SIN(PH2) + RI + GAMMA*(PH1-PH2)
C
      RETURN
      END
C         
      SUBROUTINE STPNT(NDIM,U,PAR)
C     ---------- -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
C
       GAMMA=0.175
       EPS=0.1
       RI=0.4
       PAR(1)=GAMMA
       PAR(2)=EPS
       PAR(3)=RI
C
C Set the actual period (since the data in pen.dat have scaled time variable)
         PAR(11)=1.5738797205
C
      RETURN
      END
C
      SUBROUTINE BCND
      RETURN
      END
C
      SUBROUTINE ICND
      RETURN
      END
C
      SUBROUTINE FOPT
      RETURN
      END
C 
      SUBROUTINE PVLS
      RETURN 
      END 
