C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C        Subroutines for Homoclinic Bifurcation Analysis
C       (A. R. Champneys, Yu. A. Kuznetsov, B. Sandstede,
C        B. E. Oldeman, E. J. Doedel)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for homoclinic bifurcation analysis
C
      DIMENSION IAP(*),ICP(*)
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
C Generate the function.
C
      IF(IJAC.EQ.0)THEN
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
        RETURN
      ENDIF
C
C Generate the Jacobian.
C
      UMX=0.d0
      DO I=1,NDIM
        IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
      ENDDO
C
      EP=HMACH*(1+UMX)
C
      DO I=1,NDIM
        UU=U(I)
        U(I)=UU-EP
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,DFDU(1,I),NDM,DFU)
        U(I)=UU+EP
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
        U(I)=UU
        DO J=1,NDIM
          DFDU(J,I)=(F(J)-DFDU(J,I))/(2*EP)
        ENDDO
      ENDDO
C
      CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
      IF(IJAC==1)RETURN
C
      DO I=1,NFPR
        PAR(ICP(I))=PAR(ICP(I))+EP
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,DFDP(1,ICP(I)),NDM,DFU)
        DO J=1,NDIM
          DFDP(J,ICP(I))=(DFDP(J,ICP(I))-F(J))/EP
        ENDDO
        PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),ICP(*)
      DIMENSION RAP(*),U(NDIM),UOLD(*),PAR(*),F(*)
      DIMENSION DFDU(NDM,*)
C
C       Local
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
C
      NDM=IAP(23)
C
      IF(ISTART.GE.0)THEN
         IF(ITWIST.EQ.0)THEN
C           *Evaluate the R.-H. sides
            CALL FUNC(NDM,U,ICP,PAR,0,F,DFDU,DUM1)
         ELSEIF(ITWIST.EQ.1)THEN
C           *Adjoint variational equations for normal vector
            CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUM1)
C           *Set F = - (Df)^T u
            DO J=1,NDM
               DUM1=0.0D0
               DO I=1,NDM
                  DUM1=DUM1+DFDU(I,J)*U(NDM+I)
               ENDDO
               F(NDM+J) = -DUM1
            ENDDO
C           *Set F =  F + PAR(10) * f
            DO J=1,NDM
               F(NDM+J) = F(NDM+J) + PAR(10) * F(J)
            ENDDO
         ENDIF
      ELSE
C        Homoclinic branch switching
         DO J=0,NDIM-NDM,NDM
            CALL FUNC(NDM,U(J+1),ICP,PAR,0,F(J+1),DFDU,DUM1)
         ENDDO
      ENDIF
C
C Scale by truncation interval T=PAR(11)
C
      IF (ISTART.GE.0) THEN
         DO I=1,NDIM
            F(I)=PAR(11)*F(I)
         ENDDO
      ELSE
         DO I=1,NDM
            F(I)=PAR(10)*F(I)
            DO J=1,NDIM/NDM-2
               F(I+NDM*J)=PAR(19+J*2)*F(I+NDM*J)
            ENDDO
            F(I+NDIM-NDM)=PAR(11)*F(I+NDIM-NDM)
         ENDDO   
      ENDIF
C	
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCHO(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
      INCLUDE 'auto.h'
C
      PARAMETER (M2X=2*NDIMX+NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the boundary conditions for homoclinic bifurcation analysis
C
      DIMENSION IAP(*),ICP(*)
      DIMENSION U0(*),U1(*),F(NBC),PAR(*),DBC(NBC,*)
C Local
      DIMENSION UU(NDIMX),FF1(NBCX),FF2(NBCX)
C
       NBC0=IAP(24)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,F)
C
       IF(IJAC.EQ.0)RETURN
C
C Derivatives with respect to U0.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         UU(I)=U0(I)
       ENDDO
       DO I=1,NDIM
         UU(I)=U0(I)-EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,UU,U1,FF1)
         UU(I)=U0(I)+EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,UU,U1,FF2)
         UU(I)=U0(I)
         DO J=1,NBC
           DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
C Derivatives with respect to U1.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U1(I)).GT.UMX)UMX=DABS(U1(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         UU(I)=U1(I)
       ENDDO
       DO I=1,NDIM
         UU(I)=U1(I)-EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,UU1,FF1)
         UU(I)=U1(I)+EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,UU2,FF2)
         UU(I)=U1(I)
         DO J=1,NBC
           DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF2)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(FF2(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FB)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates the boundary conditions for homoclinic orbits.
C
      DIMENSION ICP(*),IAP(*)
      DIMENSION RAP(*),PAR(*),U0(*),U1(*),FB(*)
C Local
      DIMENSION VR(NX,NX,2),VT(NX,NX,2)
      DIMENSION BOUND(NX,NX),RR(NX,2),RI(NX,2),XEQUIB1(NX),XEQUIB2(NX)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
      COMMON /BLBRN/ UMAX(NX)
      COMMON /BLRTN/ IRTN,NRTN(NBCX)

      NDM=IAP(23)
C
C     *Initialization
      DO I=1,NBC
         FB(I) = 0.0D0
      ENDDO
      JB = 1
C     
      IF ((IEQUIB.EQ.0).OR.(IEQUIB.EQ.-1)) THEN
         CALL PVLS(NDM,U0,PAR)
      ENDIF
C              write(9,*) 'Xequib:'
      DO I=1,NDM
         XEQUIB1(I)=PAR(11+I)
C              write(9,*) I,XEQUIB1(I)
      ENDDO
C     ** Rotations */
      IF(IRTN.NE.0)THEN
         DO I=1,NDIM
            XEQUIB2(I)=XEQUIB1(I)
            IF(NRTN(I).NE.0)THEN
               IF(ISTART.LT.0)THEN
                  PAR(19)=-ISTART*PI(2.d0)
               ENDIF
               XEQUIB2(I)=XEQUIB2(I)+PAR(19)*NRTN(I)
            ENDIF
         ENDDO
      ELSEIF(IEQUIB.GE.0) THEN
         DO I=1,NDM
            XEQUIB2(I)=PAR(11+I)
         ENDDO
      ELSE
         DO I=1,NDM
            XEQUIB2(I)=PAR(NDM+11+I)
         ENDDO
      ENDIF
C
C     **Regular Continuation**
      IF(ISTART.NE.3) THEN
C        *Projection boundary conditions for the homoclinic orbit
C        *NSTAB boundary conditions at t=0
	     CALL PRJCTI(IAP,RAP,BOUND,XEQUIB1,ICP,PAR,-1,1,1,NDM)
             DO I=1,NSTAB
                DO K=1,NDM
                   FB(JB)=FB(JB)+(U0(K)-XEQUIB1(K))*BOUND(I,K)
                ENDDO
                JB = JB+1
             ENDDO
C
C        *NUNSTAB boundary conditions at t=1
         IF(NREV.EQ.0) THEN
            CALL PRJCTI(IAP,RAP,BOUND,XEQUIB2,ICP,PAR,1,2,1,NDM)
            DO I=NDM-NUNSTAB+1,NDM
               DO K=1,NDM
                  IF (ISTART.GE.0) THEN
                    FB(JB)=FB(JB)+(U1(K)-XEQUIB2(K))*BOUND(I,K)
                  ELSE
                    FB(JB)=FB(JB)+(U1(NDIM-NDM+K)-XEQUIB2(K))*BOUND(I,K)
                    IF (ITWIST.EQ.0) THEN
C                     allow jump at end.
                       FB(JB)=FB(JB)+PAR(22)
                    ENDIF
                 ENDIF
               ENDDO
               JB = JB+1
            ENDDO
         ELSE
C         *NUNSTAB symmetric boundary conditions at t=1 if NREV=1
C
            DO I=1,NDIM
               IF(IREV(I).GT.0) THEN
                  FB(JB)=U1(I)  
                  JB=JB+1
               ENDIF
            ENDDO
         ENDIF
         INEIG=0
C        *NFIXED extra boundary conditions for the fixed conditions
         IF (NFIXED.GT.0) THEN
            CALL EIGHI(IAP,RAP,2,RR(1,1),RI(1,1),VR(1,1,1),
     *           XEQUIB1,ICP,PAR,NDM)
            IF(IEQUIB.LT.0) THEN
               CALL EIGHI(IAP,RAP,2,RR(1,2),RI(1,2),VR(1,1,1),
     *              XEQUIB2,ICP,PAR,NDM)
            ENDIF
            DO I=1,NFIXED
               IF((IFIXED(I).GT.10).AND.(INEIG.EQ.0)) THEN
                  CALL EIGHI(IAP,RAP,1,RR(1,1),RI(1,1),VT(1,1,1),
     *                 XEQUIB1,ICP,PAR,NDM)
                  INEIG=1
                  IF(IEQUIB.LT.0) THEN
                     CALL EIGHI(IAP,RAP,1,RR(1,2),RI(1,2),VT(1,1,2),
     *                    XEQUIB2,ICP,PAR,NDM)
                  ENDIF
               ENDIF
               FB(JB)=PSIHO(IAP,IFIXED(I),RR,RI,VR,VT,ICP,PAR,U0,U1)
               JB = JB+1 
            ENDDO
         ENDIF
C        *extra boundary condition in the case of a saddle-node homoclinic
         IF (IEQUIB.EQ.2) THEN
            IF(INEIG.EQ.0) THEN
               CALL EIGHI(IAP,RAP,1,RR(1,1),RI(1,1),VT(1,1,1),
     *              XEQUIB1,ICP,PAR,NDM)
               INEIG=1
	    ENDIF
	    FB(JB)=RR(NSTAB+1,1)
	    JB=JB+1	
         ENDIF
C        *NDM initial conditions for the equilibrium if IEQUIB=1,2,-2
         IF ((IEQUIB.NE.0).AND.(IEQUIB.NE.-1)) THEN
            CALL FUNC(NDM,XEQUIB1,ICP,PAR,0,FB(JB),DUM1,DUM2)
            JB=JB+NDM
C        *NDM extra initial conditions for the equilibrium if IEQUIB=-2
            IF (IEQUIB.EQ.-2) THEN
               CALL FUNC(NDM,XEQUIB2,ICP,PAR,0,FB(JB),DUM1,DUM2)
               JB=JB+NDM
            ENDIF
         ENDIF
C        *boundary conditions for normal vector
         IF ((ISTART.GE.0).AND.(ITWIST.EQ.1)) THEN
C           *-orthogonal to the unstable directions of A  at t=0
            CALL PRJCTI(IAP,RAP,BOUND,XEQUIB1,ICP,PAR,1,1,2,NDM)
            DO I=NDM-NUNSTAB+1,NDM
               DUM=0.0
               DO K=1,NDM
                  DUM=DUM+U0(NDM+K)*BOUND(I,K)
               ENDDO
               FB(JB)=DUM 
               JB = JB+1
            ENDDO
C           *-orthogonal to the stable directions of A  at t=1
            CALL PRJCTI(IAP,RAP,BOUND,XEQUIB2,ICP,PAR,-1,2,2,NDM)
            DO I=1,NSTAB
               DUM=0.0
               DO K=1,NDM
                  DUM=DUM+U1(NDM+K)*BOUND(I,K)
               ENDDO
               FB(JB)=DUM 
               JB = JB+1
            ENDDO
C      Branch switching to n-homoclinic orbits.
         ELSEIF(ISTART.LT.0) THEN
C         More boundary conditions: continuity+gaps
            DO K=0,NDIM/NDM-2
               DO I=1,NDM
                  FB(JB)=U0(NDM*(K+1)+I)-U1(NDM*K+I)
                  IF (ITWIST.EQ.1) THEN 
C     Lin(-Sandstede): PAR(20,22,...) contain the gap sizes,
C     PAR(NPARX-2*NDM+1...NPARX-NDM) contains the adjoint unit
C     vector at the gaps.
                     FB(JB)=FB(JB)-PAR(20+2*K)*PAR(NPARX-2*NDM+I)
                  ENDIF
                  JB = JB+1
               ENDDO
            ENDDO
C     Poincare sections: <x-x_0,\dot x_0>=0
C     PAR(NPARX-NDM+1...NPARX) contains the derivatives of the
C     point x_0 in the original
C     homoclinic orbit that is furthest from the equilibrium.
C     x_0=umax is initialized at each run to an end point, and so
C     is always in the Poincare section
            IF (UMAX(1).GT.1.0D29) THEN
               DO I=1,NDM
                  UMAX(I) = U1(I)
               ENDDO
            ENDIF
            DO K=0,NDIM/NDM-2
               DO I=1,NDM
                  FB(JB)=FB(JB)+(U1(K*NDM+I)-UMAX(I))*PAR(NPARX-NDM+I)
               ENDDO
               JB = JB + 1
            ENDDO
         ENDIF
      ELSE
C     **Starting Solutions using Homotopy**
         IP=12
         IF(IEQUIB.GE.0) THEN 
            IP=IP+NDM
         ELSE
            IP=IP+2*NDM
         ENDIF
         KP=IP
C        *Explicit boundary conditions for homoclinic orbit at t=0
         CALL EIGHI(IAP,RAP,2,RR,RI,VR,XEQUIB1,ICP,PAR,NDM)
         JB=NDM+1
         IF(NUNSTAB.GT.1) THEN
            FB(JB)=0.0
            KP=IP+NUNSTAB
            DO J=1,NUNSTAB
               DO I=1,NDM
                  FB(I)=FB(I)+U0(I)-XEQUIB1(I)-PAR(IP+J)*
     *                 VR(NDM-NUNSTAB+J,I,1)
               ENDDO
               FB(JB)=FB(JB)+PAR(IP+J)**2
            ENDDO
            FB(JB)=FB(JB)-PAR(IP)
	    JB=JB+1
         ELSE
            KP=IP+1
            JB=NDM
            DO I=1,NDM
               FB(I)=U0(I)-XEQUIB1(I)-PAR(IP)*PAR(IP+1)*
     *              VR(NDM-NUNSTAB+1,I,1)
            ENDDO
         ENDIF
C        *Projection boundary conditions for the homoclinic orbit at t=1
         CALL EIGHI(IAP,RAP,1,RR,RI,VT,XEQUIB2,ICP,PAR,NDM)
         DO I=NDM-NUNSTAB,NDM
            DUM=0.0D0
            DO J=1,NDM
               DUM=DUM+(U1(J)-XEQUIB2(J))*VT(I,J,1)
            ENDDO 
            KP=KP+1
            FB(JB)=DUM-PAR(KP)
	    JB=JB+1
         ENDDO
C        *NDM initial conditions for the equilibrium if IEQUIB=1,2,-2
         IF ((IEQUIB.NE.0).AND.(IEQUIB.NE.-1)) THEN
            CALL FUNC(NDM,XEQUIB1,ICP,PAR,0,FB(JB),DUM1,DUM2)
            JB=JB+NDM
C        *NDM extra initial conditions for the equilibrium if IEQUIB=-2
            IF (IEQUIB.EQ.-2) THEN
               CALL FUNC(NDM,XEQUIB2,ICP,PAR,0,FB(JB),DUM1,DUM2)
               JB=JB+NDM
            ENDIF
         ENDIF
      ENDIF
C
      NBCN=NBC-JB+1
C      write(9,*) NBCN,NBC
C *user defined extra boundary conditions
      IF (NBCN.GT.0) THEN
         CALL BCND(NDIM,PAR,ICP,NBCN,U0,U1,FB(JB),0,0)
      ELSE
         PRINT*,'Evil BUG!: Negative number of boundary conditions left'
         STOP
      END IF
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICHO(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
      INCLUDE 'auto.h'
      PARAMETER (M2X=NDIMX+NPARX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates integral conditions for homoclinic bifurcation analysis
C
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
C Local
      DIMENSION UU1(NDIMX),UU2(NDIMX),FF1(NINTX),FF2(NINTX)
      DIMENSION DFU(NDIMX,M2X)
C
       NNT0=IAP(25)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F,DFU)
C
       IF(IJAC.EQ.0)RETURN
C
C Generate the Jacobian.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
C
       EP=HMACH*(1+UMX)
C
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU1,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU2,UOLD,UDOT,
     *    UPOLD,FF2,DFU)
C

C
         DO J=1,NINT
           DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     * UPOLD,FI,DINT)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates the integral conditions for homoclinic orbits.
C
      DIMENSION ICP(*),IAP(*)
      DIMENSION RAP(*),U(*),UOLD(*),UDOT(*),UPOLD(*),FI(*)
      DIMENSION DINT(NINT,*),PAR(*)
      dimension fj(NDIMX)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
C
      NDM=IAP(23)
      JB=0
C
C Integral phase condition for homoclinic orbit
C    
      IF((NREV.EQ.0).AND.(ISTART.GE.0)) THEN
         DUM=0.d0
         DO I=1,NDM
            DUM=DUM+UPOLD(I)*(U(I)-UOLD(I))
         ENDDO
         JB=JB+1
         FI(JB)=DUM
      ENDIF
C     
C Integral phase condition for adjoint equation     
C
      IF ((ITWIST.EQ.1).AND.(ISTART.GE.0)) THEN
         DUM=0.d0
         DO I=1,NDM
            DUM=DUM+UOLD(NDM+I)*(U(NDM+I)-UOLD(NDM+I))
         ENDDO
         JB=JB+1
         FI(2)=DUM
      ENDIF
C
C User-defined integral constraints
C
      IF (JB.LT.NINT) THEN
      CALL ICND(NDM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FJ,0,DINT) 
         DO I=1,NINT-JB
         FI(I+JB)=FJ(I)
         END DO
      END IF
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE INHO(IAP,ICP,PAR)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(HMACHHO=1.0d-13,NX=NDIMX,NPSIX=NPARX)
      DIMENSION PAR(*),IAP(*),ICP(*)
C
C Reads from fort.11 specific constants for homoclinic continuation.
C Sets up re-defined constants in IAP. 
C Sets other constants in the following common blocks.
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
      COMMON /BLHMA/ COMPZERO
      COMMON /BLBRN/ UMAX(NX)
C
C set various constants 
C
      NDIM=IAP(1)
      IRS=IAP(3)
      ISW=IAP(10)
      NBC=IAP(12)
      NINT=IAP(13)
      NUZR=IAP(15)
      NDM=NDIM
      COMPZERO=HMACHHO
C
      OPEN(UNIT=12,FILE='fort.12',STATUS='OLD')
      READ(12,*)NUNSTAB,NSTAB,IEQUIB,ITWIST,ISTART
C
C updated reading in of constants for reversible equations
C replaces location in datafile of compzero
C
      READ(12,*)NREV
      IF(NREV.GT.0)READ(12,*)(IREV(I),I=1,NDIM)
C
      READ(12,*)NFIXED
      IF (NFIXED.GT.0)READ(12,*)(IFIXED(I),I=1,NFIXED)
      READ(12,*)NPSI
      IF (NPSI.GT.0)READ(12,*)(IPSI(I),I=1,NPSI)
      CLOSE(UNIT=12,STATUS='KEEP')
      NFREE=2+NFIXED-NREV+NINT+NBC
      IF (ISTART.LT.0) THEN
C        n-homoclinic branch switching
         NFREE=NFREE-ISTART-1
         NDIM=NDM*(-ISTART+1)
C        high value to detect later
         UMAX(1) = 1.0D30 
C      
C Free parameter (artificial parameter for psi)
C nondegeneracy parameter of the adjoint
C
      ELSEIF (ITWIST.EQ.1) THEN
         NFREE = NFREE + 1
         ICP(NFREE) = 10
         PAR(10)= 0.0D0
         NDIM=NDM*2
      ENDIF
C
C Extra free parameters for equilibrium if iequib=1,2,-2
C
      IF ((IEQUIB.NE.0).AND.(IEQUIB.NE.-1)) THEN
         DO I=1,NDM
            ICP(NFREE+I)=11+I
         ENDDO
      ENDIF
C
      IF (IEQUIB.EQ.-2) THEN
         DO I=1,NDM
            ICP(NFREE+NDM+I)=11+NDM+I
         ENDDO
      ENDIF 
C
      IF (ISTART.NE.3) THEN
C     *regular continuation
        IF (ISTART.GE.0) THEN
           NINT=NINT+ITWIST+1-NREV
        ENDIF 
        IF (ISW.EQ.2) THEN
          ICORR = 2
        ELSE
          ICORR = 1
        ENDIF
        NBC=NBC+NSTAB+NUNSTAB+NDIM-NDM+IEQUIB*NDM+NFREE-NINT-ICORR
        IF (IEQUIB.EQ.2) THEN
	  NBC=NBC-NDM+1
        ENDIF
        IF (IEQUIB.LT.0) THEN
           NBC=NBC-(3*IEQUIB+2)*NDM
        ENDIF
      ELSE
C     *starting solutions using homotopy
        IF (NUNSTAB.EQ.1) THEN
          NBC=NDM*(1+IEQUIB)+1
        ELSE
          NBC=NDM*(1+IEQUIB)+NUNSTAB+1
        ENDIF
        IF (IEQUIB.EQ.2) THEN 
        WRITE(9,*)'WARNING: IEQUIB=2 NOT ALLOWED WITH ISTART=3'
        ENDIF
        IF (IEQUIB.LT.0) THEN
          NBC=NBC-NDM*(3*IEQUIB+2)
        ENDIF
        NINT=0
      ENDIF
C
C write new constants into IAP
C
      IAP(1)=NDIM
      IAP(12)=NBC
      IAP(13)=NINT
      IAP(15)=NUZR
      IAP(23)=NDM
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE INTPHO(IAP,RAP,NDM,NCOLRS,TM,DTM,NDX,UPS,UDOTPS,T,DT,N,
     *     NDIM,J,J1)
C
      INCLUDE 'auto.h'
      PARAMETER (M1T=NTSTX+1,M2T=NDIMX*NCOLX)
      PARAMETER (MCL1=NCOLX,MCL2=MCL1+1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION UPS(NDX,*), UDOTPS(NDX,*)
C
C Local
C
      DIMENSION X(MCL2),W(MCL2)
C
C     Finds interpolant (TM(.) , UPS(.), UDOTPS(.) ) on the new mesh
C     at times TM,TM+DTM using the old mesh at times T,T+DT.
C
C     Used by TRANHO to initiate branch switching to n-homoclinic orbits.
C
      NCP1=NCOLRS+1
C
      D=DTM/NCOLRS
      DO L=1,NCP1
         X(L)=TM+(L-1)*D
      ENDDO
      DO I=0,NCOLRS-1
         Z=T+DT*I/NCOLRS
         CALL INTWTS(IAP,RAP,NCP1,Z,X,W)
         K1=I*NDIM+N
         DO K=1,NDM
            UPS(J1,K1+K)=W(NCP1)*UPS(J+1,N+K)
            UDOTPS(J1,K1+K)=W(NCP1)*UDOTPS(J+1,N+K)
            DO L=1,NCOLRS
               L1=K+(L-1)*NDIM+N
               UPS(J1,K1+K)=UPS(J1,K1+K)+W(L)*UPS(J,L1)
               UDOTPS(J1,K1+K)=UDOTPS(J1,K1+K)+W(L)*UDOTPS(J,L1)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE TRANHO(IAP,RAP,NTSR,NCOLRS,NDM,NDIM,TM,DTM,NDX,UPS,
     *     UDOTPS,PAR)
C
C     Transform the data representation of the homoclinic orbit into
C     an object suitable for homoclinic branch switching:
C
C     dim|1...............NDM|NDM+1......NDIM-NDM|NDIM-NDM+1......NDIM|
C        |                   |                   |                    |
C     t=0|start of hom. orbit|maximum from equil.| maximum from equil.|
C        |       :           |       :           |       :            |
C        |       :           |end of hom. orbit  |       :            |
C        |       :           |start of hom. orbit|       :            |
C        |       :           |        :          |       :            |
C     t=1|maximum from equil.|maximum from equil.| end of hom. orbit  |
C
C     Called by PREHO
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,M1T=NTSTX+1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TM(*), DTM(*), UPS(NDX,*), UDOTPS(NDX,*), PAR(*)
C Local
      DIMENSION TTM(M1T),J2(3),A(3),B(3),T(3),TT(3),F(NX)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLBRN/ UMAX(NX)
C
C First find maximum from the equilibrium
C     
      UPSMAX=0
      JMAX=1
      DO J=1,NTSR+1
         UPSI=0
         DO I=1,NDM
            UPSI=UPSI+(UPS(J,I)-PAR(11+I))*(UPS(J,I)-PAR(11+I))
         ENDDO
         IF (UPSI.GT.UPSMAX) THEN
            UPSMAX=UPSI
            JMAX=J
         ENDIF
      ENDDO
      TMMAX=TM(JMAX)
      DO I=1,NDM
         UMAX(I) = UPS(JMAX,I)
      ENDDO
      CALL FUNC(NDM,UMAX,ICP,PAR,0,F,DUM1,DUM2)
      DO I=1,NDM
         PAR(NPARX-NDM+I)=F(I)
      ENDDO
C      DO I=1,NDM
C         PAR(NPARX-NDM+I)=UPS(JMAX,I)
C      ENDDO
C     
C     PAR(NPARX-NDM+1...NPARX) contains the point furthest from
C     the equilibrium.
C     PAR(10)=the time for the unstable manifold tail.
C     PAR(11)=the time for the stable manifold tail.
C     PAR(20,22,...) contain the gap sizes.
C     PAR(21,23,...) contain the times between Poincare sections
C     
      PAR(10)=PAR(11)*TMMAX
      PAR(20)=0D0
      DO K=1,NDIM/NDM-2
         PAR(19+2*K)=PAR(11)
         PAR(20+2*K)=0D0
      ENDDO
      PAR(11)=PAR(11)*(1D0-TMMAX)
C 
C     Remember adjoint at maximum for applying Lin's method
C     PAR(NPARX-2*NDM+1...NPARX-NDM) will contain the adjoint unit
C     vector at the gaps.
C
      IF (ITWIST.EQ.1) THEN
         DNORM=0.D0
         DO I=1,NDM
            PAR(NPARX-2*NDM+I)=UPS(JMAX,NDM+I)
            DNORM=DNORM+UPS(JMAX,NDM+I)*UPS(JMAX,NDM+I)
         ENDDO
         DNORM=DSQRT(DNORM)
         DO I=1,NDM
            PAR(NPARX-2*NDM+I)=PAR(NPARX-2*NDM+I)/DNORM
         ENDDO
      ENDIF
C     
C     Prepare the new NDIM*NCOLRS dimensional UPS matrix
C     Move everything to the end in "middle part format"
C     so that we can subsequently overwrite the beginning.
C 
      DO L=2*NTSR,NTSR,-1
         J=L-2*NTSR+JMAX
         IF (J.LE.0) J=J+NTSR
         TTM(L)=TM(J)-TMMAX
         IF (TTM(L).LT.0) TTM(L)=TTM(L)+1D0
         DO K=0,(NCOLRS-1)*NDIM,NDIM
            DO I=K+1,K+NDM
               UPS(L,I+NDM)=UPS(J,I)
               UDOTPS(L,I+NDM)=UDOTPS(J,I)
               UPS(L,I)=UPS(J,I)
               UDOTPS(L,I)=UDOTPS(J,I)
               IF (L.LE.2*NTSR-JMAX+1) THEN
                  UPS(L+JMAX-1,I+NDIM-NDM)=UPS(J,I)
                  UDOTPS(L+JMAX-1,I+NDIM-NDM)=UDOTPS(J,I)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      TTM(2*NTSR)=1D0
C     
C     create matching mesh
C     merge TM(1..JMAX)/TMMAX, TM(JMAX..NTSR)-TMMAX,
C           TM(1..JMAX)+1D0-TMMAX, 
C           (TM(JMAX..NTSR)-TMMAX)/(1D0-TMMAX)
C
      J2(1)=2*NTSR-JMAX+2
      J2(2)=NTSR+1
      J2(3)=NTSR+1
      A(1)=TMMAX-1D0
      A(2)=0D0
      A(3)=0D0
      B(1)=TMMAX
      B(2)=1D0
      B(3)=1D0-TMMAX
      NTSR=NTSR*2-2
      DO I=1,3
         T(I) = (TTM(J2(I))+A(I))/B(I)
         TT(I) = (TTM(J2(I)-1)+A(I))/B(I)
      ENDDO
      DO J=2,NTSR+1
         TM(J)=T(1)
         I2=1
         DO I=2,3
            IF (T(I).LT.TM(J)) THEN
               TM(J)=T(I)
               I2=I
            ENDIF
         ENDDO
         DTM(J-1)=TM(J)-TM(J-1)
C     
C     copy first part to temp arrays upst
C     Replace UPS and UDOTPS by its interpolant on the new mesh :
C     
         CALL INTPHO(IAP,RAP,NDM,NCOLRS,TT(1),T(1)-TT(1),NDX,UPS,UDOTPS,
     *        TM(J-1),DTM(J-1),0,NDIM,J2(1)-1,J-1)
C
C     Remesh middle part :
C     
         CALL INTPHO(IAP,RAP,NDM,NCOLRS,TT(2),T(2)-TT(2),NDX,UPS,UDOTPS,
     *        TM(J-1),DTM(J-1),NDM,NDIM,J2(2)-1,J-1)
C     
C     Remesh last part :
C     
         CALL INTPHO(IAP,RAP,NDM,NCOLRS,TT(3),T(3)-TT(3),NDX,UPS,UDOTPS,
     *        TM(J-1),DTM(J-1),NDIM-NDM,NDIM,J2(3)+JMAX-2,J-1)
C     
C     Copy middle parts, this applies only for 1->n switching
C     where n>=3 and NDIM=(n+1)*NDM: (NDIM/NDM)-3 times.
C     
         DO K2=NDM,NDIM-3*NDM,NDM
            DO K=NDM,(NCOLRS-1)*NDIM+NDM,NDIM
               DO I=K+1,K+NDM
                  UPS(J-1,I+K2)=UPS(J-1,I)
                  UDOTPS(J-1,I+K2)=UDOTPS(J-1,I)
               ENDDO
            ENDDO
         ENDDO
         J2(I2)=J2(I2)+1
         TT(I2)=T(I2)
         T(I2)=(TTM(J2(I2))+A(I2))/B(I2)
      ENDDO
C
C     Adjust end points
C
      DO I=1,NDM
         DO K2=I,NDIM-NDM,NDM
            UPS(NTSR+1,K2)=UPS(NTSR+2,I+NDM)
            UDOTPS(NTSR+1,K2)=UDOTPS(NTSR+2,I+NDM)
         ENDDO
         UPS(NTSR+1,I+NDIM-NDM)=UPS(1,I)
         UDOTPS(NTSR+1,I+NDIM-NDM)=UDOTPS(1,I)
      ENDDO
      END
C
C     ---------- ------
      SUBROUTINE CPBKHO(NTSR,NCOLRS,NAR,NDM,TM,DTM,NDX,UPS,UDOTPS,PAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION UPS(NDX,*), UDOTPS(NDX,*), TM(*), PAR(*), DTM(*)
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
C
C     Copy the homoclinic orbit back from the special representation 
C     gotten from TRANHO to the usual representation.
C     This is called from PREHO in order to perform normal continuation
C     again once the branch switching is complete.
C
      NDIM=NDM*(ITWIST+1)
      NCOPY=NAR/NDM
      J=1
      TIME=PAR(10)+PAR(11)
      DO K=1,NCOPY-1
         TIME=TIME+PAR(19+2*K)
      ENDDO
      TBASE=TIME-PAR(11)
      TM(NTSR*NCOPY+1)=1.0D0
      DO K=NCOPY-1,0,-1
         DO J=NTSR,1,-1
            I=J+NTSR*K
            DO L=0,NCOLRS-1
               DO M=1,NDM
                  UPS(I,L*NDIM+M)=UPS(J,L*NAR+K*NDM+M)
                  UDOTPS(I,L*NDIM+M)=UDOTPS(J,L*NAR+K*NDM+M)
               ENDDO
            ENDDO
            IF (K.EQ.0) THEN
               TM(I)=TM(J)*PAR(10)/TIME
            ELSEIF (K.EQ.NCOPY-1) THEN
               TM(I)=(TBASE+TM(J)*PAR(11))/TIME
            ELSE
               TM(I)=(TBASE+TM(J)*PAR(19+K*2))/TIME
            ENDIF
            DTM(I)=TM(I+1)-TM(I)
         ENDDO
         IF (K.EQ.1) THEN
            TBASE=TBASE-PAR(10)
         ELSE
            TBASE=TBASE-PAR(17+K*2)
         ENDIF
      ENDDO
      NTSR=NTSR*NCOPY
C
C Last equal to first
C
      DO K=1,NDIM
         UPS(NTSR+1,K)=UPS(1,K)
         UDOTPS(NTSR+1,K)=UDOTPS(1,K)
      ENDDO
      PAR(10)=0.D0
      PAR(11)=TIME
      NAR=NDM
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE PREHO(IAP,RAP,PAR,ICP,NDX,NTSR,NAR,NCOLRS,UPS,UDOTPS,
     *     TM,DTM)
C
      INCLUDE 'auto.h'
C
C     Special homoclinic orbit preprocessing.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION UPS(NDX,*), TM(*), DTM(*), UDOTPS(NDX,*), PAR(*), IAP(*)
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
C
C Local
C
      DIMENSION F(NDIMX), UI(NDIMX)
C
      NDIM=IAP(1)
      NDM=IAP(23)
C
C Shift phase if necessary if continuing from
C a periodic orbit into a homoclinic one
C
      IF (ISTART.EQ.4) THEN

C Try to find an approximate value for the equilibrium if it's not
C explicitely given. This is just the point where the speed is minimal.
C We hope that Newton's method will do the rest.

         IF (IEQUIB.GT.0) THEN
            UPSMIN=1D20
            JMIN=1
            DO J=1,NTSR+1
               DO I=1,NDM
                  UI(I) = UPS(J,I)
               ENDDO
               CALL FUNC(NDM,UI,ICP,PAR,0,F,DUM1,DUM2)
               UPSI=0
               DO I=1,NDM
                  UPSI=UPSI+F(I)*F(I)
               ENDDO
               IF (UPSI.LT.UPSMIN) THEN
                  JMIN = J
                  UPSMIN = UPSI
               ENDIF
            ENDDO
            DO I=1,NDM
               PAR(11+I)=UPS(JMIN,I)
            ENDDO
         ENDIF
C
C Find smallest value in norm
C
       UPSMIN=1D20
       JMIN=1
       DO J=1,NTSR+1
         UPSI=0
         DO I=1,NDM
           UPSI=UPSI+(UPS(J,I)-PAR(11+I))*(UPS(J,I)-PAR(11+I))
         ENDDO
         IF (UPSI.LT.UPSMIN) THEN
           UPSMIN=UPSI
           JMIN=J
         ENDIF
       ENDDO
       TMMIN=TM(JMIN)
C
C And then do the actual shift
C 
       IF (JMIN.NE.1) THEN
        IST=0
        J=NTSR+1
        DO II=1,NTSR
           IF (J.EQ.NTSR+1) THEN
              IST=IST+1
              TM(J)=TM(IST)
              DO K=1,NCOLRS*NDIM
                  UPS(J,K)=UPS(IST,K)
               UDOTPS(J,K)=UDOTPS(IST,K)
              ENDDO
              J=IST
           ENDIF
           I=J
           J=J+JMIN-1
           IF (J.GT.NTSR) J=J-NTSR
           IF (J.EQ.IST) J=NTSR+1
           TM(I)=TM(J)-TMMIN
           IF (TM(I).LT.0) TM(I)=TM(I)+1.0D0
           DO K=1,NCOLRS*NDIM
                  UPS(I,K)=UPS(J,K)
               UDOTPS(I,K)=UDOTPS(J,K)
           ENDDO
        ENDDO
C
C Last equal to first
C
        TM(NTSR+1)=1.0D0
        DO K=1,NCOLRS*NDIM
             UPS(NTSR+1,K)=UPS(1,K)
          UDOTPS(NTSR+1,K)=UDOTPS(1,K)
        ENDDO
C
       ENDIF
      ENDIF
C
C If ISTART<0 we perform homoclinic branch switching and need
C to change the representation of the homoclinic orbit in UPS and
C UDOTPS.
C
      IF (ISTART.LT.0 .AND. NAR.LT.NDIM .AND. NAR.LT.3*NDM) THEN
         CALL TRANHO(IAP,RAP,NTSR,NCOLRS,NDM,NDIM,TM,DTM,NDX,UPS,
     *        UDOTPS,PAR)
      ELSEIF 
     *   (ISTART.LT.0 .AND. NAR.LT.NDIM .AND. NAR.GE.3*NDM) THEN
C Copy forelast part
         DO J=1,NTSR+1
            DO K=0,NDIM*(NCOLRS-1),NDIM
               DO I=NDIM,NAR-NDM+1,-1
                  UPS(J,K+I)=UPS(J,K+I-NDIM+NAR)
                  UDOTPS(J,K+I)=UDOTPS(J,K+I-NDIM+NAR)
               ENDDO
            ENDDO
         ENDDO
         DO I=1,(NDIM-NAR)/NDM
            PAR(16+2*(NAR/NDM+I))=PAR(16+2*NAR/NDM)
            PAR(15+2*(NAR/NDM+I))=PAR(15+2*NAR/NDM)
         ENDDO
         PAR(16+2*NAR/NDM)=(UPS(1,NAR-NDM+1)-
     *            UPS(NTSR+1,NAR-2*NDM+1))/ PAR(NPARX-2*NDM+1)
      ELSEIF (NAR.GT.2*NDM .AND. ISTART.GE.0) THEN
C        Use the usual representation again for normal continuation.
         CALL CPBKHO(NTSR,NCOLRS,NAR,NDM,TM,DTM,NDX,UPS,UDOTPS,PAR)
      ENDIF
C       
C Preprocesses (perturbs) restart data to enable 
C initial computation of the adjoint variable
C
      IF (NAR.NE.NDIM .AND. ISTART.GE.0 .AND. ITWIST.EQ.1) THEN  
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           DO K=K1+NAR,K2
             UPS(J,K)=0.1d0
           ENDDO
         ENDDO
       ENDDO
       DO K=1+NAR,NDIM
         UPS(NTSR+1,K)=0.1d0
       ENDDO
      ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNHO(IAP,RAP,PAR,ICP,NTSR,NCOLRS,RLCUR,RLDOT,
     * NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates a starting point for the continuation of a branch of
C of solutions to general boundary value problems by calling the user
C supplied subroutine STPNT where an analytical solution is given.
C
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)
C Local
      DIMENSION U(NDIMX)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
C Generate the (initially uniform) mesh.
C
       CALL MSH(IAP,RAP,TM)
       DT=1.d0/(NTST*NCOL)
C
       DO J=1,NTST+1
         IF(J.EQ.(NTST+1)) THEN
           NCOL1=1
         ELSE
           NCOL1=NCOL
         ENDIF
         DO I=1,NCOL1
           T=TM(J)+(I-1)*DT
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           CALL STPHO(IAP,ICP,U,PAR,T)
           DO K=K1,K2
             UPS(J,K)=U(K-K1+1)
           ENDDO
         ENDDO
       ENDDO
C
       NTSR=NTST
       NCOLRS=NCOL
       IBR=1
       IAP(30)=IBR
       LAB=0
       IAP(37)=LAB
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       NODIR=1
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE STPHO(IAP,ICP,U,PAR,T)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C  
C Generates a starting point for homoclinic continuation
C If ISTART=2 it calls STPNHO. 
C If ISTART=3 it sets up the homotopy method.
C
      DIMENSION IAP(*),ICP(*),PAR(*),U(*)
C Local
      DIMENSION XEQUIB(NX),RR(NX),RI(NX),VR(NX,NX),VT(NX,NX)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
C
      NDM=IAP(23)
C
C Initialize parameters
C
      CALL STPNT(NDM,U,PAR,T)
C
C Initialize solution and additional parameters
C
      IF (ISTART.NE.3) RETURN
C
C-----------------------------------------------------------------------
C ISTART = 1    CONTINUE
C Obsolete option
C      
C      RETURN
C
C-----------------------------------------------------------------------
C ISTART = 2    CONTINUE
C     *Regular continuation (explicit solution in STHO)
C
C      RETURN
C
C-----------------------------------------------------------------------
C ISTART = 3    CONTINUE
C     *Starting solutions using homotopy
C
      CALL PVLS(NDM,U,PAR)
      DO I=1,NDM
         XEQUIB(I)=PAR(11+I)
      ENDDO
      CALL EIGHI(IAP,RAP,1,RR,RI,VT,XEQUIB,ICP,PAR,NDM)
      CALL EIGHI(IAP,RAP,2,RR,RI,VR,XEQUIB,ICP,PAR,NDM)
C
C Set up artificial parameters at the left-hand end point of orbit
C
      IP=12
      IF(IEQUIB.GE.0) THEN 
         IP=IP+NDM
      ELSE
         IP=IP+2*NDM
      ENDIF
      KP=IP
C
C Parameters xi_1=1, xi_i=0, i=2,NSTAB
C
      PAR(IP+1)=1.0d0
      IF(NUNSTAB.GT.1) THEN
         DO I=2,NUNSTAB
            PAR(IP+I)=0.0
         ENDDO
      ENDIF
      IP=IP+NUNSTAB
C     
C Starting guess for homoclinic orbit in real principal unstable direction
C
      DO I=1,NDM
         U(I)=XEQUIB(I)+VR(NSTAB+1,I)*PAR(KP)*PAR(KP+1)*
     +        EXP(RR(NSTAB+1)*T*PAR(11))
      ENDDO
        write(9,111)(u(i),i=1,ndm)
111     format('stpho : ',i3,e20.10)
C
C Artificial parameters at the right-hand end point of the orbit
C omega_i=<x(1)-x_o,w_i^*>
C
      DO I=1,NUNSTAB
         PAR(IP+I)=0.0
         DO J=1,NDM
            PAR(IP+I)=PAR(IP+I)+VR(NSTAB+1,J)*PAR(KP)*PAR(KP+1)*
     +           EXP(RR(NSTAB+1)*PAR(11))*VT(NSTAB+I,J)
         ENDDO
      ENDDO
      IP=IP+NUNSTAB
      RETURN
C-----------------------------------------------------------------------
      END
C
C     ---------- ------
      SUBROUTINE PVLSHO(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),ICP(*),DTM(*),UPS(NDX,*),PAR(*)
      DIMENSION P0(NDIM,*),P1(NDIM,*)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
      COMMON /BLEIG/ RR(NX),RI(NX),V(NX,NX),VT(NX,NX),XEQUIB(NX),INEIG
C
       IID=IAP(18)
       NDM=IAP(23)
       NTST=IAP(5)
C
        CALL PVLSBV(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
C
C      *Compute eigenvalues
       INEIG=0
       DO I=1,NDM
         XEQUIB(I)=PAR(11+I)
       ENDDO
       CALL EIGHI(IAP,RAP,2,RR,RI,V,XEQUIB,ICP,PAR,NDM)
       IF(IID.GE.3)THEN
         WRITE(9,*) 'EIGENVALUES'
         DO J=1,NDM
          WRITE(9,101) RR(J),RI(J)
         ENDDO
       ENDIF
       IF ((ITWIST.EQ.1).AND.(ISTART.GE.0)) THEN
          CALL EIGHI(IAP,RAP,1,RR,RI,VT,XEQUIB,ICP,PAR,NDM)
          INEIG=1
          ORIENT = PSIHO(IAP,0,RR,RI,V,VT,ICP,PAR,UPS(1,1),UPS(1,NTST))
          IF(IID.GE.3)THEN
            IF (ORIENT.LT.0.0D0) THEN
               WRITE(9,102) ORIENT             
            ELSE
               WRITE(9,103) ORIENT   
            ENDIF
          ENDIF
       ENDIF             
C
      DO I=1,NPSI
        IF((IPSI(I).GT.10).AND.(INEIG.EQ.0)) THEN
          CALL EIGHI(IAP,RAP,1,RR,RI,VT,XEQUIB,ICP,PAR,NDM)
          INEIG=1
        ENDIF
        PAR(20+IPSI(I))=PSIHO(IAP,IPSI(I),RR,RI,V,VT,ICP,PAR,
     *       UPS(1,1),UPS(1,NTST))
        IF(IID.GE.3)WRITE(9,104)IPSI(I),PAR(20+IPSI(I))
      ENDDO
C  
      RETURN
C
 101  FORMAT(1X,'(',F12.7,',',1X,F12.7,')')
 102  FORMAT(1X,'Non-orientable',' (',D20.10,')')
 103  FORMAT(1X,'orientable',' (',D20.10,')')      
 104  FORMAT(1X,'PSI(',I2,')=',D20.10)
C
      END
C
C     -------- ------- -------- -----
      DOUBLE PRECISION FUNCTION PSIHO(IAP,IS,RR,RI,V,VT,ICP,PAR,PU0,PU1)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
C The conditions for degenerate homoclinic orbits are given by PSI(IS)=0.
C 
C RR and RI contain the real and imaginary parts of eigenvalues which are
C ordered with respect to their real parts (smallest first).    
C The (generalised) real eigenvectors are stored as the ROWS of V. 
C The (generalised) real left eigenvectors are in the ROWS of VT.
C In the block ENDPTS are stored the co-ordinates of the left (PU0)
C and right (PU1) endpoints of the solution (+  vector if that is computed)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IAP(*),ICP(*),PAR(*),RR(*),RI(*),V(NX,*),VT(NX,*)
      DIMENSION PU0(*),PU1(*)
C Local
      DIMENSION F0(NX),F1(NX)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
      COMMON /BLHMA/ COMPZERO
C
      NDM=IAP(23)
C
      CALL FUNC(NDM,PU0,ICP,PAR,0,F0,DUM1,DUM2)
      CALL FUNC(NDM,PU1,ICP,PAR,0,F1,DUM1,DUM2)
C
      PSIHO=0.0D0
C
C  Compute orientation
C
      IF (IS.EQ.0) THEN
         S1 = 0.0D0
         S2 = 0.0D0
         F0NORM = 0.0D0
         F1NORM = 0.0D0
         U0NORM = 0.0D0
         U1NORM = 0.0D0
         DO J=1,NDM
            S1 = S1 + F1(J)*PU0(NDM+J)
            S2 = S2 + F0(J)*PU1(NDM+J) 
            F0NORM=F0NORM+F0(J)**2
            F1NORM=F1NORM+F1(J)**2
            U0NORM=U0NORM+PU0(J+NDM)**2
            U1NORM=U1NORM+PU1(J+NDM)**2
         ENDDO
         DROOT=DSQRT(F0NORM*F1NORM*U0NORM*U1NORM)
         IF(DROOT.NE.0.d0)THEN
           PSIHO= - S1*S2/DROOT
         ELSE
           PSIHO=0.d0
         ENDIF
         RETURN
      ENDIF
C
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)IS
C
C Resonant eigenvalues (neutral saddle)
C
 1    CONTINUE
      PSIHO=RR(NSTAB)+RR(NSTAB+1)+RI(NSTAB)+RI(NSTAB+1)
      RETURN     
C
C Double real leading eigenvalues (stable)
C   (saddle, saddle-focus transition)
C
 2    CONTINUE
      IF (ABS(RI(NSTAB)).GT.COMPZERO) THEN
	 PSIHO=-(RI(NSTAB)-RI(NSTAB-1))**2
      ELSE
	 PSIHO=(RR(NSTAB)-RR(NSTAB-1))**2
      ENDIF
      RETURN
C     
C Double real positive eigenvalues (unstable)
C   (saddle, saddle-focus transition)
C
 3    CONTINUE
      IF (ABS(RI(NSTAB+1)).GT.COMPZERO) THEN
         PSIHO=-(RI(NSTAB+1)-RI(NSTAB+2))**2
      ELSE
         PSIHO=(RR(NSTAB+1)-RR(NSTAB+2))**2
      ENDIF
      RETURN
C
C Neutral saddle, saddle-focus or bi-focus (includes 1, above, also) 
C
 4    CONTINUE
      PSIHO=RR(NSTAB)+RR(NSTAB+1)
      RETURN     
C
C Neutrally-divergent saddle-focus (stable eigenvalues complex)
C
 5    CONTINUE
      PSIHO=RR(NSTAB)+RR(NSTAB+1)+RR(NSTAB-1)
      RETURN
C
C Neutrally-divergent saddle-focus (unstable eigenvalues complex)
C
 6    CONTINUE
      PSIHO=RR(NSTAB)+RR(NSTAB+1)+RR(NSTAB+2)
      RETURN
C
C Three leading eigenvalues (stable)
C
 7    CONTINUE
      VNORM1 = 0D0
      VNORM2 = 0D0      
      DO I=1,NDM
          VNORM1 = VNORM1 + ABS(V(NSTAB,I))
          VNORM2 = VNORM2 + ABS(V(NSTAB-2,I))
      ENDDO
      IF (VNORM1.GT.VNORM2) THEN
        PSIHO=RR(NSTAB)-RR(NSTAB-2)
      ELSE
        PSIHO=RR(NSTAB-2)-RR(NSTAB)
      ENDIF
      RETURN
C
C Three leading eigenvalues (unstable)
C
 8    CONTINUE
      VNORM1 = 0D0
      VNORM2 = 0D0      
      DO I=1,NDM
          VNORM1 = VNORM1 + ABS(V(NSTAB+1,I))
          VNORM2 = VNORM2 + ABS(V(NSTAB+3,I))
      ENDDO
      IF (VNORM1.GT.VNORM2) THEN
        PSIHO=RR(NSTAB+1)-RR(NSTAB+3)
      ELSE
        PSIHO=RR(NSTAB+3)-RR(NSTAB+1)
      ENDIF
      RETURN
C
C Local bifurcation (zero eigenvalue or Hopf): NSTAB decreases
C  (nb. the problem becomes ill-posed after a zero of 9 or 10)
C
 9    CONTINUE
      PSIHO=RR(NSTAB)
      RETURN
C
C Local bifurcation (zero eigenvalue or Hopf): NSTAB increases 
C
 10   CONTINUE
      PSIHO=RR(NSTAB+1) 
      RETURN     
C     
C Orbit flip (with respect to leading stable direction)
C     e.g. 1D unstable manifold
C
 11   CONTINUE
      DO J=1,NDM
         PSIHO= PSIHO + F1(J)*VT(NSTAB,J)
      ENDDO
      PSIHO= PSIHO * DEXP(-PAR(11)*RR(NSTAB)/2.0D0)
      RETURN
C
C Orbit flip (with respect to leading unstable direction)
C     e.g. 1D stable manifold
C
 12   CONTINUE
      DO J=1,NDM
         PSIHO= PSIHO + F0(J)*VT(NSTAB+1,J)
      ENDDO
      PSIHO= PSIHO * DEXP(PAR(11)*RR(NSTAB+1)/2.0D0)
      RETURN
C
C Inclination flip (critically twisted) with respect to stable manifold
C   e.g. 1D unstable manifold   
C
 13   CONTINUE
      DO I=1,NDM
          PSIHO= PSIHO + PU0(NDM+I)*V(NSTAB,I)
      ENDDO
      PSIHO= PSIHO * DEXP(-PAR(11)*RR(NSTAB)/2.0D0)
      RETURN
C
C Inclination flip (critically twisted) with respect to unstable manifold
C   e.g. 1D stable manifold
C
 14   CONTINUE
      DO I=1,NDM
         PSIHO= PSIHO + PU1(NDM+I)*V(NSTAB+1,I)
      ENDDO
      PSIHO= PSIHO * DEXP(PAR(11)*RR(NSTAB+1)/2.0D0)
      RETURN
C
C Non-central homoclinic to saddle-node (in stable manifold)
C
 15   CONTINUE
      DO I=1,NDM 
        PSIHO=PSIHO+(PAR(11+I)-PU1(I))*V(NSTAB+1,I)
      ENDDO
      RETURN
C
C Non-central homoclinic to saddle-node (in unstable manifold)
C
 16   CONTINUE
      DO I=1,NDM 
        PSIHO=PSIHO+(PAR(11+I)-PU0(I))*V(NSTAB+1,I)
      ENDDO 
      RETURN
C
      END
C     
C     ---------- -----
      SUBROUTINE EIGHI(IAP,RAP,ITRANS,RR,RI,VRET,XEQUIB,ICP,PAR,NDM)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Local
      DIMENSION DFDU(NX,NX),DFDP(NX,NPARX),ZZ(NX,NX)
C
        CALL EIGHO(IAP,RAP,ITRANS,RR,RI,VRET,XEQUIB,ICP,PAR,NDM,
     *             DFDU,DFDP,ZZ)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE EIGHO(IAP,RAP,ITRANS,RR,RI,VRET,XEQUIB,ICP,PAR,NDM,
     *                  DFDU,DFDP,ZZ)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
C Uses EISPACK routine RG to calculate the eigenvalues/eigenvectors
C of the linearization matrix a (obtained from DFHO) and orders them
C according to their real parts. Simple continuity with respect
C previous call with same value of ITRANS.
C
C	input variables
C               ITRANS = 1 use transpose of A
C                      = 2 otherwise
C
C       output variables
C		RR,RI real and imaginary parts of eigenvalues, ordered w.r.t
C	           real parts (largest first)
C	        VRET the rows of which are real parts of corresponding 
C                  eigenvectors 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*),RR(*),RI(*),VRET(NX,*)
      DIMENSION XEQUIB(*),DFDU(NDM,*),DFDP(NDM,*),ZZ(NDM,*)
C Local
      DIMENSION RRDUM(NX),RIDUM(NX),VRDUM(NX,NX),VIDUM(NX,NX)
      DIMENSION VI(NX,NX),VR(NX,NX),F(NX)
      DIMENSION FV1(NX),IV1(NX)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
      COMMON /BLHMA/ COMPZERO
      COMMON /BLHME/ VRPREV(2,NX,NX),IEIGC(2)
C
      IFAIL=0
C     
      CALL FUNI(IAP,RAP,NDM,XEQUIB,DUM1,ICP,PAR,1,F,DFDU,DFDP)
C     
      IF (ITRANS.EQ.1) THEN
         DO I=1,NDM
            DO J=1,NDM
               VRDUM(I,J)=DFDU(J,I)
            ENDDO
         ENDDO
         DO I=1,NDM
            DO J=1,NDM
               DFDU(I,J)=VRDUM(I,J)
            ENDDO
         ENDDO
      ENDIF
C
C EISPACK call for eigenvalues and eigenvectors
      CALL RG(NDM,NDM,DFDU,RR,RI,1,ZZ,IV1,FV1,IFAIL)
C
      IF (IFAIL.NE.0) THEN   
         WRITE(9,*) 'EISPACK EIGENVALUE ROUTINE FAILED !'
      ENDIF
C 
      DO J=1,NDM 
        IF(RI(J).GT.0.d0)THEN
          DO I=1,NDM
            VR(I,J)=ZZ(I,J)
            VI(I,J)=ZZ(I,J+1)
          ENDDO
        ELSEIF(RI(J).LT.0.d0)THEN
          DO I=1,NDM
            VR(I,J)= ZZ(I,J-1)
            VI(I,J)=-ZZ(I,J)
          ENDDO
        ELSE
          DO I=1,NDM
            VR(I,J)=ZZ(I,J)
            VI(I,J)=0.d0
          ENDDO
        ENDIF
      ENDDO   
C Order the eigenvectors/values according size of real part of eigenvalue.
C     (smallest first)
C
      DO I=1,NDM-1
         DO J=I+1,NDM
            IF (RR(I).GT.RR(J)) THEN
               RRDUM(I)=RR(I)
               RIDUM(I)=RI(I)
               RR(I)=RR(J)
               RR(J)=RRDUM(I)
               RI(I)=RI(J)
               RI(J)=RIDUM(I)
               DO K=1,NDM
                  VRDUM(K,I)=VR(K,I)
                  VR(K,I)=VR(K,J)
                  VR(K,J)=VRDUM(K,I)
                  VIDUM(K,I)=VI(K,I)
                  VI(K,I)=VI(K,J)
                  VI(K,J)=VIDUM(K,I)
               ENDDO
            ENDIF
         ENDDO
      ENDDO
C
C Choose sign of real part of eigenvectors to be 
C commensurate with that of the corresponding eigenvector 
C from the previous call with the same value of ITRANS
C
      IF (IEIGC(ITRANS).EQ.0) THEN
         DO J=1,NDM
            DO I=1,NDM
               VRPREV(ITRANS,I,J)=VR(I,J)
            ENDDO
         ENDDO
         IEIGC(ITRANS)=1
      ENDIF
C
      DO I=1,NDM
         VDOT=0.0D0
         DO J=1,NDM
            VDOT=VDOT+VR(J,I)*VRPREV(ITRANS,J,I)
         ENDDO
         IF (VDOT.LT.0.0D0) THEN
            DO J=1,NDM
               VR(J,I)=-VR(J,I)
C               VI(J,I)=-VI(J,I)
            ENDDO
         ENDIF
         DO J=1,NDM
            VRPREV(ITRANS,J,I)=VR(J,I)
         ENDDO
      ENDDO
C
C Send back the transpose of the matrix of real parts of eigenvectors
C
      DO I=1,NDM
         DO J=1,NDM
            VRET(I,J)=VR(J,I)
         ENDDO
      ENDDO
C     
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE PRJCTI(IAP,RAP,BOUND,XEQUIB,ICP,PAR,IMFD,IS,ITRANS,NDM)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Local
      DIMENSION DFDU(NX,NX)
C
      CALL PRJCTN(IAP,RAP,BOUND,XEQUIB,ICP,PAR,IMFD,IS,ITRANS,NDM,DFDU)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE PRJCTN(IAP,RAP,BOUND,XEQUIB,ICP,PAR,IMFD,IS,ITRANS,NDM,
     *                  DFDU)
C
      INCLUDE 'auto.h'
      PARAMETER(NX=NDIMX,NPSIX=NPARX)
C
C Compute NUNSTAB (or NSTAB) projection boundary condition functions
C onto to the UNSTABLE (or STABLE) manifold of the appropriate equilibrium
C
C    IMFD   = -1 stable eigenspace
C           =  1 unstable eigenspace
C    ITRANS =  1 use transpose of A
C           =  2 otherwise
C    IS     =  I (1 or 2) implies use the ith equilibrium in XEQUIB
C
C Use the normalization in Beyn 1990 (4.4) to ensure continuity 
C w.r.t parameters.
C For the purposes of this routine the "previous point on the
C branch" is at the values of PAR at which the routine was last
C called with the same values of IS and ITRANS.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ICP(*),PAR(*),DFDU(NDM,*)
      DIMENSION BOUND(NX,*),XEQUIB(*)
C Local
      DIMENSION ER(NX),EI(NX),D(NX,NX),CNOW(NX,NX),CPREV(NX,NX,2,2)
      DIMENSION DUM1(NX,NX),DUM2(NX,NX),FDUM(NX)
      DIMENSION A(NX,NX),V(NX,NX),ORT(NX)
      INTEGER IR(NX),IC(NX),IFLAG(2,2),TYPE(NX)
C
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHMP/ IPSI(NPSIX),IFIXED(NPSIX),IREV(NX)
      COMMON /BEYN/ CPREV,IFLAG
      COMMON /BLHMA/ COMPZERO
C
      CALL FUNI(IAP,RAP,NDM,XEQUIB,UDUM,ICP,PAR,1,FDUM,DFDU,DDUM)
C    
C Compute transpose of A if ITRANS=1
      IF (ITRANS.EQ.1) THEN
        DO I=1,NDM
          DO J=1,NDM
            A(I,J)=DFDU(J,I)
          ENDDO
        ENDDO
      ELSE
        DO I=1,NDM
          DO J=1,NDM
            A(I,J)=DFDU(I,J)
          ENDDO
        ENDDO
      ENDIF
C
C Compute basis V to put A in upper Hessenberg form
        CALL ORTHES(NX,NDM,1,NDM,A,ORT)
        CALL ORTRAN(NX,NDM,1,NDM,A,ORT,V)
C
C Force A to be upper Hessenberg
        IF (NDM.GT.2) THEN
          DO I=3,NDM   
            DO J=1,I-2
              A(I,J) = 0.0D0
            ENDDO 
          ENDDO
        ENDIF
C
C Computes basis to put A in "Quasi Upper-Triangular form"
C with the positive (negative) eigenvalues first if IMFD =-1 (=1)
        EPS = COMPZERO
        CALL HQR3LC(A,V,NDM,1,NDM,EPS,ER,EI,TYPE,NX,NX,IMFD)
C
C Put the basis in the appropriate part of the matrix CNOW
        IF (IMFD.EQ.1) THEN
           K1 = NDM-NUNSTAB+1
           K2 = NDM
        ELSE
           K1 = 1
           K2 = NSTAB
        ENDIF
        MCOND=K2-K1+1
        M0=K1-1
C
        DO I=K1,K2   
          DO J=1,NDM  
           CNOW(I,J) = V(J,I-K1+1)
          ENDDO
        ENDDO
C
C Set previous matrix to be the present one if this is the first call
      IF (IFLAG(IS,ITRANS).NE.1234) THEN
         DO I=K1,K2
            DO J=1,NDM
               CPREV(I,J,IS,ITRANS)=CNOW(I,J)
	       BOUND(I,J)=CNOW(I,J)
            ENDDO
         ENDDO
         IFLAG(IS,ITRANS)=1234
	 RETURN
      ENDIF
C     
C Calculate the (transpose of the) BEYN matrix D and hence BOUND 
      DO I=1,MCOND
        DO J=1,MCOND
          DUM1(I,J)=0.0D0
          DUM2(I,J)=0.0D0
          DO K=1,NDM
             DUM1(I,J) = DUM1(I,J)+CPREV(I+M0,K,IS,ITRANS)*
     +                   CNOW(J+M0,K)
             DUM2(I,J) = DUM2(I,J)+CPREV(I+M0,K,IS,ITRANS)*
     +                   CPREV(J+M0,K,IS,ITRANS)
          ENDDO
        ENDDO
      ENDDO
C     
      IF(MCOND.GT.0)THEN
        CALL GE(0,MCOND,NX,DUM1,MCOND,NX,D,NX,DUM2,IR,IC,DET)
      ENDIF
C     
      DO I=1,MCOND
         DO J=1,NDM
            BOUND(I+M0,J)=0.0
            DO K=1,MCOND
               BOUND(I+M0,J)=BOUND(I+M0,J)+D(K,I)*CNOW(K+M0,J)
            ENDDO
         ENDDO
      ENDDO
C     
      DO I=K1,K2
         DO J=1,NDM
            CPREV(I,J,IS,ITRANS)=BOUND(I,J)
         ENDDO
      ENDDO
C     
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

