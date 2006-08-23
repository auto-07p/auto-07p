C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  Subroutines for the Continuation of Folds (Algebraic Problems)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for the 2-par continuation of folds.
C
      DIMENSION IAP(*),U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function.
C
       CALL FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFLP(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFLP(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))+EP
C
       CALL FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
C
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))-EP
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION PAR(*),ICP(*),IAP(*),U(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       IPS=IAP(2)
C
       PAR(ICP(2))=U(NDIM)
       IF(IPS.EQ.-1) THEN
         CALL FNDS(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DFDP)
       ELSE
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DFDP)
       ENDIF
C
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
         ENDDO
       ENDDO
C
       F(NDIM)=-1
C
       DO I=1,NDM
         F(NDIM)=F(NDIM)+U(NDM+I)*U(NDM+I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNLP(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
      LOGICAL FOUND
C
C Generates starting data for the continuation of folds.
C
      DIMENSION U(*),PAR(*),ICP(*),IAP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      DIMENSION IR(NDIMX),IC(NDIMX),V(NDIMX),F(NDIMX)
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       NDM=IAP(23)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
       IF(IPS.EQ.-1)THEN
         CALL FNDS(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DFP)
       ELSE
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DFP)
       ENDIF
       CALL NLVC(NDM,NDM,1,DFU,V,IR,IC)
       CALL NRMLZ(NDM,V)
       DO I=1,NDM
         U(NDM+I)=V(I)
       ENDDO
       U(NDIM)=PAR(ICP(2))
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Subroutines for the Optimization of Algebraic Systems
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNC1(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Generate the equations for the continuation scheme used for
C the optimization of algebraic systems (one parameter).
C
      DIMENSION IAP(*)
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      DIMENSION DDU(NDIMX),DDP(NPARX)
C
       NDM=IAP(23)
C
       PAR(ICP(2))=U(NDIM)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
C Rearrange (Since dimensions in FNC1 and FUNI differ).
C
       IF(IJAC.NE.0)THEN
         DO J=NDM,1,-1
           DO I=NDM,1,-1
             DFDU(I,J)=DFDU( (J-1)*NDM+I ,1 )
           ENDDO
         ENDDO
C
         DO J=NPARX,1,-1
           DO I=NDM,1,-1
             DFDP(I,J)=DFDP( (J-1)*NDM+I , 1 )
           ENDDO
         ENDDO
       ENDIF
C
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,IJAC,F(NDIM),DDU,DDP)
       F(NDIM)=PAR(ICP(1))-F(NDIM)
C
       IF(IJAC.NE.0)THEN
         DO I=1,NDM
           DFDU(NDIM,I)=-DDU(I)
           DFDU(I,NDIM)=DFDP(I,ICP(2))
           DFDP(I,ICP(1))=0
         ENDDO
         DFDU(NDIM,NDIM)=-DDP(ICP(2))
         DFDP(NDIM,ICP(1))=1
       ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNC1(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Generate starting data for optimization problems (one parameter).
C
      DIMENSION U(*),PAR(*),ICP(*),IAP(*)
C 
       NDIM=IAP(1)
       NDM=IAP(23)
C
       CALL STPNT(NDIM,U,PAR,T)
       NFPR=2
       IAP(29)=NFPR
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,0,FOP,DUM,DUM)
       PAR(ICP(1))=FOP
       U(NDIM)=PAR(ICP(2))
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FNC2(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generate the equations for the continuation scheme used for the
C optimization of algebraic systems (more than one parameter).
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function.
C
       CALL FFC2(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFC2(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFC2(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         DFDP(I,ICP(1))=0.d0
       ENDDO
       DFDP(NDIM,ICP(1))=1.d0
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFC2(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C Local
      DIMENSION DDU(NDIMX),DDP(NPARX)
C
       NFPR=IAP(29)
C
       DO I=2,NFPR
         PAR(ICP(I))=U(2*NDM+I)
       ENDDO
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,2,FOP,DDU,DDP)
C
       DO I=1,NDM
         F(NDM+I)=DDU(I)*U(2*NDM+1)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(J,I)*U(NDM+J)
         ENDDO
       ENDDO
C
       NDM2=2*NDM
       ICPM=NFPR-2
       DO I=1,ICPM
         F(NDM2+I)=DDP(ICP(I+1))*U(NDM2+1)
       ENDDO
C
       DO I=1,ICPM
         DO J=1,NDM
           F(NDM2+I)=F(NDM2+I)+U(NDM+J)*DFDP(J,ICP(I+1))
         ENDDO
       ENDDO
C
       F(NDIM-1)=U(NDM2+1)*U(NDM2+1)-1
       DO J=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+J)*U(NDM+J)
       ENDDO
       F(NDIM)=PAR(ICP(1))-FOP
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNC2(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
      LOGICAL FOUND
C
C Generates starting data for the continuation equations for
C optimization of algebraic systems (More than one parameter).
C
      DIMENSION U(*),PAR(*),ICP(*),IAP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      DIMENSION DD(NDIMX,NDIMX),DU(NDIMX),DP(NPARX),V(NDIMX),F(NDIMX)
      DIMENSION IR(NDIMX),IC(NDIMX)
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR,FOUND)
       NFPR=NFPR+1
       IAP(29)=NFPR
       CALL READLB(IAP,RAP,U,PAR)
C
       IF(NFPR.EQ.3)THEN
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
         CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,2,FOP,DU,DP)
C       TRANSPOSE
         DO I=1,NDM
           DO J=1,NDM
             DD(I,J)=DFU((I-1)*NDM+J)
           ENDDO
         ENDDO
         DO I=1,NDM
           DD(I,NDM+1)=DU(I)
           DD(NDM+1,I)=DFP((ICP(2)-1)*NDM+I)
         ENDDO
         DD(NDM+1,NDM+1)=DP(ICP(2))
         CALL NLVC(NDM+1,NDIMX,1,DD,V,IR,IC)
         CALL NRMLZ(NDM+1,V)
         DO I=1,NDM+1
           U(NDM+I)=V(I)
         ENDDO
         PAR(ICP(1))=FOP
       ENDIF
C
       DO I=1,NFPR-1
         U(NDIM-NFPR+1+I)=PAR(ICP(I+1))
       ENDDO
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C        Subroutines for Discrete Dynamical Systems
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNDS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generate the equations for continuing fixed points.
C
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
       DO I=1,NDIM
         F(I)=F(I)-U(I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       DO I=1,NDIM
         DFDU(I,I)=DFDU(I,I)-1
       ENDDO
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C        Subroutines for Time Integration of ODEs
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNTI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generate the equations for continuing fixed points.
C
      DIMENSION U(*),UOLD(*),ICP(*),PAR(*),F(*),RAP(*)
      DIMENSION DFDU(NDIM,*),DFDP(NDIM,*)
C
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
       TOLD=RAP(15)
       DT=PAR(ICP(1))-TOLD
C
       DO I=1,NDIM
         DFDP(I,ICP(1))=F(I)
         F(I)= DT*F(I) - U(I) + UOLD(I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       DO I=1,NDIM
         DO J=1,NDIM
           DFDU(I,J)= DT*DFDU(I,J)
         ENDDO
         DFDU(I,I)= DFDU(I,I) - 1.d0
       ENDDO
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Subroutines for the Continuation of Hopf Bifurcation Points (Maps)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for the 2-parameter continuation of Hopf
C bifurcation points for maps.
C
      DIMENSION IAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function.
C
       CALL FFHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFHD(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFHD(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))+EP
C
       CALL FFHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
C
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))-EP
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION U(*),UOLD(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       NDM2=2*NDM
C
       THTA=U(NDIM-1)
       S1=DSIN(THTA)
       C1=DCOS(THTA)
       PAR(ICP(2))=U(NDIM)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DFDP)
       DO I=1,NDM
         F(I)=F(I)-U(I)
         DFDU(I,I)=DFDU(I,I)-C1
       ENDDO
C
       DO I=1,NDM
         F(NDM+I)=S1*U(NDM2+I)
         F(NDM2+I)=-S1*U(NDM+I)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+DFDU(I,J)*U(NDM2+J)
         ENDDO
       ENDDO
C
       F(NDIM-1)=-1
C
       DO I=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+I)*U(NDM+I)+U(NDM2+I)*U(NDM2+I)
       ENDDO
C
       F(NDIM)=0.d0
C
       DO I=1,NDM
         F(NDIM)=F(NDIM)+UOLD(NDM2+I)*U(NDM+I)-UOLD(NDM+I)*U(NDM2+I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNHD(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
      LOGICAL FOUND
C
C Generates starting data for the continuation of Hopf bifurcation
C points for maps.
C
      DIMENSION U(*),PAR(*),ICP(*),IAP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      DIMENSION SMAT(2*NDIMX,2*NDIMX),IR(2*NDIMX),IC(2*NDIMX+1)
      DIMENSION V(NDIMX),F(NDIMX)
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
       THTA=PI(2.d0)/PAR(11)
       S1=DSIN(THTA)
       C1=DCOS(THTA)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DFP)
C
       NDM2=2*NDM
       DO I=1,NDM2
         DO J=1,NDM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDM
         SMAT(I,NDM+I)=S1
       ENDDO
C
       DO I=1,NDM
         SMAT(NDM+I,I)=-S1
       ENDDO
C
       DO I=1,NDM
         DO J=1,NDM
           SMAT(I,J)=DFU((J-1)*NDM+I)
           SMAT(NDM+I,NDM+J)=DFU((J-1)*NDM+I)
         ENDDO
         SMAT(I,I)=SMAT(I,I)-C1
         SMAT(NDM+I,NDM+I)=SMAT(NDM+I,NDM+I)-C1
       ENDDO
       CALL NLVC(NDM2,2*NDIMX,2,SMAT,V,IR,IC)
       CALL NRMLZ(NDM2,V)
C
       DO I=1,NDM2
         U(NDM+I)=V(I)
       ENDDO
C
       U(NDIM-1)=THTA
       U(NDIM)=PAR(ICP(2))
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Subroutines for the Continuation of Hopf Bifurcation Points (ODE)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for the 2-parameter continuation of Hopf
C bifurcation points in ODE.
C
      DIMENSION IAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function.
C
       CALL FFHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFHB(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFHB(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))+EP
C
       CALL FFHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
C
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))-EP
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION U(*),UOLD(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       NDM2=2*NDM
C
       ROM=U(NDIM-1)
       PAR(11)=ROM*PI(2.d0)
       PAR(ICP(2))=U(NDIM)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DFDP)
C
       DO I=1,NDM
         F(NDM+I)=U(NDM2+I)
         F(NDM2+I)=-U(NDM+I)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+ROM*DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+ROM*DFDU(I,J)*U(NDM2+J)
         ENDDO
       ENDDO
C
       F(NDIM-1)=-1
C
       DO I=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+I)*U(NDM+I)+U(NDM2+I)*U(NDM2+I)
       ENDDO
C
       F(NDIM)=0.d0
C
       DO I=1,NDM
         F(NDIM)=F(NDIM)+UOLD(NDM2+I)*(U(NDM+I)-UOLD(NDM+I)) -
     *  UOLD(NDM+I)*(U(NDM2+I)-UOLD(NDM2+I))
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNHB(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
      LOGICAL FOUND
C
C Generates starting data for the 2-parameter continuation of
C Hopf bifurcation point (ODE).
C
      DIMENSION U(*),PAR(*),ICP(*),IAP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      DIMENSION SMAT(2*NDIMX,2*NDIMX),IR(2*NDIMX),IC(2*NDIMX)
      DIMENSION V(NDIMX),F(NDIMX)
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
       PERIOD=PAR(11)
       ROM=PERIOD/PI(2.d0)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DFP)
C
       NDM2=2*NDM
       DO I=1,NDM2
         DO J=1,NDM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDM
         SMAT(I,NDM+I)=1
       ENDDO
C
       DO I=1,NDM
         SMAT(NDM+I,I)=-1
       ENDDO
C
       DO I=1,NDM
         DO J=1,NDM
           SMAT(I,J)=ROM*DFU((J-1)*NDM+I)
           SMAT(NDM+I,NDM+J)=ROM*DFU((J-1)*NDM+I)
         ENDDO
       ENDDO
       CALL NLVC(NDM2,2*NDIMX,2,SMAT,V,IR,IC)
       CALL NRMLZ(NDM2,V)
C
       DO I=1,NDM2
         U(NDM+I)=V(I)
       ENDDO
C
       U(NDIM-1)=ROM
       U(NDIM)=PAR(ICP(2))
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   Subroutines for the Continuation of Hopf Bifurcation Points (Waves)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for the 2-parameter continuation of a
C bifurcation to a traveling wave.
C
      DIMENSION IAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function.
C
       CALL FFHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFHW(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFHW(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))+EP
C
       CALL FFHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
C
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
C
       PAR(ICP(1))=PAR(ICP(1))-EP
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION U(*),UOLD(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       NDM2=2*NDM
C
       ROM=U(NDIM-1)
       PAR(ICP(2))=U(NDIM)
       IJAC=1
       CALL FNWS(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
       DO I=1,NDM
         F(NDM+I)=U(NDM2+I)
         F(NDM2+I)=-U(NDM+I)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+ROM*DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+ROM*DFDU(I,J)*U(NDM2+J)
         ENDDO
       ENDDO
C
       F(NDIM-1)=-1
C
       DO I=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+I)*U(NDM+I)+U(NDM2+I)*U(NDM2+I)
       ENDDO
C
       F(NDIM)=0.d0
C
       DO I=1,NDM
         F(NDIM)=F(NDIM)+UOLD(NDM2+I)*(U(NDM+I)-UOLD(NDM+I)) -
     *   UOLD(NDM+I)*(U(NDM2+I)-UOLD(NDM2+I))
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNHW(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
      LOGICAL FOUND
C
C Generates starting data for the continuation of a bifurcation to a
C traveling wave.
C
      DIMENSION U(*),PAR(*),ICP(*),IAP(*)
C Local (Can't use BLLOC here.)
      DIMENSION DFU(NX2),DFP(NXP)
      DIMENSION SMAT(2*NDIMX,2*NDIMX),IR(2*NDIMX),IC(2*NDIMX)
      DIMENSION V(NDIMX),F(NDIMX)
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
       IJAC=1
       PERIOD=PAR(11)
       ROM=PERIOD/PI(2.d0)
       CALL FNWS(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFU,DFP)
C
       NDM2=2*NDM
       DO I=1,NDM2
         DO J=1,NDM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDM
         SMAT(I,NDM+I)=1
       ENDDO
C
       DO I=1,NDM
         SMAT(NDM+I,I)=-1
       ENDDO
C
       DO I=1,NDM
         DO J=1,NDM
           SMAT(I,J)=ROM*DFU((J-1)*NDM+I)
           SMAT(NDM+I,NDM+J)=ROM*DFU((J-1)*NDM+I)
         ENDDO
       ENDDO
       CALL NLVC(NDM2,2*NDIMX,2,SMAT,V,IR,IC)
       CALL NRMLZ(NDM2,V)
C
       DO I=1,NDM2
         U(NDM+I)=V(I)
       ENDDO
C
       U(NDIM-1)=ROM
       U(NDIM)=PAR(ICP(2))
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C          Periodic Solutions and Fixed Period Orbits
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNPS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates the equations for the continuation of periodic orbits.
C
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C
C Generate the function.
C
       IF(ICP(2).EQ.11)THEN
C          **Variable period continuation
           CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           PERIOD=PAR(11)
           DO I=1,NDIM
             DFDP(I,11)=F(I)
             F(I)=PERIOD*DFDP(I,11)
           ENDDO
           IF(IJAC.EQ.0)RETURN
C          **Generate the Jacobian.
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
             DFDP(I,ICP(1))=PERIOD*DFDP(I,ICP(1))
           ENDDO
       ELSE
C          **Fixed period continuation
           PERIOD=PAR(11)
           CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           DO I=1,NDIM
             F(I)=PERIOD*F(I)
           ENDDO
           IF(IJAC.EQ.0)RETURN
C          **Generate the Jacobian.
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
             DO J=1,2
               DFDP(I,ICP(J))=PERIOD*DFDP(I,ICP(J))
             ENDDO
           ENDDO
       ENDIF
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCPS(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
C
      DIMENSION PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
C
       DO I=1,NDIM
         F(I)=U0(I)-U1(I)
       ENDDO
C
C Rotations
       IF(IRTN.NE.0)THEN
         DO I=1,NDIM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
C
       IF(IJAC.EQ.0)RETURN
C
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         DBC(I,I)=1
         DBC(I,NDIM+I)=-1
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICPS(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
      DIMENSION ICP(*),PAR(*)
C
       F(1)=0.d0
       DO I=1,NDIM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       NN=NDIM+NPARX
       DO I=1,NN
         DINT(1,I)=0.d0
       ENDDO
C
       DO I=1,NDIM
          DINT(1,I)=UPOLD(I)
       ENDDO
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE PDBLE(IAP,RAP,NDIM,NTST,NCOL,NDX,UPS,UDOTPS,TM,PAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Preprocesses restart data for switching branches at a period doubling
C
C
      INCLUDE 'auto.h'
C
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
      DIMENSION TM(*),UPS(NDX,*),UDOTPS(NDX,*),PAR(*)
C
       PAR(11)=2.d0*PAR(11)
       IF(IRTN.NE.0)PAR(19)=2.d0*PAR(19)
C
       DO I=1,NTST
         TM(I)=.5d0*TM(I)
         TM(NTST+I)=.5d0+TM(I)
       ENDDO
C
       TM(2*NTST+1)=1
C
       DO J=1,NTST+1
         DO I1=1,NDIM
          DO I2=1,NCOL
           I=(I2-1)*NDIM+I1
              UPS(NTST+J,I)=   UPS(NTST+1,I1)+   UPS(J,I)-   UPS(1,I1)
           UDOTPS(NTST+J,I)=UDOTPS(NTST+1,I1)+UDOTPS(J,I)-UDOTPS(1,I1)
          ENDDO
         ENDDO
       ENDDO
C
       NTST=2*NTST
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNPS(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
C Generates starting data for the continuation of a branch of periodic
C solutions from a Hopf bifurcation point.
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      DIMENSION U(NDIMX),F(NDIMX),IR(2*NDIMX),IC(2*NDIMX)
      DIMENSION SMAT(2*NDIMX,2*NDIMX),RNLLV(2*NDIMX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       PERIOD=PAR(11)
       TPI=PI(2.d0)
       RIMHB=TPI/PERIOD
       NTSR=NTST
       NCOLRS=NCOL
C
       NDIM2=2*NDIM
       DO I=1,NDIM2
         DO J=1,NDIM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         SMAT(I,I)=-RIMHB
         SMAT(NDIM+I,NDIM+I)=RIMHB
       ENDDO
C
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,1,F,DFU,DFP)
C
       DO I=1,NDIM
         DO J=1,NDIM
           SMAT(I,NDIM+J)=DFU((J-1)*NDIM+I)
           SMAT(NDIM+I,J)=DFU((J-1)*NDIM+I)
         ENDDO
       ENDDO
C
       CALL NLVC(NDIM2,2*NDIMX,2,SMAT,RNLLV,IR,IC)
       CALL NRMLZ(NDIM2,RNLLV)
C
C Generate the (initially uniform) mesh.
C
       CALL MSH(IAP,RAP,TM)
       DT=1.d0/NTST
C
       DO J=1,NTST+1
         T=TM(J)
         S=DSIN(TPI*T)
         C=DCOS(TPI*T)
         DO K=1,NDIM
           UDOTPS(J,K)=S*RNLLV(K)+C*RNLLV(NDIM+K)
           UPOLDP(J,K)=C*RNLLV(K)-S*RNLLV(NDIM+K)
           UPS(J,K)=U(K)
         ENDDO
       ENDDO
C
       DO I=1,NCOL-1
         DO J=1,NTST
           T=TM(J)+I*( TM(J+1)-TM(J) )/NCOL
           S=DSIN(TPI*T)
           C=DCOS(TPI*T)
           DO  K=1,NDIM
             K1=I*NDIM+K
             UDOTPS(J,K1)=S*RNLLV(K)+C*RNLLV(NDIM+K)
             UPOLDP(J,K1)=C*RNLLV(K)-S*RNLLV(NDIM+K)
             UPS(J,K1)=U(K)
           ENDDO
         ENDDO
       ENDDO
C
       RLDOT(1)=0.d0
       RLDOT(2)=0.d0
C
       DO I=1,NTST
         DTM(I)=DT
       ENDDO
C
       CALL SCALEB(IAP,ICP,NDX,UDOTPS,RLDOT,DTM,THL,THU)
C
       NODIR=-1
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNPB(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
C Generates starting data for the continuation of a branch of periodic
C solutions from a Hopf bifurcation point, when the user has supplied
C BCND, ICND, and period-scaled F in FUNC.
C The difference with STPNPS is that the user period-scaling of F must
C be taken into account.
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      DIMENSION U(NDIMX),F(NDIMX),IR(2*NDIMX),IC(2*NDIMX)
      DIMENSION SMAT(2*NDIMX,2*NDIMX),RNLLV(2*NDIMX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       PERIOD=PAR(11)
       TPI=PI(2.d0)
       RIMHB=TPI/PERIOD
       NTSR=NTST
       NCOLRS=NCOL
C
       NDIM2=2*NDIM
       DO I=1,NDIM2
         DO J=1,NDIM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         SMAT(I,I)=-RIMHB
         SMAT(NDIM+I,NDIM+I)=RIMHB
       ENDDO
C
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,1,F,DFU,DFP)
C
       DO I=1,NDIM
         DO J=1,NDIM
C Note that the user period-scaling in FUNC is taken into account:
           SMAT(I,NDIM+J)=DFU((J-1)*NDIM+I)/PAR(11)
           SMAT(NDIM+I,J)=DFU((J-1)*NDIM+I)/PAR(11)
         ENDDO
       ENDDO
C
       CALL NLVC(NDIM2,2*NDIMX,2,SMAT,RNLLV,IR,IC)
       CALL NRMLZ(NDIM2,RNLLV)
C
C Generate the (initially uniform) mesh.
C
       CALL MSH(IAP,RAP,TM)
       DT=1.d0/NTST
C
       DO J=1,NTST+1
         T=TM(J)
         S=DSIN(TPI*T)
         C=DCOS(TPI*T)
         DO K=1,NDIM
           UDOTPS(J,K)=S*RNLLV(K)+C*RNLLV(NDIM+K)
           UPOLDP(J,K)=C*RNLLV(K)-S*RNLLV(NDIM+K)
           UPS(J,K)=U(K)
         ENDDO
       ENDDO
C
       DO I=1,NCOL-1
         DO J=1,NTST
           T=TM(J)+I*( TM(J+1)-TM(J) )/NCOL
           S=DSIN(TPI*T)
           C=DCOS(TPI*T)
           DO  K=1,NDIM
             K1=I*NDIM+K
             UDOTPS(J,K1)=S*RNLLV(K)+C*RNLLV(NDIM+K)
             UPOLDP(J,K1)=C*RNLLV(K)-S*RNLLV(NDIM+K)
             UPS(J,K1)=U(K)
           ENDDO
         ENDDO
       ENDDO
C
       RLDOT(1)=0.d0
       RLDOT(2)=0.d0
C
       DO I=1,NTST
         DTM(I)=DT
       ENDDO
C
       CALL SCALEB(IAP,ICP,NDX,UDOTPS,RLDOT,DTM,THL,THU)
C
       NODIR=-1
C
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C          Travelling Wave Solutions to Parabolic PDEs
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
C Sets up equations for the continuation of spatially homogeneous
C solutions to parabolic systems, for the purpose of finding
C bifurcations to travelling wave solutions.
C
      DIMENSION IAP(*)
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function.
C
       NDM2=NDM/2
       CALL FFWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,NDM2,
     *      DFU,DFP)
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,
     *                NDM,DFU,DFP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DIMENSION DFU(NDM,*),DFP(NDM,*)
C
       NFPR=IAP(29)
C
       C=PAR(10)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFU,DFP)
C
       DO I=1,NDM
         F(NDM+I)=-( C*U(NDM+I) + F(I) )/PAR(14+I)
         F(I)=U(NDM+I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       DO I=1,NDM
         DO J=1,NDM
           DFDU(I,J)        =0.d0
           DFDU(I,J+NDM)    =0.d0
           DFDU(I+NDM,J)    =-DFU(I,J)/PAR(14+I)
           DFDU(I+NDM,J+NDM)=0.d0
         ENDDO
         DFDU(I,I+NDM)     =1
         DFDU(I+NDM,I+NDM)=-C/PAR(14+I)
         IF(ICP(1).LT.10)THEN
           DFDP(I,ICP(1))    =0.d0
           DFDP(I+NDM,ICP(1))=-DFP(I,ICP(1))/PAR(14+I)
         ENDIF
         IF(NFPR.GT.1.AND.ICP(2).LT.10)THEN
           DFDP(I,ICP(2))    =0.d0
           DFDP(I+NDM,ICP(2))=-DFP(I,ICP(2))/PAR(14+I)
         ENDIF
       ENDDO
C
C Derivative with respect to the wave speed.
C
       DO I=1,NDM
         DFDP(I,10)    =0.d0
         DFDP(I+NDM,10)=-U(NDM+I)/PAR(14+I)
       ENDDO
C
C Derivatives with respect to the diffusion coefficients.
C
       DO J=1,NDM
         DO I=1,NDM
           DFDP(I,14+J)    =0.d0
           DFDP(I+NDM,14+J)=0.d0
         ENDDO
         DFDP(J+NDM,14+J)=-F(J+NDM)/PAR(14+J)
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FNWP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Equations for the continuation of traveling waves.
C
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
C
C Generate the function and Jacobian.
C
       IF(ICP(2).EQ.11)THEN
C          **Variable wave length
           CALL FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           PERIOD=PAR(11)
           DO I=1,NDIM
             DFDP(I,11)=F(I)
             F(I)=PERIOD*F(I)
           ENDDO
           IF(IJAC.EQ.0)RETURN
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
           ENDDO
           DO I=1,NDIM
             DFDP(I,ICP(1))=PERIOD*DFDP(I,ICP(1))
           ENDDO
       ELSE
C          **Fixed wave length
           CALL FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           PERIOD=PAR(11)
           DO I=1,NDIM
             F(I)=PERIOD*F(I)
           ENDDO
           IF(IJAC.EQ.0)RETURN
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
           ENDDO
           DO I=1,NDIM
             DO J=1,2
               DFDP(I,ICP(J))=PERIOD*DFDP(I,ICP(J))
             ENDDO
           ENDDO
       ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNWP(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
C Generates starting data for the continuation of a branch of periodic
C solutions starting from a Hopf bifurcation point (Waves).
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
C Local (Can't use BLLOC here.)
      DIMENSION DFU(NX2),DFP(NXP)
      DIMENSION U(NDIMX),F(NDIMX),IR(2*NDIMX),IC(2*NDIMX)
      DIMENSION SMAT(2*NDIMX,2*NDIMX),RNLLV(2*NDIMX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       PERIOD=PAR(11)
       TPI=PI(2.d0)
       RIMHB=TPI/PERIOD
       NTSR=NTST
       NCOLRS=NCOL
C
       NDIM2=2*NDIM
       DO I=1,NDIM2
         DO J=1,NDIM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         SMAT(I,I)=-RIMHB
         SMAT(NDIM+I,NDIM+I)=RIMHB
       ENDDO
C
       IJAC=1
       CALL FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFU,DFP)
C
       DO I=1,NDIM
         DO J=1,NDIM
           SMAT(I,NDIM+J)=DFU((J-1)*NDIM+I)
           SMAT(NDIM+I,J)=DFU((J-1)*NDIM+I)
         ENDDO
       ENDDO
C
       CALL NLVC(NDIM2,2*NDIMX,2,SMAT,RNLLV,IR,IC)
       CALL NRMLZ(NDIM2,RNLLV)
C
C Generate the (initially uniform) mesh.
C
       CALL MSH(IAP,RAP,TM)
       DT=1.d0/NTST
C
       DO J=1,NTST+1
         T=TM(J)
         S=DSIN(TPI*T)
         C=DCOS(TPI*T)
         DO K=1,NDIM
           UDOTPS(J,K)=S*RNLLV(K)+C*RNLLV(NDIM+K)
           UPOLDP(J,K)=C*RNLLV(K)-S*RNLLV(NDIM+K)
           UPS(J,K)=U(K)
         ENDDO
       ENDDO
C
       DO I=1,NCOL-1
         DO J=1,NTST
           T=TM(J)+I*( TM(J+1)-TM(J) )/NCOL
           S=DSIN(TPI*T)
           C=DCOS(TPI*T)
           DO K=1,NDIM
             K1=I*NDIM+K
             UDOTPS(J,K1)=S*RNLLV(K)+C*RNLLV(NDIM+K)
             UPOLDP(J,K1)=C*RNLLV(K)-S*RNLLV(NDIM+K)
             UPS(J,K1)=U(K)
           ENDDO
         ENDDO
       ENDDO
C
       RLDOT(1)=0.d0
       RLDOT(2)=0.d0
C
       DO I=1,NTST
         DTM(I)=DT
       ENDDO
C
       CALL SCALEB(IAP,ICP,NDX,UDOTPS,RLDOT,DTM,THL,THU)
C
       NODIR=-1
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C             Parabolic PDEs : Stationary States
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNSP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
C Generates the equations for taking one time step (Implicit Euler).
C
      DIMENSION IAP(*),U(*),UOLD(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function and Jacobian.
C
       CALL FFSP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,
     *           NDM,DFU,DFP)
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFSP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,
     *                NDM,DFU,DFP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*),DFU(NDM,*),DFP(NDM,*)
C
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F(NDM+1),DFU,DFP)
C
       PERIOD=PAR(11)
       DO I=1,NDM
         F(I)    = PERIOD*U(NDM+I)
         F(NDM+I)=-PERIOD*F(NDM+I)/PAR(14+I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       DO I=1,NDM
         DO J=1,NDM
           DFDU(I,J)        =0.d0
           DFDU(I,J+NDM)    =0.d0
           DFDU(I+NDM,J)    =-PERIOD*DFU(I,J)/PAR(14+I)
           DFDU(I+NDM,J+NDM)=0.d0
         ENDDO
         DFDU(I,I+NDM)     =PERIOD
         IF(ICP(1).EQ.11)THEN
           DFDP(I,ICP(1))    = F(I)/PERIOD
           DFDP(NDM+I,ICP(1))= F(NDM+I)/PERIOD
         ELSEIF(ICP(1).EQ.14+I)THEN
           DFDP(I,ICP(1))    = 0.d0
           DFDP(NDM+I,ICP(1))=-F(NDM+I)/PAR(14+I)
         ELSEIF(ICP(1).NE.11 .AND. 
     *          .NOT. (ICP(1).GT.14 .AND. ICP(1).LE.14+NDM) )THEN
           DFDP(I,ICP(1))    =0.d0
           DFDP(I+NDM,ICP(1))=-PERIOD*DFP(I,ICP(1))/PAR(14+I)
         ENDIF
       ENDDO
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C            Time Evolution of Parabolic PDEs
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNPE(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
C Generates the equations for taking one time step (Implicit Euler).
C
      DIMENSION IAP(*),U(*),UOLD(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
C
C Generate the function and Jacobian.
C
       CALL FFPE(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,
     *           NDM,DFU,DFP)
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFPE(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,
     *                NDM,DFU,DFP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*),DFU(NDM,*),DFP(NDM,*)
C
       DS=RAP(1)
       DSMIN=RAP(2)
C
       PERIOD=PAR(11)
       T=PAR(ICP(1))
       RLOLD=RAP(15)
       DT=T-RLOLD
       IF(DABS(DT).LT.DSMIN)DT=DS
C
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F(NDM+1),DFU,DFP)
C
       DO I=1,NDM
         F(I)=PERIOD*U(NDM+I)
         F(NDM+I)=PERIOD*( (U(I)-UOLD(I))/DT - F(NDM+I) )/PAR(14+I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       DO I=1,NDM
         DO J=1,NDM
           DFDU(I,J)        =0.d0
           DFDU(I,J+NDM)    =0.d0
           DFDU(I+NDM,J)    =-PERIOD*DFU(I,J)/PAR(14+I)
           DFDU(I+NDM,J+NDM)=0.d0
         ENDDO
         DFDU(I,I+NDM)     =PERIOD
         DFDU(I+NDM,I)     =DFDU(I+NDM,I) + PERIOD/(DT*PAR(14+I))
         DFDP(I,ICP(1))    =0.d0
         DFDP(I+NDM,ICP(1))=-PERIOD*(U(I)-UOLD(I))/(DT**2*PAR(14+I))
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICPE(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
C Dummy integral condition subroutine for parabolic systems.
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    Subroutines for the Continuation of Folds for Periodic Solution
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNPL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
      DIMENSION IAP(*)
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FFPL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFPL(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFPL(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFPL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFPL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       PERIOD=PAR(11)
       BETA=PAR(12)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
C
       IPS=IAP(2)
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
         ENDDO
           IF(ICP(3).EQ.11)THEN
C            ** Variable period
           F(NDM+I)=PERIOD*F(NDM+I)+BETA*F(I)
         ELSE
C            ** Fixed period
           F(NDM+I)=PERIOD*F(NDM+I)+BETA*DFDP(I,ICP(2))
         ENDIF
         F(I)=PERIOD*F(I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCPL(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
C Boundary conditions for continuing folds (Periodic solutions)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
C
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
C
       DO I=1,NDIM
         F(I)=U0(I)-U1(I)
       ENDDO
C
C Rotations
       IF(IRTN.NE.0)THEN
         NDM=IAP(23)
         DO I=1,NDM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
C
       IF(IJAC.EQ.0)RETURN
C
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         DBC(I,I)=1
         DBC(I,NDIM+I)=-1
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICPL(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
C Integral conditions for continuing folds (Periodic solutions)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
C
       NDM=IAP(23)
C
       F(1)=0.d0
       F(2)=0.d0
       F(3)=PAR(12)**2 - PAR(13)
C
       DO I=1,NDM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
         F(2)=F(2)+U(NDM+I)*UPOLD(I)
         F(3)=F(3)+U(NDM+I)*U(NDM+I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       NN=NDIM+NPARX
       DO I=1,NINT
         DO J=1,NN
           DINT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDM
         DINT(1,I)=UPOLD(I)
         DINT(2,NDM+I)=UPOLD(I)
         DINT(3,NDM+I)=2.d0*U(NDM+I)
       ENDDO
C
       DINT(3,NDIM+12)=2.d0*PAR(12)
       DINT(3,NDIM+13)=-1.d0
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNPL(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Generates starting data for the 2-parameter continuation of folds
C on a branch of periodic solutions.
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
C Local
      DIMENSION TEMP(7),ICPRS(NPARX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
       IBR=IAP(30)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       READ(3,*)IBR,NTOT1,ITP1,LAB1,NFPR1,ISW1,NTPL1,NAR1,NSKIP1,
     * NTSR,NCOLRS,NPARR
       IAP(30)=IBR
       NRSP1=NTSR+1
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)TEMP(I),(UPS(J,K),K=K1,K2)
         ENDDO
         TM(J)=TEMP(1)
       ENDDO
       READ(3,*)TM(NRSP1),(UPS(NRSP1,K),K=1,NDM)
C
       READ(3,*)ICPRS(1),ICPRS(2)
       READ(3,*)RD1,RD2
C
C Read U-dot (derivative with respect to arclength).
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)(UDOTPS(J,K),K=K1,K2)
         ENDDO
       ENDDO
       READ(3,*)(UDOTPS(NRSP1,K),K=1,NDM)
C
C Read the parameter values.
C
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(6,100)NPARR
 100     FORMAT(' Warning : NPARX too small for restart data : ',/,
     *          ' PAR(i) set to zero, for i > ',I3)
       ENDIF
       READ(3,*)(PAR(I),I=1,NPARR)
C
C Complement starting data
         PAR(12)=0.d0
         PAR(13)=0.d0
         IF(ICP(3).EQ.11)THEN
C          Variable period
           RLDOT(1)=RD1
           RLDOT(2)=0.d0
           RLDOT(3)=RD2
           RLDOT(4)=0.d0
C          Variable period
         ELSE
C          Fixed period
           RLDOT(1)=RD1
           RLDOT(2)=RD2
           RLDOT(3)=0.d0
           RLDOT(4)=0.d0
         ENDIF
         DO J=1,NTSR
           DO I=1,NCOLRS
             K1=(I-1)*NDIM+NDM+1
             K2=I*NDIM
             DO K=K1,K2
               UPS(J,K)=0.d0
               UDOTPS(J,K)=0.d0
             ENDDO
           ENDDO
         ENDDO
         K1=NDM+1
         DO K=K1,NDIM
           UPS(NRSP1,K)=0.d0
           UDOTPS(NRSP1,K)=0.d0
         ENDDO
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       NODIR=0
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   Subroutines for the Continuation of Period Doubling Bifurcations
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNPD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
      DIMENSION IAP(*)
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FFPD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFPD(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFPD(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFPD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFPD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       PERIOD=PAR(11)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DFDP)
C
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
        ENDDO
         F(I)=PERIOD*F(I)
         F(NDM+I)=PERIOD*F(NDM+I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCPD(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
C
C Generate boundary conditions for the 2-parameter continuation
C of period doubling bifurcations.
C
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
C
       NDM=IAP(23)
C
       DO I=1,NDM
         F(I)=U0(I)-U1(I)
         F(NDM+I)=U0(NDM+I)+U1(NDM+I)
       ENDDO
C
C Rotations
       IF(IRTN.NE.0)THEN
         DO I=1,NDM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
C
       IF(IJAC.EQ.0)RETURN
C
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         DBC(I,I)=1
         IF(I.LE.NDM) THEN
           DBC(I,NDIM+I)=-1
         ELSE
           DBC(I,NDIM+I)=1
         ENDIF
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICPD(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
C
       NDM=IAP(23)
C
       F(1)=0.d0
       F(2)=-PAR(13)
C
       DO I=1,NDM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
         F(2)=F(2)+U(NDM+I)*U(NDM+I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       NN=NDIM+NPARX
       DO I=1,NINT
         DO J=1,NN
           DINT(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDM
         DINT(1,I)=UPOLD(I)
         DINT(2,NDM+I)=2.d0*U(NDM+I)
       ENDDO
C
       DINT(2,NDIM+13)=-1.d0
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNPD(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Generates starting data for the 2-parameter continuation of
C period-doubling bifurcations on a branch of periodic solutions.
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
C Local
      DIMENSION TEMP(7),ICPRS(NPARX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
       IBR=IAP(30)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       READ(3,*)IBR,NTOT1,ITP1,LAB1,NFPR1,ISW1,NTPL1,NAR1,NSKIP1,
     * NTSR,NCOLRS,NPARR
       IAP(30)=IBR
       NRSP1=NTSR+1
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)TEMP(I),(UPS(J,K),K=K1,K2)
         ENDDO
         TM(J)=TEMP(1)
       ENDDO
       READ(3,*)TM(NRSP1),(UPS(NRSP1,K),K=1,NDM)
C
       READ(3,*)ICPRS(1),ICPRS(2)
       READ(3,*)RLDOT(1),RLDOT(2)
C
C Read U-dot (derivative with respect to arclength).
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)(UDOTPS(J,K),K=K1,K2)
         ENDDO
       ENDDO
       READ(3,*)(UDOTPS(NRSP1,K),K=1,NDM)
C
C Read the parameter values.
C
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(6,100)NPARR
 100     FORMAT(' Warning : NPARX too small for restart data : ',/,
     *          ' PAR(i) set to zero, for i > ',I3)
       ENDIF
       READ(3,*)(PAR(I),I=1,NPARR)
C
C Complement starting data 
         PAR(13)=0.d0
         RLDOT(3)=0.d0
         DO J=1,NTSR
           DO I=1,NCOLRS
             K1=(I-1)*NDIM+NDM+1
             K2=I*NDIM
             DO K=K1,K2
               UPS(J,K)=0.d0
               UDOTPS(J,K)=0.d0
             ENDDO
           ENDDO
         ENDDO
         K1=NDM+1
         DO K=K1,NDIM
           UPS(NRSP1,K)=0.d0
           UDOTPS(NRSP1,K)=0.d0
         ENDDO
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       NODIR=0
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       Subroutines for the Continuation of Torus Bifurcations
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNTR(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for the 2-parameter continuation of
C torus bifurcations.
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FFTR(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
         CALL FFTR(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFTR(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFTR(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFTR(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       PERIOD=PAR(11)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DFDP)
C
       NDM2=2*NDM
       DO I=1,NDM
         F(NDM+I)=0.d0
         F(NDM2+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+DFDU(I,J)*U(NDM2+J)
         ENDDO
         F(NDM+I)=PERIOD*F(NDM+I)
         F(NDM2+I)=PERIOD*F(NDM2+I)
         F(I)=PERIOD*F(I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCTR(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
C
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
C
       NDM=IAP(23)
C
       NDM2=2*NDM
       THETA=PAR(12)
C
       SS=DSIN(THETA)
       CS=DCOS(THETA)
C
       DO I=1,NDM
         F(I)=U0(I)-U1(I)
         F(NDM+I)= U1(NDM+I) -CS*U0(NDM+I) +SS*U0(NDM2+I)
         F(NDM2+I)=U1(NDM2+I)-CS*U0(NDM2+I)-SS*U0(NDM+I)
       ENDDO
C
C Rotations
       IF(IRTN.NE.0)THEN
         DO I=1,NDM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
C
       IF(IJAC.EQ.0)RETURN
C
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
C
       DO I=1,NDM
         DBC(I,I)=1
         DBC(I,NDIM+I)=-1
         DBC(NDM+I,NDM+I)=-CS
         DBC(NDM+I,NDM2+I)=SS
         DBC(NDM+I,NDIM+NDM+I)=1
         DBC(NDM+I,2*NDIM+12)=CS*U0(NDM2+I)+SS*U0(NDM+I)
         DBC(NDM2+I,NDM+I)=-SS
         DBC(NDM2+I,NDM2+I)=-CS
         DBC(NDM2+I,NDIM+NDM2+I)=1
         DBC(NDM2+I,2*NDIM+12)=SS*U0(NDM2+I)-CS*U0(NDM+I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICTR(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
      DIMENSION IAP(*),U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
      DIMENSION ICP(*),PAR(*)
C
       NDM=IAP(23)
       NDM2=2*NDM
C
       F(1)=0.d0
       F(2)=0.d0
       F(3)=-PAR(13)
C
       DO I=1,NDM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
         F(2)=F(2)+U(NDM+I)*U(NDM2+I)-U(NDM2+I)*U(NDM+I)
         F(3)=F(3)+U(NDM+I)*U(NDM+I) +U(NDM2+I)*U(NDM2+I)
       ENDDO
C
       IF(IJAC.EQ.0)RETURN
C
       NN=NDIM+NPARX
       DO I=1,NINT
         DO J=1,NN
           DINT(I,J)=0.d0
         ENDDO
       ENDDO
C
      DO I=1,NDM
        DINT(1,I)=UPOLD(I)
        DINT(2,NDM+I)=U(NDM2+I)
        DINT(2,NDM2+I)=-U(NDM+I)
        DINT(3,NDM+I)=2*U(NDM+I)
        DINT(3,NDM2+I)=2*U(NDM2+I)
      ENDDO
C
      DINT(3,NDIM+13)=-1
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNTR(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates starting data for the 2-parameter continuation of torus
C bifurcations.
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
C Local
      DIMENSION TEMP(7),ICPRS(NPARX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
       IBR=IAP(30)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       READ(3,*)IBR,NTOT1,ITP1,LAB1,NFPR1,ISW1,NTPL1,NAR1,NSKIP1,
     * NTSR,NCOLRS,NPARR
       IAP(30)=IBR
       NRSP1=NTSR+1
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)TEMP(I),(UPS(J,K),K=K1,K2)
           K2P1=K2+1
           K3=K2+NDM
           DO K=K2P1,K3
             UPS(J,K)    =0.0001*DSIN(TEMP(I))
             UPS(J,K+NDM)=0.0001*DCOS(TEMP(I))
           ENDDO
         ENDDO
         TM(J)=TEMP(1)
       ENDDO
       READ(3,*)TM(NRSP1),(UPS(NRSP1,K),K=1,NDM)
       DO I=1,NDM
         UPS(NRSP1,NDM+I)=0.d0
         UPS(NRSP1,2*NDM+I)=0.d0
       ENDDO
C
       READ(3,*)ICPRS(1),ICPRS(2)
       READ(3,*)RLDOT(1),RLDOT(2)
       RLDOT(3)=0.d0
       RLDOT(4)=0.d0
C
C Read the direction vector and initialize the starting direction.
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)(UDOTPS(J,K),K=K1,K2)
           K2P1=K2+1
           K3=K2+NDM
           DO K=K2P1,K3
             UDOTPS(J,K)=0.d0
             UDOTPS(J,K+NDM)=0.d0
           ENDDO
         ENDDO
       ENDDO
       READ(3,*)(UDOTPS(NRSP1,K),K=1,NDM)
       DO I=1,NDM
         UDOTPS(NRSP1,NDM+I)=0.d0
         UDOTPS(NRSP1,2*NDM+I)=0.d0
       ENDDO
C
C Read the parameter values.
C
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(6,100)NPARR
 100     FORMAT(' Warning : NPARX too small for restart data : ',/,
     *          ' PAR(i) set to zero, for i > ',I3)
       ENDIF
       READ(3,*)(PAR(I),I=1,NPARR)
C
       PAR(13)=0.d0
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       NODIR=0
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C        Subroutines for Optimization of Periodic Solutions
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNPO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for periodic optimization problems.
C
      DIMENSION IAP(*),U(*),UOLD(*),ICP(*),PAR(*),F(*)
      DIMENSION DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      DIMENSION UPOLD(NDIMX)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
C Generate F(UOLD)
C
       CALL FUNI(IAP,RAP,NDM,UOLD,UOLD,ICP,PAR,0,UPOLD,DFDU,DFDP)
       PERIOD=PAR(11)
       DO I=1,NDM
         UPOLD(I)=PERIOD*UPOLD(I)
       ENDDO
C
C Generate the function.
C
      CALL FFPO(IAP,RAP,NDIM,U,UOLD,UPOLD,ICP,PAR,F,NDM,DFU,DFP)
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
        CALL FFPO(IAP,RAP,NDIM,UU1,UOLD,UPOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        CALL FFPO(IAP,RAP,NDIM,UU2,UOLD,UPOLD,ICP,PAR,FF2,NDM,DFU,DFP)
        DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
        ENDDO
      ENDDO
C
      DO I=1,NFPR
        PAR(ICP(I))=PAR(ICP(I))+EP
        CALL FFPO(IAP,RAP,NDIM,U,UOLD,UPOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
        ENDDO
        PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFPO(IAP,RAP,NDIM,U,UOLD,UPOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
      DIMENSION IAP(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
      DIMENSION U(*),UOLD(*),UPOLD(*)
C Local
      DIMENSION DFU(NDIMX),DFP(NPARX)
C
       PERIOD=PAR(11)
       RKAPPA=PAR(13)
       GAMMA =PAR(14)
C
       DO I=1,NDM
         DO J=1,NPARX
           DFDP(I,J)=0.d0
         ENDDO
       ENDDO
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DFDP)
       DO I=1,NPARX
        DFP(I)=0.d0
       ENDDO
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,1,FOP,DFU,DFP)
C
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
         ENDDO
         F(I)=PERIOD*F(I)
         F(NDM+I)=PERIOD*F(NDM+I)+ RKAPPA*UPOLD(I) + GAMMA*DFU(I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCPO(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
C
C Generates the boundary conditions for periodic optimization problems.
C
      DIMENSION IAP(*),U0(*),U1(*),F(NBC),ICP(*),PAR(*),DBC(NBC,*)
C
      NFPR=IAP(29)
C
      DO I=1,NBC
        F(I)=U0(I)-U1(I)
      ENDDO
C
C Rotations
       IF(IRTN.NE.0)THEN
         NBC0=IAP(24)
         DO I=1,NBC0
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
C
       IF(IJAC.EQ.0)RETURN
C
      DO I=1,NBC
        DO J=1,2*NDIM
         DBC(I,J)=0.d0
        ENDDO
        DBC(I,I)=1.D0
        DBC(I,NDIM+I)=-1.d0
        DO J=1,NFPR
          DBC(I,2*NDIM+ICP(J))=0.d0
        ENDDO
      ENDDO
 
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICPO(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates integral conditions for periodic optimization problems.
C
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
      ALLOCATABLE F1(:),F2(:),DNT(:,:)
      ALLOCATE(DNT(NINT,NDIM+NPARX))
C
       NDM=IAP(23)
       NNT0=IAP(25)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FIPO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *  UPOLD,F,DNT,NDM,DFU,DFP)
C
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DNT)
         RETURN
       ENDIF
C
C Generate the Jacobian.
C
       ALLOCATE(F1(NINT),F2(NINT))
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
         CALL FIPO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU1,UOLD,UDOT,
     *    UPOLD,F1,DNT,NDM,DFU,DFP)
         CALL FIPO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU2,UOLD,UDOT,
     *    UPOLD,F2,DNT,NDM,DFU,DFP)
         DO J=1,NINT
           DINT(J,I)=(F2(J)-F1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIPO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *    UPOLD,F1,DNT,NDM,DFU,DFP)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(F1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
       DEALLOCATE(DNT,F1,F2)
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FIPO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,
     * U,UOLD,UDOT,UPOLD,FI,DINT,NDMT,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
      POINTER NRTN(:)
      COMMON /BLRTN/ IRTN,NRTN
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),FI(*),DINT(NNT0,*)
      DIMENSION DFDU(NDMT,NDMT),DFDP(NDMT,*)
C
C Local
      DIMENSION DFU(NDIMX),DFP(NPARX),F(NDIMX)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
       FI(1)=0.d0
       DO I=1,NDM
         FI(1)=FI(1)+(U(I)-UOLD(I))*UPOLD(I)
       ENDDO
C
       DO I=1,NPARX
        DFP(I)=0.d0
       ENDDO
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,2,FOP,DFU,DFP)
       FI(2)=PAR(10)-FOP
C
       FI(3)=PAR(13)**2+PAR(14)**2-PAR(12)
       DO I=1,NDM
         FI(3)=FI(3)+U(NDM+I)**2
       ENDDO
C
       DO I=1,NDM
         DO J=1,NPARX
           DFDP(I,J)=0.d0
         ENDDO
       ENDDO
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
C
       DO L=4,NINT
         INDX=ICP(NFPR+L-3)
         IF(INDX.EQ.11)THEN
           FI(L)=-PAR(14)*DFP(INDX) - PAR(20+INDX)
           DO I=1,NDM
             FI(L)=FI(L)+F(I)*U(NDM+I)
           ENDDO
         ELSE
           FI(L)=-PAR(14)*DFP(INDX) - PAR(20+INDX)
           DO I=1,NDM
             FI(L)=FI(L)+PAR(11)*DFDP(I,INDX)*U(NDM+I)
           ENDDO
         ENDIF
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNPO(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Generates starting data for optimization of periodic solutions.
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
C Local
      DIMENSION TEMP(7),ICPRS(NPARX),U(NDIMX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
       IBR=IAP(30)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       READ(3,*)IBR,NTOT1,ITP1,LAB1,NFPR1,ISW1,NTPL1,NAR1,NSKIP1,
     * NTSR,NCOLRS,NPARR
       IAP(30)=IBR
       NRSP1=NTSR+1
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)TEMP(I),(UPS(J,K),K=K1,K2)
         ENDDO
         TM(J)=TEMP(1)
       ENDDO
       READ(3,*)TM(NRSP1),(UPS(NRSP1,K),K=1,NDM)
       DO J=1,NTSR
         DTM(J)=TM(J+1)-TM(J)
       ENDDO
C
       READ(3,*)ICPRS(1),ICPRS(2)
       READ(3,*)RLD1,RLD2
C
C Read U-dot (derivative with respect to arclength).
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)(UDOTPS(J,K),K=K1,K2)
         ENDDO
       ENDDO
       READ(3,*)(UDOTPS(NRSP1,K),K=1,NDM)
C
C Read the parameter values.
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(6,100)NPARR
 100     FORMAT(' Warning : NPARX too small for restart data : ',/,
     *          ' PAR(i) set to zero, for i > ',I3)
       ENDIF
       READ(3,*)(PAR(I),I=1,NPARR)
C
C Compute the starting value of the objective functional
C (using UPOLDP for temporary storage)
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           DO K=K1,K2
             U(K-K1+1)=UPS(J,K)
           ENDDO
           CALL FOPT(NDM,U,ICP,PAR,0,FS,DUMU,DUMP)
           UPOLDP(J,K1)=FS
         ENDDO
       ENDDO
       DO K=1,NDM
          U(K)=UPS(NRSP1,K)
       ENDDO
       CALL FOPT(NDM,U,ICP,PAR,0,FS,DUMU,DUMP)
       UPOLDP(NRSP1,1)=FS
       PAR(10)=RINTG(IAP,NDX,1,UPOLDP,DTM)
C
C Complement starting data
C
       DO I=12,NPARX
         PAR(I)=0.d0
       ENDDO
C
       DO  J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+NDM+1
           K2=I*NDIM
           DO K=K1,K2
             UPS(J,K)=0.d0
           ENDDO
         ENDDO
       ENDDO
       K1=NDM+1
       DO K=K1,NDIM
         UPS(NRSP1,K)=0.d0
       ENDDO
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
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C        Subroutines for the Continuation of Folds for BVP.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNBL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the equations for the 2-parameter continuation
C of folds (BVP).
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
C Generate the function.
C
      CALL FFBL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
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
        CALL FFBL(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        CALL FFBL(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
        DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
        ENDDO
      ENDDO
C
      DO I=1,NFPR
        PAR(ICP(I))=PAR(ICP(I))+EP
        CALL FFBL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
        ENDDO
        PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FFBL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
C
       NFPR=IAP(29)
C
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
C
       NFPX=NFPR/2-1
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
         ENDDO
         IF(NFPX.GT.0)THEN
           DO J=1,NFPX
             F(NDM+I)=F(NDM+I)
     *      + DFDP(I,ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
           ENDDO
         ENDIF
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCBL(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates the boundary conditions for the 2-parameter continuation
C of folds (BVP).
C
      DIMENSION IAP(*),U0(*),U1(*),F(NBC),ICP(*),PAR(*),DBC(NBC,*)
C Local
      ALLOCATABLE UU1(:),UU2(:),FF1(:),FF2(:),DFU(:,:)
      ALLOCATE(DFU(NBC,2*NDIM+NPARX))
C
       NBC0=IAP(24)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,F,DFU)
C
       IF(IJAC.EQ.0)THEN
          DEALLOCATE(DFU)
          RETURN
       ENDIF
C
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NBC),FF2(NBC))
C
C Derivatives with respect to U0.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U0(J)
           UU2(J)=U0(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,UU1,U1,FF1,DFU)
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,UU2,U1,FF2,DFU)
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
         DO J=1,NDIM
           UU1(J)=U1(J)
           UU2(J)=U1(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,UU1,FF1,DFU)
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,UU2,FF2,DFU)
         DO J=1,NBC
           DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(FF1,UU1,UU2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF2,DFU)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF2,DFU)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(FF2(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
      DEALLOCATE(DFU,FF2)
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FBBL(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,F,DBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC0,*)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
       NFPX=NFPR/2-1
       CALL BCNI(IAP,RAP,NDM,PAR,ICP,NBC0,U0,U1,F,2,DBC)
       DO I=1,NBC0
         F(NBC0+I)=0.d0
         DO J=1,NDM
           F(NBC0+I)=F(NBC0+I)+DBC(I,J)*U0(NDM+J)
           F(NBC0+I)=F(NBC0+I)+DBC(I,NDM+J)*U1(NDM+J)
         ENDDO
         IF(NFPX.NE.0) THEN
           DO J=1,NFPX
             F(NBC0+I)=F(NBC0+I)
     *       + DBC(I,NDIM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
           ENDDO
         ENDIF
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICBL(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Generates integral conditions for the 2-parameter continuation of
C folds (BVP).
C
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
C Local
      ALLOCATABLE UU1(:),UU2(:),FF1(:),FF2(:),DFU(:,:)
      ALLOCATE(DFU(NDIM,NDIM+NPARX))
C
       NNT0=IAP(25)
       NFPR=IAP(29)
C
C Generate the function.
C
       CALL FIBL(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F,DFU)
C
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
C
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NINT),FF2(NINT))
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
         CALL FIBL(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU1,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         CALL FIBL(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU2,UOLD,UDOT,
     *    UPOLD,FF2,DFU)
         DO J=1,NINT
           DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIBL(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
      DEALLOCATE(FF1,DFU)
      RETURN
      END
C
C     --------------
      SUBROUTINE FIBL(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,
     * U,UOLD,UDOT,UPOLD,F,DINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NNT0,*)
C
       NDM=IAP(23)
       NFPR=IAP(29)
C
       NFPX=0
       IF(NINT.GT.1) THEN
         NFPX=NFPR/2-1
         CALL ICNI(IAP,RAP,NDM,PAR,ICP,NNT0,U,UOLD,UDOT,UPOLD,F,2,DINT)
         DO I=1,NNT0
           F(NNT0+I)=0.d0
           DO J=1,NDM
             F(NNT0+I)=F(NNT0+I)+DINT(I,J)*U(NDM+J)
           ENDDO
           IF(NFPX.NE.0) THEN
             DO J=1,NFPX
               F(NNT0+I)=F(NNT0+I)
     *         + DINT(I,NDM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
             ENDDO
           ENDIF
         ENDDO
       ENDIF
C
C Note that PAR(11+NFPR/2) is used to keep the norm of the null vector
       F(NINT)=-PAR(11+NFPR/2)
       DO I=1,NDM
         F(NINT)=F(NINT)+U(NDM+I)*U(NDM+I)
       ENDDO
       IF(NFPX.NE.0) THEN
         DO I=1,NFPX
           F(NINT)=F(NINT)+PAR(ICP(NFPR-NFPX+I))**2
         ENDDO
       ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNBL(IAP,RAP,PAR,ICP,NTSR,NCOLRS,
     * RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Generates starting data for the 2-parameter continuation of folds.
C (BVP).
C
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
C Local
      DIMENSION TEMP(7),ICPRS(NPARX)
C
      LOGICAL FOUND
C
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
       IBR=IAP(30)
C
       CALL FINDLB(IAP,RAP,IRS,NFPR1,FOUND)
       READ(3,*)IBR,NTOT1,ITP1,LAB1,NFPR1,ISW1,NTPL1,NAR1,NSKIP1,
     * NTSR,NCOLRS,NPARR
       IAP(30)=IBR
       NRSP1=NTSR+1
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           READ(3,*)TEMP(I),(UPS(J,K),K=K1,K2)
         ENDDO
         TM(J)=TEMP(1)
       ENDDO
       READ(3,*)TM(NRSP1),(UPS(NRSP1,K),K=1,NDM)
C
       NFPR0=NFPR/2
       READ(3,*)ICPRS(1)
       READ(3,*)(RLDOT(I),I=1,NFPR0)
C
C Read U-dot (Derivative with respect to arclength).
C
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+NDM+1
           K2=I*NDIM
           READ(3,*)(UPS(J,K),K=K1,K2)
         ENDDO
       ENDDO
       K1=NDM+1
       READ(3,*)(UPS(NRSP1,K),K=K1,NDIM)
C
C Read the parameter values.
C
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(6,100)NPARR
 100     FORMAT(' Warning : NPARX too small for restart data : ',/,
     *          ' PAR(i) set to zero, for i > ',I3)
       ENDIF
       READ(3,*)(PAR(I),I=1,NPARR)
C
       NFPX=NFPR/2-1
       IF(NFPX.GT.0) THEN
         DO I=1,NFPX
           PAR(ICP(NFPR0+1+I))=RLDOT(I+1)
         ENDDO
       ENDIF
C Initialize the norm of the null vector
       PAR(11+NFPR/2)=0.
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
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C          Routines for Interface with User Supplied Routines
C  (To generate Jacobian by differencing, if not supplied analytically)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Interface subroutine to user supplied FUNC.
C
      DIMENSION IAP(*),U(*),UOLD(*),ICP(*),PAR(*),F(*)
      DIMENSION DFDU(NDIM,*),DFDP(NDIM,*)
C Local
      DIMENSION U1ZZ(NDIMX),U2ZZ(NDIMX),F1ZZ(NDIMX),F2ZZ(NDIMX)
C
       JAC=IAP(22)
C
C Generate the function.
C
       IF((IJAC.EQ.1.AND.JAC.NE.0).OR.(IJAC.EQ.2.AND.JAC.EQ.1))THEN
         IJC=IJAC
       ELSE
         IJC=0
       ENDIF
       CALL FUNC(NDIM,U,ICP,PAR,IJC,F,DFDU,DFDP,UOLD)
C
       IF(JAC.EQ.1 .OR. IJAC.EQ.0 .OR. (JAC.EQ.-1.AND.IJAC.EQ.1))RETURN
C
       IF(IJAC.NE.1)THEN
          NFPR=IAP(29)
          DO I=1,NFPR
             EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
             PAR(ICP(I))=PAR(ICP(I))+EP
             CALL FUNC(NDIM,U,ICP,PAR,0,F1ZZ,DFDU,DFDP,UOLD)
             DO J=1,NDIM
                DFDP(J,ICP(I))=(F1ZZ(J)-F(J))/EP
             ENDDO
             PAR(ICP(I))=PAR(ICP(I))-EP
          ENDDO
       ENDIF
C
C if the user specified the Jacobian but not the
C parameter derivatives we return here
C
       IF (JAC.EQ.-1)RETURN
C
C Generate the Jacobian by differencing.
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
           U1ZZ(J)=U(J)
           U2ZZ(J)=U(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL FUNC(NDIM,U1ZZ,ICP,PAR,0,F1ZZ,DFDU,DFDP,UOLD)
         CALL FUNC(NDIM,U2ZZ,ICP,PAR,0,F2ZZ,DFDU,DFDP,UOLD)
         DO J=1,NDIM
           DFDU(J,I)=(F2ZZ(J)-F1ZZ(J))/(2*EP)
        ENDDO
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Interface subroutine to the user supplied BCND.
C
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
C Local
      ALLOCATABLE U1ZZ(:),U2ZZ(:),F1ZZ(:),F2ZZ(:)
C
       JAC=IAP(22)
       NFPR=IAP(29)
C
C Generate the function.
C
       IF(JAC.EQ.0)THEN
         IJC=0
       ELSE
         IJC=IJAC
       ENDIF
       CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJC,DBC)
C
       IF(JAC.EQ.1 .OR. IJAC.EQ.0)RETURN
C
       ALLOCATE(U1ZZ(NDIM),U2ZZ(NDIM),F1ZZ(NBC),F2ZZ(NBC))
C
C Generate the Jacobian by differencing.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
C
       EP=HMACH*(1+UMX)
C
       DO I=1,NDIM
         DO J=1,NDIM
           U1ZZ(J)=U0(J)
           U2ZZ(J)=U0(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL BCND(NDIM,PAR,ICP,NBC,U1ZZ,U1,F1ZZ,0,DBC)
         CALL BCND(NDIM,PAR,ICP,NBC,U2ZZ,U1,F2ZZ,0,DBC)
         DO J=1,NBC
           DBC(J,I)=(F2ZZ(J)-F1ZZ(J))/(2*EP)
         ENDDO
       ENDDO
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U1(I)).GT.UMX)UMX=DABS(U1(I))
       ENDDO
C
       EP=HMACH*(1+UMX)
C
       DO I=1,NDIM
         DO J=1,NDIM
           U1ZZ(J)=U1(J)
           U2ZZ(J)=U1(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL BCND(NDIM,PAR,ICP,NBC,U0,U1ZZ,F1ZZ,0,DBC)
         CALL BCND(NDIM,PAR,ICP,NBC,U0,U2ZZ,F2ZZ,0,DBC)
         DO J=1,NBC
           DBC(J,NDIM+I)=(F2ZZ(J)-F1ZZ(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(U1ZZ,U2ZZ,F2ZZ)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(F1ZZ)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F1ZZ,0,DBC)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(F1ZZ(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
      DEALLOCATE(F1ZZ)
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ICNI(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Interface subroutine to user supplied ICND.
C
      DIMENSION IAP(*),U(*),UOLD(*),UDOT(*),UPOLD(*)
      DIMENSION F(*),DINT(NINT,*),ICP(*),PAR(*)
C Local
      ALLOCATABLE U1ZZ(:),U2ZZ(:),F1ZZ(:),F2ZZ(:)
C
       JAC=IAP(22)
       NFPR=IAP(29)
C
C Generate the integrand.
C
       IF(JAC.EQ.0)THEN
         IJC=0
       ELSE
         IJC=IJAC
       ENDIF
       CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJC,DINT)
C
       IF(JAC.EQ.1 .OR. IJAC.EQ.0)RETURN
C
       ALLOCATE(U1ZZ(NDIM),U2ZZ(NDIM),F1ZZ(NINT),F2ZZ(NINT))
C
C Generate the Jacobian by differencing.
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
           U1ZZ(J)=U(J)
           U2ZZ(J)=U(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL ICND(NDIM,PAR,ICP,NINT,U1ZZ,UOLD,UDOT,UPOLD,F1ZZ,0,DINT)
         CALL ICND(NDIM,PAR,ICP,NINT,U2ZZ,UOLD,UDOT,UPOLD,F2ZZ,0,DINT)
         DO J=1,NINT
           DINT(J,I)=(F2ZZ(J)-F1ZZ(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(U1ZZ,U2ZZ,F2ZZ)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(F1ZZ)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F1ZZ,0,DINT)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(F1ZZ(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
      DEALLOCATE(F1ZZ)
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE FOPI(IAP,RAP,NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Interface subroutine to user supplied FOPT.
C
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),DFDU(*),DFDP(*)
C Local
      DIMENSION U1ZZ(NDIMX),U2ZZ(NDIMX)
C
       JAC=IAP(22)
       NFPR=IAP(29)
C
C Generate the objective function.
C
       IF(JAC.EQ.0)THEN
         IJC=0
       ELSE
         IJC=IJAC
       ENDIF
       CALL FOPT(NDIM,U,ICP,PAR,IJC,F,DFDU,DFDP)
C
       IF(JAC.EQ.1 .OR. IJAC.EQ.0)RETURN
C
C Generate the Jacobian by differencing.
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
           U1ZZ(J)=U(J)
           U2ZZ(J)=U(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL FOPT(NDIM,U1ZZ,ICP,PAR,0,F1,DFDU,DFDP)
         CALL FOPT(NDIM,U2ZZ,ICP,PAR,0,F2,DFDU,DFDP)
         DFDU(I)=(F2-F1)/(2*EP)
       ENDDO
C
       IF(IJAC.EQ.1)RETURN
C
       DO I=1,NFPR
         EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FOPT(NDIM,U,ICP,PAR,0,F1,DFDU,DFDP)
         DFDP(ICP(I))=(F1-F)/EP
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
C
       RETURN
       END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
