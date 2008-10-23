!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Output (Algebraic Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE IO

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: FINDLB, READLB, READBV, WRLINE, WRBAR, STHD, NEWLAB, &
       GETNDIM3, GETNTST3, GETNCOL3, GETNFPR3

  TYPE SOLUTION
     INTEGER :: IBR, NTOT, ITP, LAB, NFPR, ISW, NTPL, NAR, NROWPR, NTST, NCOL,&
          NPAR
     DOUBLE PRECISION, DIMENSION(:,:), POINTER :: UPS, UDOTPS
     DOUBLE PRECISION, DIMENSION(:), POINTER :: TM
     DOUBLE PRECISION, DIMENSION(:), POINTER :: RLDOT, PAR
     INTEGER, DIMENSION(:), POINTER :: ICP
     TYPE(SOLUTION), POINTER :: NEXT
  END TYPE SOLUTION
  TYPE(SOLUTION), POINTER :: ROOTSOL, CURSOL
  INTEGER, SAVE :: MBR=0, MLAB=0
CONTAINS

! ---------- ----
  SUBROUTINE STHD(IAP,RAP,ICP,ICU)

    USE COMPAT

! Write the values of the user defined parameters on unit 7.
! This identifying information is preceded by a '   0' on each line.
! The first line in the file contains the (generally) user-supplied
! limits of the bifurcation diagram, viz. RL0,RL1,A0 and A1.
! These are often convenient for an initial plot of the diagram.

    INTEGER, INTENT(IN) :: ICP(*),ICU(*),IAP(*)
    DOUBLE PRECISION, INTENT(IN) :: RAP(*)
    CHARACTER (LEN=*), PARAMETER :: D3 = "('   0',3(A8,ES11.4))"
    CHARACTER (LEN=*), PARAMETER :: I4 = "('   0',4(A8,I4))"
    CHARACTER (LEN=*), PARAMETER :: I5 = "('   0',A7,I5,3(A8,I4))"
    CHARACTER (LEN=*), PARAMETER :: I6 = "('   0',3(A8,I4),2(A7,I4))"
    INTEGER NDIM,IPS,IRS,ILP,NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT,NMX
    INTEGER NUZR,NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC,NFPR,NICP,I
    DOUBLE PRECISION DS,DSMIN,DSMAX,RL0,RL1,A0,A1,EPSL,EPSU,EPSS

    NDIM=IAP(1)
    IPS=IAP(2)
    IRS=IAP(3)
    ILP=IAP(4)
    NTST=IAP(5)
    NCOL=IAP(6)
    IAD=IAP(7)
    ISP=IAP(9)
    ISW=IAP(10)
    IPLT=IAP(11)
    NBC=IAP(12)
    NINT=IAP(13)
    NMX=IAP(14)
    NUZR=IAP(15)
    NPR=IAP(16)
    MXBF=IAP(17)
    IID=IAP(18)
    ITMX=IAP(19)
    ITNW=IAP(20)
    NWTN=IAP(21)
    JAC=IAP(22)
    NFPR=IAP(29)
    NICP=IAP(41)

    DS=RAP(1)
    DSMIN=RAP(2)
    DSMAX=RAP(3)
    RL0=RAP(6)
    RL1=RAP(7)
    A0=RAP(8)
    A1=RAP(9)
    EPSL=RAP(11)
    EPSU=RAP(12)
    EPSS=RAP(13)

    WRITE(7,"(I4,' ',4ES12.4)")0,RL0,RL1,A0,A1
    WRITE(7,D3)'EPSL=',EPSL,'EPSU =',EPSU, 'EPSS =',EPSS
    WRITE(7,D3)'DS  =',DS,  'DSMIN=',DSMIN,'DSMAX=',DSMAX
    WRITE(7,I4)'NDIM=',NDIM,'IPS =',IPS, 'IRS =',IRS, 'ILP =',ILP
    WRITE(7,I4)'NTST=',NTST,'NCOL=',NCOL,'IAD =',IAD, 'ISP =',ISP
    WRITE(7,I4)'ISW =',ISW, 'IPLT=',IPLT,'NBC =',NBC, 'NINT=',NINT
    WRITE(7,I5)' NMX=',NMX, 'NPR =',NPR, 'MXBF=',MXBF,'IID =',IID
    WRITE(7,I6)'ITMX=',ITMX,'ITNW=',ITNW,'NWTN=',NWTN,'JAC=',JAC,'  NUZR=',NUZR

    WRITE(7,"('   0   User-specified parameter')",ADVANCE="NO")
    IF(NICP.EQ.1)THEN
       WRITE(7,"(':       ',  I4)")(ICU(I),I=1,NICP)
    ELSE
       WRITE(7,"('s:      ',24I4)")(ICU(I),I=1,NICP)
    ENDIF

    WRITE(7,"('   0   Active continuation parameter')",ADVANCE="NO")
    IF(NFPR.EQ.1)THEN
       WRITE(7,"(':  ',  I4)")(ICP(I),I=1,NFPR)
    ELSE
       WRITE(7,"('s: ',24I4)")(ICP(I),I=1,NFPR)
    ENDIF
    CALL AUTOFLUSH(7)

  END SUBROUTINE STHD

! ---------- ------
  SUBROUTINE HEADNG(IAP,ICP,IUNIT,N1,N2)

    USE COMPAT

! Prints headings above columns on unit 6, 7, and 9.
! N1 = number of parameters to print (maximum: 7 for screen output)
! N2 = number of (max) variables to print (maximum: max(0,7-N1,7))

    INTEGER, INTENT(IN) :: IAP(*),ICP(*),IUNIT,N1,N2
! Local
    INTEGER I,J,IPS,IPLT,NDM

    IPS=IAP(2)
    IPLT=IAP(11)
    NDM=IAP(23)

    IF(IUNIT.EQ.7)THEN
       WRITE(7,"(I4/I4,A)",ADVANCE="NO")0,0,'    PT  TY  LAB '
    ELSE
       WRITE(IUNIT,"(1X/A)",ADVANCE="NO")'  BR    PT  TY  LAB '
    ENDIF

    DO J=1,N1+N2+1
       IF(J==1.OR.J>N2+2)THEN
          I=1
          IF(J>1)I=J-N2-1
          IF(ICP(I)==11.AND.IPS>0.AND.IPS/=4.AND.IPS/=7)THEN
             CALL WRITECOL(5,'PERIOD')
          ELSEIF(ICP(I)==10.AND.(IPS==5.OR.IPS==15))THEN
             CALL WRITECOL(6,'FOPT')
          ELSEIF(ICP(I)==14.AND.(IPS==14.OR.IPS==16))THEN
             CALL WRITECOL(6,'TIME')
          ELSE
             CALL WRITECOL(4,'PAR',ICP(I))
          ENDIF
       ELSEIF(J==2)THEN
          IF(IPLT>NDM.AND.IPLT<=2*NDM) THEN
             CALL WRITECOL(2,'INTEGRAL U',IPLT-NDM)
          ELSE IF(IPLT>2*NDM.AND.IPLT<=3*NDM) THEN
             CALL WRITECOL(2,'L2-NORM U',IPLT-2*NDM)
          ELSE IF(IPLT>0.AND.IPLT<=NDM) THEN
             IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                CALL WRITECOL(6,'U',IPLT)
             ELSE
                CALL WRITECOL(4,'MAX U',IPLT)
             ENDIF
          ELSE IF(IPLT<0.AND.IPLT>=-NDM) THEN
             IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                CALL WRITECOL(6,'U',-IPLT)
             ELSE
                CALL WRITECOL(4,'MIN U',-IPLT)
             ENDIF
          ELSE
             CALL WRITECOL(4,'L2-NORM')
          ENDIF
       ELSE !J>2 with N2>0
          IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
             CALL WRITECOL(6,'U',J-2)
          ELSE
             CALL WRITECOL(4,'MAX U',J-2)
          ENDIF
       ENDIF

    ENDDO

    WRITE(IUNIT,"()")
    CALL AUTOFLUSH(IUNIT)

  CONTAINS

    SUBROUTINE WRITECOL(I,S,N)
      INTEGER, INTENT(IN) :: I
      CHARACTER(*), INTENT(IN) :: S
      INTEGER, INTENT(IN), OPTIONAL :: N
! Local
      CHARACTER(10) SN
      CHARACTER(19) COL
      COL=' '
      IF(PRESENT(N))THEN
         WRITE(SN,"(I10)")N
         WRITE(COL(I:),"(A,A,A,A)") S,'(',TRIM(ADJUSTL(SN)),')'
      ELSE
         WRITE(COL(I:),"(A)") S
      ENDIF
      IF(IUNIT.EQ.7)THEN
         WRITE(IUNIT,"(A19)",ADVANCE="NO")COL
      ELSE
         WRITE(IUNIT,"(A14)",ADVANCE="NO")COL
      ENDIF
    END SUBROUTINE WRITECOL
    
  END SUBROUTINE HEADNG

! ---------- ------
  SUBROUTINE WRLINE(IAP,PAR,ICU,IBR,NTOT,LAB,VAXIS,U)

    USE COMPAT

! Write one line of output on unit 6 and 7.

    INTEGER, INTENT(IN) :: IAP(*),ICU(*),IBR,NTOT,LAB
    DOUBLE PRECISION, INTENT(IN) :: PAR(*),U(*),VAXIS
! Local
    CHARACTER*2 ATYPE
    CHARACTER*2, PARAMETER :: ATYPESP(9) = &
         (/ 'BP','LP','HB','  ','LP','BP','PD','TR','EP' /)
    CHARACTER*2, PARAMETER :: ATYPESN(9) = &
         (/ '  ','  ','  ','UZ','  ','  ','  ','  ','MX' /)
    CHARACTER(33) :: F69 ! (I4,I6,2X,A2,I5,**********ES14.5)
    CHARACTER(31) :: F7  ! (I4,I6,I4,I5,**********ES19.10)
    INTEGER MTOT,NDM,ITP,NICP,N1,N2,I

    NDM=IAP(23)
    ITP=IAP(27)
    NICP=IAP(41)

    N1=NICP
    N2=NDM

    IF(N1.GT.7)THEN
       N1=7
       N2=0
    ELSEIF(N1+N2.GT.7)THEN
       N2=7-N1
    ENDIF

! Write a heading above the first line.

    IF(ABS(NTOT).EQ.1)CALL HEADNG(IAP,ICU,6,N1,N2)
    IF(ABS(NTOT).EQ.1)CALL HEADNG(IAP,ICU,7,NICP,N2)
    CALL HEADNG(IAP,ICU,9,N1,N2)

    IF(MOD(ITP,10)>0)THEN
       ATYPE=ATYPESP(MOD(ITP,10))
    ELSEIF(MOD(ITP,10)<0)THEN
       ATYPE=ATYPESN(-MOD(ITP,10))
    ELSE
       ATYPE='  '
    ENDIF

    IF(NTOT>0)THEN
       MTOT=MOD(NTOT-1,9999)+1
    ELSE
       MTOT=-MOD(-NTOT-1,9999)-1
    ENDIF
    WRITE(F69,"(A,I10,A)") '(I4,I6,2X,A2,I5,',N1+N2+1,'ES14.5)'
    WRITE(F7,"(A,I10,A)") '(I4,I6,I4,I5,',NICP+N2+1,'ES19.10)'
    IF(MOD(ITP,10).NE.0)THEN
       WRITE(6,F69)ABS(IBR),ABS(MTOT),ATYPE,LAB,PAR(ICU(1)),VAXIS, &
            (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
       CALL AUTOFLUSH(6)
    ENDIF
    WRITE(7,F7)IBR,MTOT,ITP,LAB,PAR(ICU(1)),VAXIS, &
         (U(I),I=1,N2),(PAR(ICU(I)),I=2,NICP)
    CALL AUTOFLUSH(7)
    WRITE(9,F69)IBR,MTOT,ATYPE,LAB,PAR(ICU(1)),VAXIS, &
         (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
  END SUBROUTINE WRLINE

! ---------- -----
  SUBROUTINE WRBAR(C,N)

    CHARACTER*1, INTENT(IN) :: C
    INTEGER, INTENT(IN) :: N
    INTEGER I

    WRITE(9,101)(C,I=1,N)
101 FORMAT(80A1)
  END SUBROUTINE WRBAR

! ---------- ------
  SUBROUTINE NEWLAB(IAP)

! Determine a suitable label when restarting.

    INTEGER, INTENT(INOUT) :: IAP(*)

    INTEGER IPS,IRS,ISW,ITP,LAB,IBR

    IPS=IAP(2)
    IRS=IAP(3)
    ISW=IAP(10)
    ITP=IAP(27)

    LAB=MLAB
    IAP(37)=LAB
    IF(ISW.LT.0.OR.IRS.EQ.0)THEN
       IBR=MBR+1
       IAP(30)=IBR
    ELSEIF( (ABS(ITP).LT.10.AND.ABS(ISW).EQ.2) &
         .OR. (IPS.EQ.2.AND.ITP.EQ.3) &
         .OR. (IPS.EQ.4.AND.ISW.EQ.2.AND.ABS(ITP).LT.10) &
         .OR. (IPS.EQ.5.AND.MOD(ITP,10).EQ.2) )THEN
       IBR=IRS
       IAP(30)=IBR
    ENDIF

  END SUBROUTINE NEWLAB

! ------- -------- --------
  INTEGER FUNCTION GETNDIM3()
    GETNDIM3 = CURSOL%NAR-1
  END FUNCTION GETNDIM3

! ------- -------- --------
  INTEGER FUNCTION GETNTST3()
    GETNTST3 = CURSOL%NTST
  END FUNCTION GETNTST3

! ------- -------- --------
  INTEGER FUNCTION GETNCOL3()
    GETNCOL3 = CURSOL%NCOL
  END FUNCTION GETNCOL3

! ------- -------- --------
  INTEGER FUNCTION GETNFPR3()
    GETNFPR3 = CURSOL%NFPR
  END FUNCTION GETNFPR3

! ---------- ------
  SUBROUTINE FINDLB(NAME,IAP,IRS,NFPR,NPAR,FOUND)

    INTEGER, INTENT(INOUT) :: IAP(*)
    INTEGER, INTENT(INOUT) :: IRS
    INTEGER, INTENT(OUT) :: NFPR,NPAR
    LOGICAL, INTENT(OUT) :: FOUND
    CHARACTER*(*), INTENT(IN) :: NAME

    LOGICAL EOF3
    INTEGER IBR,NTOT,ITP,LAB,ISWR,NTPL,NAR,NROWPR,NTST,NCOL
    INTEGER ISW,ITPST,I

! Locates restart point with label IRS and determines type.
! If the label can not be located on unit 3 then FOUND will be .FALSE.
! If IRS is negative, then just pick the abs(IRS)th solution.

    FOUND=.FALSE.
    ISW=IAP(10)

    OPEN(3,FILE=NAME,STATUS='old',ACCESS='sequential')
    I=0
    DO
       I=I+1
       READ(3,*,END=2)IBR,NTOT,ITP,LAB,NFPR,ISWR,NTPL,NAR,NROWPR,NTST,NCOL,NPAR
       IF(IBR>MBR)MBR=IBR
       IF(LAB>MLAB)MLAB=LAB

       IF((LAB==IRS.OR.IRS==-I).AND..NOT.FOUND)THEN
          IRS=LAB
          FOUND=.TRUE.
          IAP(27)=ITP
          IAP(30)=IBR
          IF(ABS(ISW).GE.2)THEN
             IF(ABS(ITP).LT.10)THEN
                ITPST=ABS(ITP)
             ELSE
                ITPST=ABS(ITP/10)
             ENDIF
          ELSE
             ITPST=0
          ENDIF
          IAP(28)=ITPST
          CALL READSOL(IBR,NTOT,ITP,LAB,NFPR,ISWR,NTPL,NAR,NROWPR,NTST,NCOL, &
               NPAR)
       ELSE
          CALL SKIP3(NROWPR,EOF3)
          IF(EOF3)GOTO 2
       ENDIF
    ENDDO

2   CONTINUE
    CLOSE(3)

  END SUBROUTINE FINDLB

! ---------- -------
  SUBROUTINE READSOL(IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR)

    INTEGER, INTENT(IN) :: IBR,NTOT,ITP,LAB,NFPR
    INTEGER, INTENT(IN) :: ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR

! Local
    INTEGER J, NTNC

    NULLIFY(ROOTSOL)
    NTNC=NTPL-1

    CALL NEWSOL(IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR)
    DO J=0,NTNC
       READ(3,*)CURSOL%TM(J),CURSOL%UPS(:,J)
    ENDDO

    IF(NTST>0)THEN
       READ(3,*)CURSOL%ICP(:)
       READ(3,*)CURSOL%RLDOT(:)

! Read U-dot (derivative with respect to arclength).

       READ(3,*)CURSOL%UDOTPS(:,:)
    ENDIF

! Read the parameter values.

    READ(3,*)CURSOL%PAR(:)

  END SUBROUTINE READSOL

! ---------- ------
  SUBROUTINE NEWSOL(IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR)

    INTEGER, INTENT(IN) :: IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,&
         NTST,NCOL,NPAR
    TYPE(SOLUTION), POINTER :: SOL
    INTEGER NDIM,NTNC

    ALLOCATE(SOL)
    SOL%IBR = IBR
    SOL%NTOT = NTOT
    SOL%ITP = ITP
    SOL%LAB = LAB
    SOL%NFPR = NFPR
    SOL%ISW = ISW
    SOL%NTPL = NTPL
    SOL%NAR = NAR
    SOL%NROWPR = NROWPR
    SOL%NTST = NTST
    SOL%NCOL = NCOL
    SOL%NPAR = NPAR
    NDIM=NAR-1
    NTNC=NTPL-1
    ALLOCATE(SOL%UPS(NDIM,0:NTNC),SOL%TM(0:NTNC),SOL%PAR(NPAR))
    IF(NTST>0)THEN
       ALLOCATE(SOL%UDOTPS(NDIM,0:NTNC),SOL%ICP(NFPR),SOL%RLDOT(NFPR))
    ELSE
       NULLIFY(SOL%UDOTPS,SOL%ICP,SOL%RLDOT)
    ENDIF
    NULLIFY(SOL%NEXT)
    IF(ASSOCIATED(ROOTSOL))THEN
       CURSOL%NEXT => SOL
    ELSE
       ROOTSOL => SOL
    ENDIF
    CURSOL => SOL
  END SUBROUTINE NEWSOL

! ---------- ------
  SUBROUTINE READLB(IAP,ICPRS,U,UDOT,PAR)

    INTEGER, INTENT(IN) :: IAP(*)
    INTEGER, INTENT(OUT) :: ICPRS(*)
    DOUBLE PRECISION, INTENT(OUT) :: U(*),UDOT(*),PAR(*)
    INTEGER NFPR,NPARR,NDIM

! Reads the restart data for algebraic problems.

    NFPR=CURSOL%NFPR
    NPARR=CURSOL%NPAR
    NDIM=MIN(CURSOL%NAR-1,IAP(1))
    U(1:NDIM)=CURSOL%UPS(1:NDIM,0)
    IF(CURSOL%NTST>0)THEN
       ICPRS(1:NFPR)=CURSOL%ICP(1:NFPR)
       UDOT(NDIM+1)=CURSOL%RLDOT(1)
       UDOT(NDIM-NFPR+2:NDIM)=CURSOL%RLDOT(2:NFPR)
       UDOT(1:NDIM)=CURSOL%UDOTPS(1:NDIM,0)
    ENDIF
    PAR(1:NPARR)=CURSOL%PAR(1:NPARR)

  END SUBROUTINE READLB

! ---------- ------
  SUBROUTINE READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOTRS,UPS, &
       UDOTPS,TM,ITPRS,NDIM)

    INTEGER, INTENT(IN) :: NDIM
    INTEGER, INTENT(INOUT) :: IAP(*)
    INTEGER, INTENT(OUT) :: ICPRS(*),NTSRS,NCOLRS,NDIMRD,ITPRS
    DOUBLE PRECISION, INTENT(OUT) :: RLDOTRS(*),UPS(NDIM,0:*),UDOTPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(OUT) :: TM(0:*),PAR(*)
! Local
    INTEGER J,NFPR,NFPRS,NPARR

    NFPR=IAP(29)
    ITPRS=CURSOL%ITP
    NFPRS=CURSOL%NFPR
    NTSRS=CURSOL%NTST
    NCOLRS=CURSOL%NCOL
    NPARR=CURSOL%NPAR
    IAP(30)=CURSOL%IBR
    IAP(37)=CURSOL%LAB

    NDIMRD=MIN(NDIM,CURSOL%NAR-1)

    DO J=0,NTSRS*NCOLRS
       IF(MOD(J,NCOLRS)==0)THEN
          TM(J/NCOLRS)=CURSOL%TM(J)
       ENDIF
       UPS(1:NDIMRD,J)=CURSOL%UPS(1:NDIMRD,J)
    ENDDO

    ICPRS(1:MIN(NFPR,NFPRS))=CURSOL%ICP(1:MIN(NFPR,NFPRS))
    RLDOTRS(1:MIN(NFPR,NFPRS))=CURSOL%RLDOT(1:MIN(NFPR,NFPRS))

! Read U-dot (derivative with respect to arclength).

    DO J=0,NTSRS*NCOLRS
       UDOTPS(1:NDIMRD,J)=CURSOL%UDOTPS(1:NDIMRD,J)
    ENDDO

! Read the parameter values.

    PAR(1:NPARR)=CURSOL%PAR(1:NPARR)

  END SUBROUTINE READBV

! ---------- -----
  SUBROUTINE SKIP3(NSKIP,EOF3)

! Skips the specified number of lines on unit 3.

    INTEGER, INTENT(IN) :: NSKIP
    LOGICAL, INTENT(OUT) :: EOF3
    CHARACTER(12) FMT
    INTEGER I

    EOF3=.TRUE.
    IF(NSKIP<=1)THEN
       DO I=1,NSKIP
         READ(3,'(A)',END=2)
       ENDDO
    ELSE
       WRITE(FMT,'(A,I9,A)')'(',NSKIP-1,'/)'
       READ(3,FMT,END=2)
    ENDIF
    EOF3=.FALSE.
2   RETURN

  END SUBROUTINE SKIP3

END MODULE IO
