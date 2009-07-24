!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Output (Algebraic Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE IO

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: FINDLB, READLB, READBV, WRLINE, WRBAR, STHD, NEWLAB, &
       GETNDIM3, GETNTST3, GETNCOL3, GETNFPR3, NAMEIDX

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

! ------------- -------- -------
  CHARACTER(13) FUNCTION getname(is, ind)
    USE AUTO_CONSTANTS, ONLY: INDEXSTR
    TYPE(INDEXSTR), INTENT(IN) :: is(:)
    INTEGER, INTENT(IN) :: ind
    INTEGER i

    getname = ''
    DO i = 1, SIZE(is)
       IF (is(i)%index == ind) THEN
          getname = is(i)%str
          RETURN
       ENDIF
    ENDDO
  END FUNCTION getname


! ---------- ----
  SUBROUTINE STHD(IAP,RAP,ICP)

    USE COMPAT
    USE AUTO_CONSTANTS, ONLY : IVTHL, IVTHU, IVUZR, unames, parnames, &
         NDIM, IRS, ILP, IPS, ISP, ISW, NBC, NINT, NMX, DS, DSMIN, DSMAX, ICU,&
         EFILE, SVFILE, SFILE, DATFILE, HCONST, NPAR, UVALS, PARVALS, SP

! Write the values of the user defined parameters on unit 7.
! This identifying information is preceded by a '   0' on each line.
! The first line in the file contains the (generally) user-supplied
! limits of the bifurcation diagram, viz. RL0,RL1,A0 and A1.
! These are often convenient for an initial plot of the diagram.

    INTEGER, INTENT(IN) :: ICP(*),IAP(*)
    DOUBLE PRECISION, INTENT(IN) :: RAP(*)
    CHARACTER (LEN=*), PARAMETER :: D3 = "('   0',3(A8,ES11.4))"
    CHARACTER (LEN=*), PARAMETER :: I4 = "('   0',4(A8,I4))"
    CHARACTER (LEN=*), PARAMETER :: I5 = "('   0',2(A7,I5),3(A8,I4))"
    CHARACTER (LEN=*), PARAMETER :: I6 = "('   0',5(A8,I4))"
    CHARACTER (LEN=*), PARAMETER :: I7 = "('   0',3(A8,I4),2(A7,I4))"
    CHARACTER (LEN=*), PARAMETER :: I8 = "('   0',(A7,I5),2(A8,I4),2(A7,I4))"
    INTEGER NDIMA,IPSA,IRSA,ILPA,NTST,NCOL,IAD,IADS,ISPA,ISWA,IPLT,NBCA,NINTA
    INTEGER NMXA,NUZR,NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC,NFPR,I,NPARA
    INTEGER LSV,LDAT,LE,LS,INDX,io
    DOUBLE PRECISION DSA,DSMINA,DSMAXA,RL0,RL1,A0,A1,EPSL,EPSU,EPSS
    CHARACTER(LEN=12) :: INDSTR
    CHARACTER(LEN=13) :: name

    NDIMA=IAP(1)
    IPSA=IAP(2)
    IRSA=IAP(3)
    ILPA=IAP(4)
    NTST=IAP(5)
    NCOL=IAP(6)
    IAD=IAP(7)
    IADS=IAP(8)
    ISPA=IAP(9)
    ISWA=IAP(10)
    IPLT=IAP(11)
    NBCA=IAP(12)
    NINTA=IAP(13)
    NMXA=IAP(14)
    NUZR=IAP(15)
    NPR=IAP(16)
    MXBF=IAP(17)
    IID=IAP(18)
    ITMX=IAP(19)
    ITNW=IAP(20)
    NWTN=IAP(21)
    JAC=IAP(22)
    NFPR=IAP(29)
    NPARA=IAP(31)

    DSA=RAP(1)
    DSMINA=RAP(2)
    DSMAXA=RAP(3)
    RL0=RAP(6)
    RL1=RAP(7)
    A0=RAP(8)
    A1=RAP(9)
    EPSL=RAP(11)
    EPSU=RAP(12)
    EPSS=RAP(13)

    WRITE(7,"(I4,' ',4ES12.4)")0,RL0,RL1,A0,A1
    WRITE(7,D3)'EPSL=',EPSL,'EPSU =',EPSU, 'EPSS =',EPSS
    WRITE(7,D3)'DS  =',DSA,  'DSMIN=',DSMINA,'DSMAX=',DSMAXA
    WRITE(7,I4)'NDIM=',NDIMA,'IPS =',IPSA, 'IRS =',IRSA, 'ILP =',ILPA
    WRITE(7,I4)'NTST=',NTST,'NCOL=',NCOL,'IAD =',IAD, 'ISP =',ISPA
    WRITE(7,I4)'ISW =',ISWA, 'IPLT=',IPLT,'NBC =',NBCA, 'NINT=',NINTA
    WRITE(7,I5)' NMX=',NMXA, 'NPR=', NPR, 'MXBF=',MXBF,'IID =',IID, 'IADS=',IADS
    WRITE(7,I6)'ITMX=',ITMX,'ITNW=',ITNW,'NWTN=',NWTN,'JAC =',JAC,'  NUZR=',NUZR

    WRITE(7,"(A,I4,A)",ADVANCE="NO")"   0   NPAR=",NPARA
    CALL WRITELIST("   THL = ",IVTHL)
    CALL WRITELIST("    THU = ",IVTHU)
    WRITE(7,*)
    IF(NUZR>0)THEN
       CALL WRITEUZRLIST("   0   UZR = ",IVUZR)
       WRITE(7,*)
    ENDIF
    IF(IPS==9)THEN
       !homcont constants
       WRITE(7,"('   0   ',2(A,I4),2(A8,I2),(A8,I4))") &
            'NUNSTAB=',HCONST%NUNSTAB,' NSTAB=',HCONST%NSTAB,&
            'IEQUIB=',HCONST%IEQUIB,'ITWIST=',HCONST%ITWIST,&
            'ISTART=',HCONST%ISTART
       IF(SIZE(HCONST%IREV)>0.OR.SIZE(HCONST%IFIXED)>0.OR.&
            SIZE(HCONST%IPSI)>0)THEN
          WRITE(7,"('   0  ')",ADVANCE='NO')
          CALL WRITEINTLIST(" IREV=",HCONST%IREV)
          CALL WRITEINTLIST(" IFIXED=",HCONST%IFIXED)
          CALL WRITEINTLIST(" IPSI=",HCONST%IPSI)
          WRITE(7,*)
       ENDIF
    ENDIF
    LE=LEN_TRIM(EFILE)
    LSV=LEN_TRIM(SVFILE)
    LS=LEN_TRIM(SFILE)
    LDAT=LEN_TRIM(DATFILE)
    IF(LE>0.OR.LSV>0.OR.LS>0.OR.LDAT>0)THEN
       WRITE(7,"('   0  ')",ADVANCE="NO")
       IF(LE>0)THEN
          WRITE(7,"(A,A,A)",ADVANCE="NO")" e = '",TRIM(EFILE),"'"
       ENDIF
       IF(LS>0)THEN
          WRITE(7,"(A,A,A)",ADVANCE="NO")" s = '",TRIM(SFILE),"'"
       ENDIF
       IF(LDAT>0)THEN
          WRITE(7,"(A,A,A)",ADVANCE="NO")" dat = '",TRIM(DATFILE),"'"
       ENDIF
       IF(LSV>0)THEN
          WRITE(7,"(A,A,A)",ADVANCE="NO")" sv = '",TRIM(SVFILE),"'"
       ENDIF
       WRITE(7,*)
    ENDIF
    IF(SIZE(parnames)>0)THEN
       CALL WRITESTRLIST("   0   parnames = ",parnames)
       WRITE(7,*)
    ENDIF
    IF(SIZE(unames)>0)THEN
       CALL WRITESTRLIST("   0   unames   = ",unames)
       WRITE(7,*)
    ENDIF
    IF(SIZE(PARVALS)>0)THEN
       CALL WRITELIST("   0   PAR     = ",PARVALS)
       WRITE(7,*)
    ENDIF
    IF(SIZE(UVALS)>0)THEN
       CALL WRITELIST("   0   U       = ",UVALS)
       WRITE(7,*)
    ENDIF
    IF(SIZE(SP)>0)THEN
       WRITE(7,"(A,A,A)", ADVANCE="NO")"   0   SP=['",TRIM(SP(1)),"'"
       DO I=2,SIZE(SP)
          WRITE(7,"(A,A,A)", ADVANCE="NO")", '",TRIM(SP(I)),"'"
       ENDDO
       WRITE(7,"(A)")']'
    ENDIF

    WRITE(7,"('   0   User-specified constants:')")
    WRITE(7,D3)'DS  =',DS,  'DSMIN=',DSMIN,'DSMAX=',DSMAX
    WRITE(7,I7)'NDIM=',NDIM,'IPS =',IPS, 'IRS =',IRS, 'ILP =',ILP, 'NPAR=',NPAR
    WRITE(7,I8)'NMX=', NMX, 'ISP =',ISP, 'ISW =',ISW,' NBC =',NBC, 'NINT=',NINT

    WRITE(7,"('   0   User-specified parameter')",ADVANCE="NO")
    IF(SIZE(ICU).EQ.1)THEN
       WRITE(7,"(':       ')",ADVANCE="NO")
    ELSE
       WRITE(7,"('s:      ')",ADVANCE="NO")
    ENDIF
    DO I=1,SIZE(ICU)
       READ(ICU(I),*,IOSTAT=io)INDX
       IF(io==0)THEN
          WRITE(7,"(3X,A)",ADVANCE="NO")TRIM(ICU(I))
       ELSE
          WRITE(7,"(A,A,A)",ADVANCE="NO")" '",TRIM(ICU(I)),"'"
       ENDIF
    ENDDO
    WRITE(7,"(/'   0   Active continuation parameter')",ADVANCE="NO")
    IF(NFPR.EQ.1)THEN
       WRITE(7,"(':  ')",ADVANCE="NO")
    ELSE
       WRITE(7,"('s: ')",ADVANCE="NO")
    ENDIF
    DO I=1,NFPR
       name = getname(parnames, ICP(I))
       IF (LEN_TRIM(name)>0) THEN
          WRITE(7,"(A,A,A)",ADVANCE="NO")" '",TRIM(name),"'"
       ELSE
          WRITE(INDSTR,"(I12)")ICP(I)
          WRITE(7,"(3X,A)",ADVANCE="NO")TRIM(ADJUSTL(INDSTR))
       ENDIF
    ENDDO
    WRITE(7,*)

    CALL AUTOFLUSH(7)

  CONTAINS

    SUBROUTINE WRITELIST(NAME,IVLIST)
      USE AUTO_CONSTANTS, ONLY: INDEXVAR
      CHARACTER(LEN=*), INTENT(IN) :: NAME
      TYPE(INDEXVAR), INTENT(IN) :: IVLIST(:)
      
      LOGICAL FIRST
      CHARACTER(LEN=15) :: INDSTR
      INTEGER INDX,io
      CHARACTER(LEN=19) :: VARSTR

      WRITE(7,"(A,A)", ADVANCE="NO")NAME,'{'
      FIRST=.TRUE.
      DO I=1,SIZE(IVLIST)
         IF(.NOT.FIRST)WRITE(7,"(A)", ADVANCE="NO")", "
         READ(IVLIST(I)%INDEX,*,IOSTAT=io)INDX
         IF(io==0)THEN
            INDSTR=IVLIST(I)%INDEX
         ELSE
            INDSTR="'"//TRIM(IVLIST(I)%INDEX)//"'"
         ENDIF
         IF(INT(IVLIST(I)%VAR)==IVLIST(I)%VAR)THEN
            WRITE(VARSTR,'(I19)')INT(IVLIST(I)%VAR)
         ELSE
            WRITE(VARSTR,'(ES19.10)')IVLIST(I)%VAR
         ENDIF
         WRITE(7,"(A,A,A)", ADVANCE="NO")TRIM(INDSTR),&
              ": ",TRIM(ADJUSTL(VARSTR))
         FIRST=.FALSE.
      ENDDO
      WRITE(7,"(A)", ADVANCE="NO")'}'
    END SUBROUTINE WRITELIST

    SUBROUTINE WRITEUZRLIST(NAME,IVLIST)
      USE AUTO_CONSTANTS, ONLY: INDEXMVAR
      CHARACTER(LEN=*), INTENT(IN) :: NAME
      TYPE(INDEXMVAR), INTENT(IN) :: IVLIST(:)
      
      LOGICAL FIRST
      CHARACTER(LEN=15) :: INDSTR
      INTEGER INDX,I,J,io
      CHARACTER(LEN=19) :: VARSTR
      DOUBLE PRECISION V

      WRITE(7,"(A,A)", ADVANCE="NO")NAME,'{'
      FIRST=.TRUE.
      DO I=1,SIZE(IVLIST)
         IF(.NOT.FIRST)WRITE(7,"(A)", ADVANCE="NO")", "
         READ(IVLIST(I)%INDEX,*,IOSTAT=io)INDX
         IF(io==0)THEN
            INDSTR=IVLIST(I)%INDEX
         ELSE
            INDSTR="'"//TRIM(IVLIST(I)%INDEX)//"'"
         ENDIF
         WRITE(7,"(A,A)", ADVANCE="NO")TRIM(INDSTR),": "
         IF(SIZE(IVLIST(I)%VAR)>1)WRITE(7,"(A)", ADVANCE="NO")'['
         DO J=1,SIZE(IVLIST(I)%VAR)
            IF(J>1)WRITE(7,"(A)", ADVANCE="NO")", "
            V=IVLIST(I)%VAR(J)
            IF(INT(V)==V)THEN
               WRITE(VARSTR,'(I19)')INT(V)
            ELSE
               WRITE(VARSTR,'(ES19.10)')V
            ENDIF
            WRITE(7,"(A)", ADVANCE="NO")TRIM(ADJUSTL(VARSTR))
         ENDDO
         IF(SIZE(IVLIST(I)%VAR)>1)WRITE(7,"(A)", ADVANCE="NO")']'
         FIRST=.FALSE.
      ENDDO
      WRITE(7,"(A)", ADVANCE="NO")'}'
    END SUBROUTINE WRITEUZRLIST

    SUBROUTINE WRITESTRLIST(NAME,ISLIST)
      USE AUTO_CONSTANTS, ONLY: INDEXSTR
      CHARACTER(LEN=*), INTENT(IN) :: NAME
      TYPE(INDEXSTR), INTENT(IN) :: ISLIST(:)
      
      LOGICAL FIRST
      CHARACTER(LEN=15) :: INDSTR

      WRITE(7,"(A,A)", ADVANCE="NO")NAME,'{'
      FIRST=.TRUE.
      DO I=1,SIZE(ISLIST)
         IF(.NOT.FIRST)WRITE(7,"(A)", ADVANCE="NO")", "
         WRITE(INDSTR,'(I15)')ISLIST(I)%INDEX
         WRITE(7,"(A,A,A,A)", ADVANCE="NO")TRIM(ADJUSTL(INDSTR)), &
              ": '",TRIM(ISLIST(I)%STR),"'"
         FIRST=.FALSE.
      ENDDO
      WRITE(7,"(A)", ADVANCE="NO")'}'
    END SUBROUTINE WRITESTRLIST

    SUBROUTINE WRITEINTLIST(NAME,ILIST)
      USE AUTO_CONSTANTS, ONLY: INDEXVAR
      CHARACTER(LEN=*), INTENT(IN) :: NAME
      INTEGER, INTENT(IN) :: ILIST(:)
      
      LOGICAL FIRST
      CHARACTER(LEN=12) :: INDSTR

      IF (SIZE(ILIST) == 0) RETURN
      WRITE(7,"(A,A)", ADVANCE="NO")NAME,'['
      FIRST=.TRUE.
      DO I=1,SIZE(ILIST)
         IF(.NOT.FIRST)WRITE(7,"(A)", ADVANCE="NO")", "
         WRITE(INDSTR,'(I12)')ILIST(I)
         WRITE(7,"(A)", ADVANCE="NO")TRIM(ADJUSTL(INDSTR))
         FIRST=.FALSE.
      ENDDO
      WRITE(7,"(A)", ADVANCE="NO")']'
    END SUBROUTINE WRITEINTLIST

  END SUBROUTINE STHD

! ---------- ------
  SUBROUTINE HEADNG(IAP,ICP,IUNIT,N1,N2)

    USE COMPAT
    USE AUTO_CONSTANTS, ONLY : unames, parnames

! Prints headings above columns on unit 6, 7, and 9.
! N1 = number of parameters to print (maximum: 7 for screen output)
! N2 = number of (max) variables to print (maximum: max(0,7-N1,7))

    INTEGER, INTENT(IN) :: IAP(*),ICP(*),IUNIT,N1,N2
! Local
    INTEGER I,J,IPS,IPLT,NDM
    CHARACTER(LEN=13) name

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
          name = getname(parnames, ICP(I))
          IF(LEN_TRIM(name)>0)THEN
             CALL WRITECOL(-1,name)
          ELSEIF(ICP(I)==11.AND.IPS>0.AND.IPS/=4.AND.IPS/=7)THEN
             CALL WRITECOL(5,'PERIOD')
          ELSEIF(ICP(I)==10.AND.(IPS==5.OR.IPS==15))THEN
             CALL WRITECOL(6,'FOPT')
          ELSEIF(ICP(I)==14.AND.(IPS==14.OR.IPS==16))THEN
             CALL WRITECOL(6,'TIME')
          ELSE
             CALL WRITECOL(4,'PAR',ICP(I))
          ENDIF
       ELSEIF(J==2.AND.(IPLT==0.OR.IPLT<-NDM.OR.IPLT>3*NDM))THEN
          CALL WRITECOL(4,'L2-NORM')
       ELSEIF(J==2)THEN
          name = getname(unames, MOD(ABS(IPLT)-1,NDM)+1)
          IF(LEN_TRIM(name)>0)THEN
             IF(IPLT>NDM.AND.IPLT<=2*NDM) THEN
                CALL WRITECOL(-1, 'INTEGRAL ' // name)
             ELSE IF(IPLT>2*NDM.AND.IPLT<=3*NDM) THEN
                CALL WRITECOL(-1, 'L2-NORM '// name)
             ELSE IF(ABS(IPLT)<=NDM) THEN
                IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                   CALL WRITECOL(2, name)
                ELSE IF(IPLT>0)THEN
                   CALL WRITECOL(-1, 'MAX ' // name)
                ELSE
                   CALL WRITECOL(-1, 'MIN ' // name)
                ENDIF
             ENDIF
          ELSE
             IF(IPLT>NDM.AND.IPLT<=2*NDM) THEN
                CALL WRITECOL(2,'INTEGRAL U',IPLT-NDM)
             ELSE IF(IPLT>2*NDM.AND.IPLT<=3*NDM) THEN
                CALL WRITECOL(2,'L2-NORM U',IPLT-2*NDM)
             ELSE IF(IPLT/=0.AND.ABS(IPLT)<=NDM) THEN
                IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                   CALL WRITECOL(6,'U',ABS(IPLT))
                ELSE IF(IPLT>0)THEN
                   CALL WRITECOL(4,'MAX U',IPLT)
                ELSE
                   CALL WRITECOL(4,'MIN U',-IPLT)
                ENDIF
             ENDIF
          ENDIF
       ELSE !J>2 with N2>0
          name = getname(unames, J-2)
          IF(LEN_TRIM(name)>0)THEN
             IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                CALL WRITECOL(-1,name)
             ELSE
                CALL WRITECOL(-1,'MAX '//name)
             ENDIF
          ELSEIF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
             CALL WRITECOL(6,'U',J-2)
          ELSE
             CALL WRITECOL(4,'MAX U',J-2)
          ENDIF
       ENDIF

    ENDDO

    WRITE(IUNIT,"()")
    CALL AUTOFLUSH(IUNIT)

  CONTAINS

    SUBROUTINE WRITECOL(II,S,N)
      INTEGER, INTENT(IN) :: II
      CHARACTER(*), INTENT(IN) :: S
      INTEGER, INTENT(IN), OPTIONAL :: N
! Local
      CHARACTER(10) SN
      CHARACTER(19) COL
      INTEGER I
      COL=' '
      I=II
      IF(I==-1)THEN
         ! centre into the column
         I=MAX(7-(LEN_TRIM(S)-1)/2,2)
      ENDIF
      IF(PRESENT(N))THEN
         WRITE(SN,"(I10)")N
         WRITE(COL(I:),"(A,A,A,A)") S,'(',TRIM(ADJUSTL(SN)),')'
      ELSE
         WRITE(COL(I:),"(A)") TRIM(S)
      ENDIF
      IF(IUNIT.EQ.7)THEN
         WRITE(IUNIT,"(A19)",ADVANCE="NO")COL
      ELSE
         WRITE(IUNIT,"(A14)",ADVANCE="NO")COL
      ENDIF
    END SUBROUTINE WRITECOL
    
  END SUBROUTINE HEADNG

! ------------ -------- ------
  CHARACTER(2) FUNCTION LBTYPE(ITP)

    ! returns the string label type corresponding to numerical type ITP
    INTEGER, INTENT(IN) :: ITP

    CHARACTER*2, PARAMETER :: ATYPESP(9) = &
         (/ 'BP','LP','HB','  ','LP','BP','PD','TR','EP' /)
    CHARACTER*2, PARAMETER :: ATYPESN(9) = &
         (/ '  ','  ','  ','UZ','  ','  ','  ','  ','MX' /)

    IF(MOD(ITP,10)>0)THEN
       LBTYPE=ATYPESP(MOD(ITP,10))
    ELSEIF(MOD(ITP,10)<0)THEN
       LBTYPE=ATYPESN(-MOD(ITP,10))
    ELSE
       LBTYPE='  '
    ENDIF
  END FUNCTION LBTYPE

! ---------- ------
  SUBROUTINE WRLINE(IAP,PAR,ICU,IBR,NTOT,LAB,VAXIS,U)

    USE COMPAT

! Write one line of output on unit 6 and 7.

    INTEGER, INTENT(IN) :: IAP(*),ICU(*),IBR,NTOT,LAB
    DOUBLE PRECISION, INTENT(IN) :: PAR(*),U(*),VAXIS
! Local
    CHARACTER*2 ATYPE
    CHARACTER(33) :: F69 ! (I4,I6,2X,A2,I5,**********ES14.5)
    CHARACTER(31) :: F7  ! (I4,I6,I4,I5,**********ES19.10)
    INTEGER MTOT,NDM,ITP,NICP,N1,N2,I

    NDM=IAP(23)
    ITP=IAP(27)
    NICP=IAP(35)

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

    ATYPE=LBTYPE(ITP)

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
    IAP(34)=LAB
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

    USE AUTO_CONSTANTS, ONLY: SIRS
    INTEGER, INTENT(INOUT) :: IAP(*)
    INTEGER, INTENT(INOUT) :: IRS
    INTEGER, INTENT(OUT) :: NFPR,NPAR
    LOGICAL, INTENT(OUT) :: FOUND
    CHARACTER*(*), INTENT(IN) :: NAME

    LOGICAL EOF3
    INTEGER IBR,NTOT,ITP,LAB,NFPRR,ISWR,NTPL,NAR,NROWPR,NTST,NCOL,NPARR
    INTEGER ISW,ITPST,I,ios,number
    CHARACTER(2) :: ATYPE

! Locates restart point with label IRS and determines type.
! If the label can not be located on unit 3 then FOUND will be .FALSE.
! If IRS is negative, then just pick the abs(IRS)th solution.

    FOUND=.FALSE.
    NFPR=0
    NPAR=0
    ISW=IAP(10)

    OPEN(3,FILE=NAME,STATUS='old',ACCESS='sequential',IOSTAT=ios)
    IF(ios/=0)THEN
       WRITE(6,'(A,A)')'The solution file (fort.3 or s. file) ',&
            'could not be found.'
       STOP
    ENDIF
    I=0
    READ(SIRS,'(A2,I11)',IOSTAT=ios)ATYPE,number
    IF(ios/=0.OR. &
       (LGE(ATYPE(1:1),'0').AND.LLE(ATYPE(1:1),'9')).OR.ATYPE(1:1)=='-')THEN
       number=0
    ENDIF
    DO
       I=I+1
       READ(3,*,END=2)IBR,NTOT,ITP,LAB,NFPRR,ISWR,NTPL,NAR,NROWPR,NTST, &
            NCOL,NPARR
       IF(IBR>MBR)MBR=IBR
       IF(LAB>MLAB)MLAB=LAB
       IF(number>0.AND.ATYPE==LBTYPE(ITP))THEN
          number=number-1
          IF(number==0)IRS=LAB
       ENDIF

       IF((LAB==IRS.OR.IRS==-I).AND..NOT.FOUND.AND.number==0)THEN
          IRS=LAB
          NFPR=NFPRR
          NPAR=NPARR
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

    USE AUTO_CONSTANTS, ONLY: UVALS, PARVALS, unames, parnames
    INTEGER, INTENT(IN) :: IAP(*)
    INTEGER, INTENT(OUT) :: ICPRS(*)
    DOUBLE PRECISION, INTENT(OUT) :: U(*),UDOT(*),PAR(*)

    DOUBLE PRECISION, ALLOCATABLE :: UX(:),P(:)
    INTEGER I,NFPR,NPARR,NPAR,NDIM,NDIMRD

    NPAR=IAP(31)

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

! override parameter/point values with values from constants file

    DO I=1,SIZE(UVALS)
       U(NAMEIDX(UVALS(I)%INDEX,unames))=UVALS(I)%VAR
    ENDDO
    DO I=1,SIZE(PARVALS)
       PAR(NAMEIDX(PARVALS(I)%INDEX,parnames))=PARVALS(I)%VAR
    ENDDO

    NDIMRD=MIN(NDIM,CURSOL%NAR-1)
    IF(NDIM>CURSOL%NAR-1)THEN
       ! system is extended; call STPNT for extension
       ALLOCATE(UX(NDIM),P(NPAR))
       P(:)=PAR(:NPAR)
       UX(1:NDIMRD)=U(1:NDIMRD)
       UX(NDIMRD+1:NDIM)=0.d0
       CALL STPNT(NDIM,UX,P,0d0)
       U(NDIMRD+1:NDIM)=UX(NDIMRD+1:NDIM)
       DEALLOCATE(UX,P)
    ENDIF

  END SUBROUTINE READLB

! ---------- ------
  SUBROUTINE READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOTRS,UPS, &
       UDOTPS,TM,ITPRS,NDIM)

    USE AUTO_CONSTANTS, ONLY: PARVALS, parnames
    INTEGER, INTENT(IN) :: NDIM
    INTEGER, INTENT(INOUT) :: IAP(*)
    INTEGER, INTENT(OUT) :: ICPRS(*),NTSRS,NCOLRS,NDIMRD,ITPRS
    DOUBLE PRECISION, INTENT(OUT) :: RLDOTRS(*),UPS(NDIM,0:*),UDOTPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(OUT) :: TM(0:*),PAR(*)
! Local
    INTEGER I,J,NFPR,NFPRS,NPARR,NPAR
    DOUBLE PRECISION, ALLOCATABLE :: U(:),P(:)

    NFPR=IAP(29)
    NPAR=IAP(31)
    ITPRS=CURSOL%ITP
    NFPRS=CURSOL%NFPR
    NTSRS=CURSOL%NTST
    NCOLRS=CURSOL%NCOL
    NPARR=CURSOL%NPAR
    IAP(30)=CURSOL%IBR
    IAP(34)=CURSOL%LAB

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

! override parameter values with values from constants file

    DO I=1,SIZE(PARVALS)
       PAR(NAMEIDX(PARVALS(I)%INDEX,parnames))=PARVALS(I)%VAR
    ENDDO

    IF(NDIM>NDIMRD)THEN
       ! system is extended; call STPNT for extension
       ALLOCATE(U(NDIM),P(NPAR))
       P(:)=PAR(:NPAR)
       U(NDIMRD+1:NDIM)=0.d0
       DO J=0,NTSRS*NCOLRS
          U(1:NDIMRD)=UPS(1:NDIMRD,J)
          CALL STPNT(NDIM,U,P,CURSOL%TM(J))
          UPS(NDIMRD+1:NDIM,J)=U(NDIMRD+1:NDIM)
       ENDDO
       DEALLOCATE(U,P)
    ENDIF

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

! ------- -------- -------
  INTEGER FUNCTION NAMEIDX(NAME,NAMES)

    USE AUTO_CONSTANTS, ONLY: INDEXSTR
    CHARACTER(13), INTENT(IN) :: NAME
    TYPE(INDEXSTR), INTENT(IN) :: NAMES(:)
    CHARACTER(13) PNAME
    INTEGER I,SIGN,ios

    ! map a symbolic parameter name or an ascii integer to an index

    SIGN=1
    IF(NAME(1:1)=='-')THEN
       PNAME=NAME(2:)
       SIGN=-1
    ELSE
       PNAME=NAME
    ENDIF
    DO I=1,SIZE(NAMES)
       IF(NAMES(I)%STR==PNAME)THEN
          NAMEIDX=NAMES(I)%INDEX*SIGN
          RETURN
       ENDIF
    ENDDO
    IF(TRIM(PNAME)=='PERIOD')THEN
       NAMEIDX=SIGN*11
    ELSE
       READ(NAME,*,IOSTAT=ios)NAMEIDX
       IF(ios/=0)THEN
          WRITE(6,"(A,A,A)")"Name '",TRIM(NAME),"' not found."
          STOP
       ENDIF
    ENDIF
  END FUNCTION NAMEIDX

END MODULE IO
