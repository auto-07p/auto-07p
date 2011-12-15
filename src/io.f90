!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Output (Algebraic Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE IO

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: READC, FINDLB, READLB, READBV, WRLINE, WRBAR, STHD, NEWLAB, &
       GETNDIM3, GETNTST3, GETNCOL3, GETNFPR3, GETIPS3

  TYPE SOLUTION
     INTEGER :: IBR, NTOT, ITP, LAB, NFPR, ISW, NTPL, NAR, NROWPR, NTST, NCOL,&
          NPAR, NPARI, NDM, IPS, IPRIV
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

! ---------- -----
  SUBROUTINE READC(UNITC,EOF,LINE,NPOS,STR,KEYEND,POS,LISTLEN,IERR)

! Reads the file of continuation constants

    USE AUTO_CONSTANTS

    INTEGER, INTENT(IN) :: UNITC
    LOGICAL, INTENT(OUT) :: EOF
    INTEGER, INTENT(INOUT) :: LINE
    CHARACTER(*), INTENT(INOUT) :: STR
    INTEGER, INTENT(INOUT) :: NPOS
    INTEGER, INTENT(OUT) :: KEYEND, POS, LISTLEN, IERR

    INTEGER I,J,IC,N
    INTEGER NICP
    DOUBLE PRECISION RC
    CHARACTER(LEN=1) :: C,QUOTE,PREV
    LOGICAL QUOTEESC
    INTEGER LISTLEN2,ios

    TYPE INDEXSTRL
       CHARACTER(13) INDEX
       CHARACTER(2048) STRL
    END TYPE INDEXSTRL
    TYPE(INDEXSTRL),ALLOCATABLE :: IVUZRS(:)

    CHARACTER(LEN=*), PARAMETER :: ICONSTANTS(24) = (/                    &
         "NDIM", "IPS ", "ILP ", "NTST", "NCOL", "IAD ", "IADS", "ISP ",  &
         "ISW ", "IPLT", "NBC ", "NINT", "NMX ", "NPR ", "MXBF", "IID ",  &
         "ITMX", "ITNW", "NWTN", "JAC ", "NPAR", "IBR ", "LAB ", "IIS " /)
    CHARACTER(LEN=*), PARAMETER :: RCONSTANTS(10) = (/                    &
         "DS   ", "DSMIN", "DSMAX", "RL0  ", "RL1  ", "A0   ", "A1   ",   &
         "EPSL ", "EPSU ", "EPSS " /)

    EOF=.FALSE.
    POS=0
    KEYEND=0
    LISTLEN=0
    IERR=0
    IF(NPOS==1)THEN
       LINE=LINE+1
       N=READINTEGER(UNITC,STR(1:1),IERR)
       IF(IERR/=0)THEN
          IF(IERR==4)THEN
             IERR=0
             EOF=.TRUE.
          ELSEIF(IERR==-1)THEN
             CALL READOLDC(UNITC,N,EOF,LINE,IERR)
          ENDIF
          RETURN
       ENDIF
       NEWCFILE=.TRUE.
       READ(UNITC,'(A)',IOSTAT=ios) STR(2:)
       IF(ios/=0)THEN
          EOF=.TRUE.
          RETURN
       ENDIF
       QUOTE=' '
       QUOTEESC=.FALSE.
       DO I=1,LEN_TRIM(STR)
          C=STR(I:I)
          IF(QUOTE==' ')THEN
             ! replace a tab with spaces if not in a string
             IF(IACHAR(C)==9)THEN
                STR(I:I)=' '
             ELSEIF(C=="'".OR.C=='"')THEN
                QUOTE=STR(I:I)
             ENDIF
          ELSEIF(C==QUOTE)THEN
             ! ignore "" and ''
             IF(STR(I+1:I+1)==C.OR.QUOTEESC)THEN
                QUOTEESC=.NOT.QUOTEESC
             ELSE
                QUOTE=' '
             ENDIF
          ENDIF
       ENDDO
    ELSE
       STR=STR(NPOS:)
    ENDIF
    STR=ADJUSTL(STR)
    IF(LEN_TRIM(STR)==0)RETURN
    ! comment on line
    IF(STR(1:1)=='#'.OR.STR(1:1)=='!')THEN
       NPOS=1
       RETURN
    ENDIF
    ! look for = after keyword
    KEYEND=SCAN(STR,'= ')-1
    IF(KEYEND==-1)GOTO 3
    POS=SCAN(STR,'=')+1
    STR(POS:)=ADJUSTL(STR(POS:))
    CALL SCANVALUE(UNITC,STR(POS:),NPOS,LISTLEN)
    IF(NPOS/=1)THEN
       NPOS=NPOS+POS-1
    ENDIF
    DO I=1,SIZE(ICONSTANTS)
       IF(STR(1:KEYEND)==TRIM(ICONSTANTS(I)))THEN
          READ(STR(POS:),*,ERR=3)IC
          SELECT CASE(I)
          CASE(1)
             NDIM=IC
          CASE(2)
             IPS=IC
          CASE(3)
             ILP=IC
          CASE(4)
             NTST=IC
          CASE(5)
             NCOL=IC
          CASE(6)
             IAD=IC
          CASE(7)
             IADS=IC
          CASE(8)
             ISP=IC
          CASE(9)
             ISW=IC
          CASE(10)
             IPLT=IC
          CASE(11)
             NBC=IC
          CASE(12)
             NINT=IC
          CASE(13)
             NMX=IC
          CASE(14)
             NPR=IC
          CASE(15)
             MXBF=IC
          CASE(16)
             IID=IC
          CASE(17)
             ITMX=IC
          CASE(18)
             ITNW=IC
          CASE(19)
             NWTN=IC
          CASE(20)
             JAC=IC
          CASE(21)
             NPAR=IC
          CASE(22)
             IBR=IC
          CASE(23)
             LAB=IC
          CASE(24)
             IIS=IC
          END SELECT
          RETURN
       ENDIF
    ENDDO
    DO I=1,SIZE(RCONSTANTS)
       IF(STR(1:KEYEND)==TRIM(RCONSTANTS(I)))THEN
          READ(STR(POS:),*,ERR=3)RC
          SELECT CASE(I)
          CASE(1)
             DS=RC
          CASE(2)
             DSMIN=RC
          CASE(3)
             DSMAX=RC
          CASE(4)
             RL0=RC
          CASE(5)
             RL1=RC
          CASE(6)
             A0=RC
          CASE(7)
             A1=RC
          CASE(8)
             EPSL=RC
          CASE(9)
             EPSU=RC
          CASE(10)
             EPSS=RC
          END SELECT
          RETURN
       ENDIF
    ENDDO
    SELECT CASE(STR(1:KEYEND))
    CASE('IRS')
       READ(STR(POS:),*,ERR=3)SIRS
       READ(SIRS,*,IOSTAT=ios)IRS
       IF(ios/=0)IRS=1
    CASE('ICP')
       NICP=LISTLEN
       DEALLOCATE(ICU)
       ALLOCATE(ICU(NICP))
       READ(STR(POS:),*,ERR=3)ICU            
    CASE('UZR','UZSTOP')
       ALLOCATE(IVUZRS(LISTLEN))
       READ(STR(POS:),*,ERR=3)IVUZRS
       IF(STR(1:KEYEND)=='UZSTOP')THEN
          DO I=1,SIZE(IVUZSTOP)
             DEALLOCATE(IVUZSTOP(I)%VAR)
          ENDDO
          DEALLOCATE(IVUZSTOP)
          ALLOCATE(IVUZSTOP(LISTLEN))
       ELSE
          DO I=1,SIZE(IVUZR)
             DEALLOCATE(IVUZR(I)%VAR)
          ENDDO
          DEALLOCATE(IVUZR)
          ALLOCATE(IVUZR(LISTLEN))
       ENDIF
       DO I=1,LISTLEN
          PREV=' '
          LISTLEN2=0
          DO J=1,LEN_TRIM(IVUZRS(I)%STRL)
             C=IVUZRS(I)%STRL(J:J)
             IF(C/=' '.AND.C/=','.AND.(PREV==' '.OR.PREV==','))THEN
                LISTLEN2=LISTLEN2+1
             ENDIF
             PREV=C
          ENDDO
          IF(STR(1:KEYEND)=='UZSTOP')THEN
             ALLOCATE(IVUZSTOP(I)%VAR(LISTLEN2))
             IVUZSTOP(I)%INDEX=IVUZRS(I)%INDEX
             READ(IVUZRS(I)%STRL,*,ERR=3)IVUZSTOP(I)%VAR
          ELSE
             ALLOCATE(IVUZR(I)%VAR(LISTLEN2))
             IVUZR(I)%INDEX=IVUZRS(I)%INDEX
             READ(IVUZRS(I)%STRL,*,ERR=3)IVUZR(I)%VAR
          ENDIF
       ENDDO
       DEALLOCATE(IVUZRS)
    CASE('THL')
       IF(ALLOCATED(IVTHL))DEALLOCATE(IVTHL)
       ALLOCATE(IVTHL(LISTLEN))
       READ(STR(POS:),*,ERR=3)IVTHL
    CASE('THU')
       DEALLOCATE(IVTHU)
       ALLOCATE(IVTHU(LISTLEN))
       READ(STR(POS:),*,ERR=3)IVTHU
    CASE('SP')
       IF(ALLOCATED(SP))DEALLOCATE(SP)
       ALLOCATE(SP(LISTLEN))
       READ(STR(POS:),*,ERR=3)SP
    CASE('STOP')
       IF(ALLOCATED(STOPS))DEALLOCATE(STOPS)
       ALLOCATE(STOPS(LISTLEN))
       READ(STR(POS:),*,ERR=3)STOPS
    CASE('PAR')
       IF(ALLOCATED(PARVALS))DEALLOCATE(PARVALS)
       ALLOCATE(PARVALS(LISTLEN))
       READ(STR(POS:),*,ERR=3)PARVALS
    CASE('U')
       IF(ALLOCATED(UVALS))DEALLOCATE(UVALS)
       ALLOCATE(UVALS(LISTLEN))
       READ(STR(POS:),*,ERR=3)UVALS
    CASE('parnames')
       IF(ALLOCATED(parnames))DEALLOCATE(parnames)
       ALLOCATE(parnames(LISTLEN))
       READ(STR(POS:),*,ERR=3)parnames
    CASE('unames')
       IF(ALLOCATED(unames))DEALLOCATE(unames)
       ALLOCATE(unames(LISTLEN))
       READ(STR(POS:),*,ERR=3)unames
    CASE('s')
       READ(STR(POS:),*)SFILE
       IF(TRIM(SFILE)=='/')THEN
          ! special case from Python interface: s='/' is followed on
          ! the next lines by the solution
          EOF=.TRUE.
          RETURN
       ENDIF
    CASE('dat')
       READ(STR(POS:),*)DATFILE
    CASE('sv')
       READ(STR(POS:),*)SVFILE
    CASE('e')
       READ(STR(POS:),*)EFILE
    CASE('TY')
       READ(STR(POS:),*,ERR=3)TY
    CASE DEFAULT
       IERR=1
    END SELECT
    RETURN
3   IERR=3
  END SUBROUTINE READC

! ---------- ---------
  SUBROUTINE SCANVALUE(UNITC,STR,NPOS,LISTLEN)
    IMPLICIT NONE

!   Scans STR(:) for a value
!   NPOS points to the next keyword on the same line,
!     or is set to 1 if there is none
!   LISTLEN gives the number of items in lists delimited by [] or {}
!   [] characters are removed

    INTEGER, INTENT(IN) :: UNITC
    CHARACTER(*), INTENT(INOUT) :: STR
    INTEGER, INTENT(OUT) :: NPOS,LISTLEN

    INTEGER I,LEVEL,LENSTR,ios
    CHARACTER(1) C,PREV,QUOTE
    LOGICAL QUOTEESC,ISDICT
    LISTLEN=1
    LEVEL=0
    QUOTE=' '
    QUOTEESC=.FALSE.
    PREV=' '

    NPOS=1
    ISDICT=.FALSE.
    LENSTR=LEN_TRIM(STR)
    I=1
    DO
       IF(I>LENSTR)THEN
          IF(LEVEL==0)EXIT
          LENSTR=LEN_TRIM(STR)
          READ(UNITC,'(A)',IOSTAT=ios) STR(LENSTR+1:)
          IF(ios/=0)EXIT
          LENSTR=LEN_TRIM(STR)
       ENDIF
       NPOS=I
       C=STR(I:I)
       IF(QUOTE==' ')THEN
          SELECT CASE(C)
          CASE(',',' ')
             IF(LEVEL==0)EXIT
             IF(PREV==':'.OR.PREV=='['.OR.PREV=='{')C=PREV
             !eat ',' and ' ' after ':', '[', or '{'
          CASE(':')
             STR(I:I)=','
          CASE(']','}')
             IF(C=='}') ISDICT=.FALSE.
             STR(I:I)=' '
             IF(LEVEL==1.AND.(PREV=='['.OR.PREV=='{'))LISTLEN=0
             LEVEL=LEVEL-1
             IF(C==']'.AND.ISDICT) STR(I:I)="'"
          CASE DEFAULT
             IF((PREV==','.OR.PREV==' ').AND.LEVEL==1)THEN
                LISTLEN=LISTLEN+1
             ENDIF
             SELECT CASE(C)
             CASE('[','{')
                STR(I:I)=' '
                LEVEL=LEVEL+1
                IF(C=='{')THEN
                   ISDICT=.TRUE.
                ELSEIF(ISDICT)THEN
                   STR(I:I)="'"
                ENDIF
             CASE('"',"'")
                QUOTE=C
             END SELECT
          END SELECT
       ELSEIF(C==QUOTE)THEN
          ! ignore "" and ''
          IF(STR(I+1:I+1)==C.OR.QUOTEESC)THEN
             QUOTEESC=.NOT.QUOTEESC
          ELSE
             QUOTE=' '
          ENDIF
       ENDIF
       PREV=C
       I=I+1
    ENDDO
    I=VERIFY(STR(NPOS:)," ,")
    IF(I==0)THEN
       NPOS=1
    ELSE
       NPOS=NPOS+I-1
       IF(NPOS>=LEN_TRIM(STR))NPOS=1
    ENDIF
  END SUBROUTINE SCANVALUE

! ------- -------- -----------
  INTEGER FUNCTION READINTEGER(UNITC,C,IERR)

    ! read arbitrary positive integer from unit 2 without advancing
    ! input, to work around advance='no' restriction for list-directed
    ! input.
    ! on output: C contains the last-read character
    ! ierr:-1: no problems and number read
    !       0: no number read
    !       3: error reading
    !       4: eof
    INTEGER, INTENT(IN) :: UNITC
    CHARACTER(1), INTENT(OUT) :: C
    INTEGER, INTENT(OUT) :: IERR

    INTEGER N

    READINTEGER = 0

    N = 0
    IERR = 0
    DO
       READ(UNITC,'(A1)',ERR=3,END=4,EOR=5,ADVANCE='NO')C
       IF(IERR==0.AND.(IACHAR(C)==9.OR.C==' '))CYCLE
       IF(LLT(C, '0').OR.LGT(C, '9'))EXIT
       IERR = -1
       N = N*10 + IACHAR(C) - IACHAR('0')
    ENDDO
    READINTEGER = N
    RETURN

3   IERR = 3
    RETURN

4   IERR = 4
5   RETURN

  END FUNCTION READINTEGER

! ---------- --------
  SUBROUTINE READOLDC(UNITC,N,EOF,LINE,IERR)

! Reads the continuation constants in the old format
    USE AUTO_CONSTANTS

    INTEGER I
    INTEGER NUZR,NICP
    INTEGER LISTLEN,ios
    CHARACTER(1) C

    INTEGER, INTENT(IN) :: UNITC, N
    LOGICAL, INTENT(INOUT) :: EOF
    INTEGER, INTENT(INOUT) :: LINE
    INTEGER, INTENT(OUT) :: IERR

    NDIM=N
    READ(UNITC,*,ERR=3,END=4) IPS,SIRS,ILP
    READ(SIRS,*,IOSTAT=ios)IRS
    IF(ios/=0)IRS=1
    LINE=LINE+1
    LISTLEN=READINTEGER(UNITC,C,IERR)
    IF(IERR==0.OR.IERR==3)GOTO 3
    IF(IERR==4)GOTO 4
    DEALLOCATE(ICU)
    NICP=LISTLEN
    IF(LISTLEN==0)NICP=1
    ALLOCATE(ICU(NICP))
    ICU(1)='1'
    READ(UNITC,*,ERR=3,END=4) (ICU(I),I=1,LISTLEN)
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4) NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4) NMX,RL0,RL1,A0,A1
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4) NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4) EPSL,EPSU,EPSS
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4) DS,DSMIN,DSMAX,IADS
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4) LISTLEN
    !allocate: no THL vs. non-allocated:default THL (in SUB. INIT1)
    IF(ALLOCATED(IVTHL))DEALLOCATE(IVTHL)
    ALLOCATE(IVTHL(LISTLEN))
    IF(LISTLEN>0)THEN
       DO I=1,LISTLEN
          LINE=LINE+1
          READ(UNITC,*,ERR=3,END=4)IVTHL(I)
       ENDDO
    ENDIF
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4) LISTLEN
    IF(LISTLEN>0)THEN
       DEALLOCATE(IVTHU)
       ALLOCATE(IVTHU(LISTLEN))
       DO I=1,LISTLEN
          LINE=LINE+1
          READ(UNITC,*,ERR=3,END=4)IVTHU(I)
       ENDDO
    ENDIF
    LINE=LINE+1
    READ(UNITC,*,ERR=3,END=4)NUZR
    IF(NUZR>0)THEN
       DO I=1,SIZE(IVUZR)
          DEALLOCATE(IVUZR(I)%VAR)
       ENDDO
       DEALLOCATE(IVUZR)
       ALLOCATE(IVUZR(NUZR))
       DO I=1,NUZR
          LINE=LINE+1
          ALLOCATE(IVUZR(I)%VAR(1))
          READ(UNITC,*,ERR=3,END=4)IVUZR(I)%INDEX,IVUZR(I)%VAR(1)
       ENDDO
    ENDIF
    IERR=-1
    RETURN

3   IERR=3
    RETURN

4   WRITE(6,"(A,I2,A)") &
         " Error in fort.2 or c. file: ends prematurely on line ", LINE,"."
    EOF=.TRUE.

  END SUBROUTINE READOLDC
    
! ---------- ----
  SUBROUTINE STHD(AP,ICP)

    USE COMPAT
    USE SUPPORT, ONLY: LBTYPE
    USE AUTO_CONSTANTS, ONLY : IVTHL, IVTHU, IVUZR, IVUZSTOP, unames, parnames,&
         NDIM, IRS, ILP, ISP, ISW, NBC, NINT, NMX, DS, DSMIN, DSMAX, ICU,&
         EFILE, SVFILE, SFILE, DATFILE, HCONST, NPAR, UVALS, PARVALS, SP, &
         STOPS, IBR, LAB, TY

! Write the values of the user defined parameters on unit 7.
! This identifying information is preceded by a '   0' on each line.
! The first line in the file contains the (generally) user-supplied
! limits of the bifurcation diagram, viz. RL0,RL1,A0 and A1.
! These are often convenient for an initial plot of the diagram.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    CHARACTER (LEN=*), PARAMETER :: D3 = "('   0',3(A8,ES11.4))"
    CHARACTER (LEN=*), PARAMETER :: I4 = "('   0',4(A8,I4))"
    CHARACTER (LEN=*), PARAMETER :: I5 = "('   0',2(A7,I5),3(A8,I4))"
    CHARACTER (LEN=*), PARAMETER :: I6 = "('   0',5(A8,I4))"
    INTEGER NDIMA,IPS,IRSA,ILPA,NTST,NCOL,IAD,IADS,ISPA,ISWA,IPLT,NBCA,NINTA
    INTEGER NMXA,NUZR,NPR,MXBF,IIS,IID,ITMX,ITNW,NWTN,JAC,NFPR,I,NPARA
    INTEGER ITPST,LSV,LDAT,LE,LS,INDX,io
    DOUBLE PRECISION DSA,DSMINA,DSMAXA,RL0,RL1,A0,A1,EPSL,EPSU,EPSS
    CHARACTER(LEN=11) :: SDS, SDSA, SDSMAX, SDSMAXA, SDSMIN, SDSMINA
    CHARACTER(LEN=12) :: INDSTR
    CHARACTER(LEN=13) :: name

    INTEGER ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
    COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV

    NDIMA=AP%NDIM
    IPS=AP%IPS
    IRSA=AP%IRS
    ILPA=AP%ILP
    NTST=AP%NTST
    NCOL=AP%NCOL
    IAD=AP%IAD
    IADS=AP%IADS
    ISPA=AP%ISP
    ISWA=AP%ISW
    IPLT=AP%IPLT
    NBCA=AP%NBC
    NINTA=AP%NINT
    NMXA=AP%NMX
    NUZR=AP%NUZR
    NPR=AP%NPR
    MXBF=AP%MXBF
    IIS=AP%IIS
    IID=AP%IID
    ITMX=AP%ITMX
    ITNW=AP%ITNW
    NWTN=AP%NWTN
    JAC=AP%JAC
    ITPST=AP%ITPST
    NFPR=AP%NFPR
    NPARA=AP%NPAR

    DSA=AP%DS
    DSMINA=AP%DSMIN
    DSMAXA=AP%DSMAX
    RL0=AP%RL0
    RL1=AP%RL1
    A0=AP%A0
    A1=AP%A1
    EPSL=AP%EPSL
    EPSU=AP%EPSU
    EPSS=AP%EPSS

    WRITE(7,"(I4,' ',4ES12.4)")0,RL0,RL1,A0,A1
    WRITE(7,D3)'EPSL=',EPSL,'EPSU =',EPSU, 'EPSS =',EPSS
    WRITE(7,D3)'DS  =',DSA,  'DSMIN=',DSMINA,'DSMAX=',DSMAXA
    WRITE(7,I4)'NDIM=',NDIMA,'IPS =',IPS, 'IRS =',IRSA, 'ILP =',ILPA
    WRITE(7,I4)'NTST=',NTST,'NCOL=',NCOL,'IAD =',IAD, 'ISP =',ISPA
    WRITE(7,I4)'ISW =',ISWA, 'IPLT=',IPLT,'NBC =',NBCA, 'NINT=',NINTA
    WRITE(7,I5)' NMX=',NMXA, 'NPR=', NPR, 'MXBF=',MXBF,'IID =',IID, 'IADS=',IADS
    WRITE(7,I6)'ITMX=',ITMX,'ITNW=',ITNW,'NWTN=',NWTN,'JAC =',JAC,'  NUZR=',NUZR

    IF(IBR>0.OR.LAB>0.OR.LEN_TRIM(TY)>0.OR.IIS<3)THEN
       WRITE(7,"('   0')",ADVANCE="NO")
       IF(IIS<3)THEN
          WRITE(7,"(A8,I4)",ADVANCE="NO")"IIS =",IIS
       ENDIF
       IF(IBR>0)THEN
          WRITE(7,"(A8,I4)",ADVANCE="NO")"IBR =",IBR
       ENDIF
       IF(LAB>0)THEN
          WRITE(7,"(A8,I4)",ADVANCE="NO")"LAB =",LAB
       ENDIF
       IF(LEN_TRIM(TY)>0)THEN
          WRITE(7,"(A9,A,A)",ADVANCE="NO")"TY = '",TRIM(TY),"'"
       ENDIF
       WRITE(7,*)
    ENDIF
    WRITE(7,"(A,I4,A)",ADVANCE="NO")"   0   NPAR=",NPARA
    CALL WRITELIST("   THL = ",IVTHL)
    CALL WRITELIST("    THU = ",IVTHU)
    WRITE(7,*)
    IF(SIZE(IVUZR)>0)THEN
       CALL WRITEUZRLIST("   0   UZR = ",IVUZR)
       WRITE(7,*)
    ENDIF
    IF(SIZE(IVUZSTOP)>0)THEN
       CALL WRITEUZRLIST("   0   UZSTOP = ",IVUZSTOP)
       WRITE(7,*)
    ENDIF
    IF(IPS==9)THEN
       !homcont constants
       WRITE(7,"('   0   ',2(A,I4),2(A8,I2),(A8,I4))") &
            'NUNSTAB=',NUNSTAB,' NSTAB=',NSTAB,&
            'IEQUIB=',HCONST%IEQUIB,'ITWIST=',HCONST%ITWIST,&
            'ISTART=',ISTART
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
    IF(SIZE(STOPS)>0)THEN
       WRITE(7,"(A,A,A)", ADVANCE="NO")"   0   STOP=['",TRIM(STOPS(1)),"'"
       DO I=2,SIZE(STOPS)
          WRITE(7,"(A,A,A)", ADVANCE="NO")", '",TRIM(STOPS(I)),"'"
       ENDDO
       WRITE(7,"(A)")']'
    ENDIF

    WRITE(SDS,    "(ES11.4)")DS
    WRITE(SDSA,   "(ES11.4)")DSA
    WRITE(SDSMIN, "(ES11.4)")DSMIN
    WRITE(SDSMINA,"(ES11.4)")DSMINA
    WRITE(SDSMAX, "(ES11.4)")DSMAX
    WRITE(SDSMAXA,"(ES11.4)")DSMAXA

    IF(SDS/=SDSA.OR.SDSMIN/=SDSMINA.OR.SDSMAX/=SDSMAXA.OR. &
       NDIM/=NDIMA.OR.IRS/=IRSA.OR.ILP/=ILPA.OR.NPAR/=NPARA.OR. &
       NMX/=NMXA.OR.ISP/=ISPA.OR.ISW/=ISWA.OR.NBC/=NBCA.OR.NINT/=NINTA)THEN
       WRITE(7,"('   0   User-specified constants, where different from above:')")
       IF(SDS/=SDSA.OR.SDSMIN/=SDSMINA.OR.SDSMAX/=SDSMAXA)THEN
          WRITE(7,"('   0')", ADVANCE="NO")
          IF(SDS/=SDSA)THEN
             WRITE(7, "(A8,A)", ADVANCE="NO")'DS  =',SDS
          ENDIF
          IF(SDSMIN/=SDSMINA)THEN
             WRITE(7, "(A8,A)", ADVANCE="NO")'DSMIN=',SDSMIN
          ENDIF
          IF(SDSMAX/=SDSMAXA)THEN
             WRITE(7, "(A8,A)", ADVANCE="NO")'DSMAX=',SDSMAX
          ENDIF
          WRITE(7,*)
       ENDIF
       IF(NDIM/=NDIMA.OR.IRS/=IRSA.OR.ILP/=ILPA.OR.NPAR/=NPARA)THEN
          WRITE(7,"('   0')", ADVANCE="NO")
          IF(NDIM/=NDIMA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'NDIM=',NDIM
          ENDIF
          IF(IRS/=IRSA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'IRS =',IRS
          ENDIF
          IF(ILP/=ILPA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'ILP =',ILP
          ENDIF
          IF(NPAR/=NPARA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'NPAR=',NPAR
          ENDIF
          WRITE(7,*)
       ENDIF
       IF(NMX/=NMXA.OR.ISP/=ISPA.OR.ISW/=ISWA.OR.NBC/=NBCA.OR.NINT/=NINTA)THEN
          WRITE(7,"('   0')", ADVANCE="NO")
          IF(NMX/=NMXA)THEN
             WRITE(7, "(A7,I5)", ADVANCE="NO")'NMX=',NMX
          ENDIF
          IF(ISP/=ISPA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'ISP =',ISP
          ENDIF
          IF(ISW/=ISWA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'ISW =',ISW
          ENDIF
          IF(NBC/=NBCA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'NBC =',NBC
          ENDIF
          IF(NINT/=NINTA)THEN
             WRITE(7, "(A8,I4)", ADVANCE="NO")'NINT=',NINT
          ENDIF
          WRITE(7,*)
       ENDIF
    ENDIF
    IF(IPS==9.AND.(NUNSTAB/=HCONST%NUNSTAB.OR.NSTAB/=HCONST%NSTAB.OR. &
         ISTART/=HCONST%ISTART))THEN
       !homcont constants
       WRITE(7,"('   0   ')", ADVANCE="NO")
       IF(NUNSTAB/=HCONST%NUNSTAB)THEN
          WRITE(7,"(A,I4)", ADVANCE="NO")'NUNSTAB=',HCONST%NUNSTAB
       ENDIF
       IF(NSTAB/=HCONST%NSTAB)THEN
          WRITE(7,"(A,I4)", ADVANCE="NO")' NSTAB=',HCONST%NSTAB
       ENDIF
       IF(ISTART/=HCONST%ISTART)THEN
          WRITE(7,"(A8,I4)", ADVANCE="NO")'ISTART=',HCONST%ISTART
       ENDIF
       WRITE(7,*)
    ENDIF

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

    IF(ITPST /= 0)THEN
       WRITE(7,"('   0   Branch type: ',I2,A,A,A)")&
            ITPST,"    TY = '",LBTYPE(ITPST),"'"
    ENDIF

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
  SUBROUTINE HEADNG(AP,ICP,IUNIT,N1,N2)

    USE COMPAT
    USE AUTO_CONSTANTS, ONLY : unames, parnames

! Prints headings above columns on unit 6, 7, and 9.
! N1 = number of parameters to print (maximum: 7 for screen output)
! N2 = number of (max) variables to print (maximum: max(0,7-N1,7))

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),IUNIT,N1,N2
! Local
    INTEGER I,J,IPS,IPLT,NDM
    CHARACTER(LEN=13) name

    IPS=AP%IPS
    IPLT=AP%IPLT
    NDM=AP%NDM

    IF(IUNIT.EQ.7)THEN
       WRITE(7,"(I4/I4,A)",ADVANCE="NO")0,0,'    PT  TY  LAB '
    ELSE
       WRITE(IUNIT,"(/A)",ADVANCE="NO")'  BR    PT  TY  LAB '
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

! ---------- ------
  SUBROUTINE WRLINE(AP,PAR,ICU,IBR,NTOT,LAB,VAXIS,U)

    USE COMPAT
    USE SUPPORT, ONLY: LBTYPE

! Write one line of output on unit 6 and 7.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICU(*),IBR,NTOT,LAB
    DOUBLE PRECISION, INTENT(IN) :: PAR(*),U(*),VAXIS
! Local
    CHARACTER(3) ATYPE
    CHARACTER(33) :: F69 ! (I4,I6,2X,A2,I5,**********ES14.5)
    CHARACTER(31) :: F7  ! (I4,I6,I4,I5,**********ES19.10)
    INTEGER MTOT,NDM,ITP,NICP,N1,N2,I

    NDM=AP%NDM
    ITP=AP%ITP
    NICP=AP%NICP

    N1=NICP
    N2=NDM

    IF(N1.GT.7)THEN
       N1=7
       N2=0
    ELSEIF(N1+N2.GT.7)THEN
       N2=7-N1
    ENDIF

! Write a heading above the first line.

    IF(ABS(NTOT).EQ.1)CALL HEADNG(AP,ICU,6,N1,N2)
    IF(ABS(NTOT).EQ.1)CALL HEADNG(AP,ICU,7,NICP,N2)
    IF(AP%IID>0)CALL HEADNG(AP,ICU,9,N1,N2)

    ATYPE=ADJUSTR(LBTYPE(ITP))

    IF(NTOT>0)THEN
       MTOT=MOD(NTOT-1,9999)+1
    ELSE
       MTOT=-MOD(-NTOT-1,9999)-1
    ENDIF
    WRITE(F69,"(A,I10,A)") '(I4,I6,1X,A3,I5,',N1+N2+1,'ES14.5)'
    WRITE(F7,"(A,I10,A)") '(I4,I6,I4,I5,',NICP+N2+1,'ES19.10)'
    IF(MOD(ITP,10).NE.0)THEN
       WRITE(6,F69)ABS(IBR),ABS(MTOT),ATYPE,LAB,PAR(ICU(1)),VAXIS, &
            (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
       CALL AUTOFLUSH(6)
    ENDIF
    WRITE(7,F7)IBR,MTOT,ITP,LAB,PAR(ICU(1)),VAXIS, &
         (U(I),I=1,N2),(PAR(ICU(I)),I=2,NICP)
    CALL AUTOFLUSH(7)
    IF(AP%IID>0)WRITE(9,F69)IBR,MTOT,ATYPE,LAB,PAR(ICU(1)),VAXIS, &
         (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
  END SUBROUTINE WRLINE

! ---------- -----
  SUBROUTINE WRBAR(C,N)

    CHARACTER(1), INTENT(IN) :: C
    INTEGER, INTENT(IN) :: N
    INTEGER I

    WRITE(9,101)(C,I=1,N)
101 FORMAT(80A1)
  END SUBROUTINE WRBAR

! ---------- ------
  SUBROUTINE NEWLAB(AP)

! Determine a suitable label when restarting.

    USE AUTO_CONSTANTS, ONLY: IBR
    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP

    INTEGER IPS,IRS,ISW,ITP

    IPS=AP%IPS
    IRS=AP%IRS
    ISW=AP%ISW
    ITP=AP%ITP

    IF(AP%LAB.EQ.0)AP%LAB=MLAB+1
    IF(IBR.NE.0)RETURN
    IF(ISW.LT.0.OR.IRS.EQ.0)THEN
       AP%IBR=MBR+1
    ELSEIF( (ABS(ITP).LT.10.AND.ABS(ISW).EQ.2) &
         .OR. ((IPS.EQ.2.OR.IPS.EQ.12).AND.ITP.EQ.3) &
         .OR. (IPS.EQ.4.AND.ISW.EQ.2.AND.ABS(ITP).LT.10) &
         .OR. (IPS.EQ.5.AND.MOD(ITP,10).EQ.2) )THEN
       AP%IBR=IRS
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

! ------- -------- --------
  INTEGER FUNCTION GETIPS3()
    GETIPS3 = CURSOL%IPS
  END FUNCTION GETIPS3

! ---------- ------
  SUBROUTINE FINDLB(AP,UNITC,IRS,NFPR,NPAR,FOUND)

    USE AUTO_CONSTANTS, ONLY: SIRS, SFILE
    USE SUPPORT, ONLY: LBTYPE, AUTOSTOP

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: UNITC
    INTEGER, INTENT(INOUT) :: IRS
    INTEGER, INTENT(OUT) :: NFPR,NPAR
    LOGICAL, INTENT(OUT) :: FOUND

    LOGICAL EOF3
    INTEGER IBR,NTOT,ITP,LAB,NFPRR,ISWR,NTPL,NAR,NROWPR,NTST,NCOL,NPARR
    INTEGER NPARI,NDM,IPS,IPRIV
    INTEGER ISW,ITPST,I,J,ios,number,UNIT
    CHARACTER(3) :: ATYPE
    CHARACTER(100) :: HEADER

! Locates restart point with label IRS and determines type.
! If the label can not be located on unit 3 then FOUND will be .FALSE.
! If IRS is negative, then just pick the abs(IRS)th solution.

    FOUND=.FALSE.
    NFPR=0
    NPAR=0
    ISW=AP%ISW

    UNIT=3
    ios=0
    IF(LEN_TRIM(SFILE)==0)THEN
       OPEN(UNIT,FILE='fort.3',STATUS='old',ACCESS='sequential',IOSTAT=ios)
    ELSEIF(TRIM(SFILE)=='/')THEN
       SFILE=''
       UNIT=UNITC
    ELSE
       OPEN(UNIT,FILE='s.'//SFILE,STATUS='old',ACCESS='sequential',IOSTAT=ios)
    ENDIF
    IF(ios/=0)THEN
       WRITE(6,'(A,A)')'The solution file (fort.3 or s. file) ',&
            'could not be found.'
       CALL AUTOSTOP()
    ENDIF
    I=0
    J=SCAN(SIRS,"-0123456789")
    number=0
    IF(J>2)THEN
       ATYPE=SIRS(1:J-1)
       READ(SIRS(J:),'(I11)',IOSTAT=ios)number
       IF(ios/=0)THEN
          number=0
       ENDIF
    ENDIF
    DO
       I=I+1
       READ(UNIT,'(A)',END=2)HEADER
       IF(LEN_TRIM(HEADER) <= 73)THEN
          READ(HEADER,*)IBR,NTOT,ITP,LAB,NFPRR,ISWR,NTPL,NAR,NROWPR,NTST, &
               NCOL,NPARR
          NPARI=0
          NDM=0
          IPS=0
          IPRIV=0
       ELSE
          READ(HEADER,*)IBR,NTOT,ITP,LAB,NFPRR,ISWR,NTPL,NAR,NROWPR,NTST, &
               NCOL,NPARR,NPARI,NDM,IPS,IPRIV
       ENDIF
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
          IF(AP%ITP==0)AP%ITP=ITP
          IF(AP%IBR==0)AP%IBR=IBR
          IF(ABS(ISW).GE.2)THEN
             IF(ABS(ITP).LT.10)THEN
                ITPST=ABS(ITP)
             ELSE
                ITPST=ABS(ITP/10)
             ENDIF
          ELSE
             ITPST=0
          ENDIF
          AP%ITPST=ITPST
          CALL READSOL(UNIT,IBR,NTOT,ITP,LAB,NFPR,ISWR,NTPL,NAR,NROWPR,NTST,&
               NCOL,NPAR,NPARI,NDM,IPS,IPRIV)
          ! strip internal parameters from returned NPAR so they can
          ! be thrown away when possible
          NPAR=NPAR-NPARI
       ELSE
          CALL SKIPS(UNIT,NROWPR,EOF3)
          IF(EOF3)GOTO 2
       ENDIF
    ENDDO

2   CONTINUE
    CLOSE(UNIT)

  END SUBROUTINE FINDLB

! ---------- -------
  SUBROUTINE READSOL(UNIT,IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,&
       NPAR,NPARI,NDM,IPS,IPRIV)

    INTEGER, INTENT(IN) :: UNIT,IBR,NTOT,ITP,LAB,NFPR
    INTEGER, INTENT(IN) :: ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR
    INTEGER, INTENT(IN) :: NPARI,NDM,IPS,IPRIV

! Local
    INTEGER J, NTNC, NROWPRSMALL

    NULLIFY(ROOTSOL)
    NTNC=NTPL-1

    CALL NEWSOL(IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR,&
         NPARI,NDM,IPS,IPRIV)
    DO J=0,NTNC
       READ(UNIT,*)CURSOL%TM(J),CURSOL%UPS(:,J)
    ENDDO

    NROWPRSMALL=((NAR-1)/7+1)*NTPL + (NPAR+6)/7
    IF(NTST>0.AND.NROWPR>NROWPRSMALL)THEN
       READ(UNIT,*)CURSOL%ICP(:)
       READ(UNIT,*)CURSOL%RLDOT(:)

! Read U-dot (derivative with respect to arclength).

       READ(UNIT,*)CURSOL%UDOTPS(:,:)
    ENDIF

! Read the parameter values.

    READ(UNIT,*)CURSOL%PAR(:)

  END SUBROUTINE READSOL

! ---------- ------
  SUBROUTINE NEWSOL(IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR,&
       NPARI,NDM,IPS,IPRIV)

    INTEGER, INTENT(IN) :: IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,&
         NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
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
    SOL%NPARI = NPARI
    SOL%NDM = NDM
    SOL%IPS = IPS
    SOL%IPRIV = IPRIV
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
  SUBROUTINE READLB(AP,ICPRS,U,UDOT,PAR)

    USE AUTO_CONSTANTS, ONLY: UVALS, PARVALS, unames, parnames
    USE SUPPORT, ONLY: NAMEIDX
    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: ICPRS(*)
    DOUBLE PRECISION, INTENT(OUT) :: U(*),UDOT(*),PAR(*)

    DOUBLE PRECISION, ALLOCATABLE :: UX(:),P(:)
    INTEGER I,NFPR,NPARR,NPAR,NDIM,NDM,NDMRD

    NPAR=AP%NPAR
    NDM=AP%NDM
    IF(AP%IPS==11)NDM=NDM/2

! Reads the restart data for algebraic problems.

    NFPR=CURSOL%NFPR
    NPARR=CURSOL%NPAR
    NDIM=MIN(CURSOL%NAR-1,AP%NDIM)
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

    NDMRD=CURSOL%NDM
    IF(NDMRD==0)THEN
       ! old-style solution file: try to get NDM: this works for
       ! equilibria/maps/timeint problems (not parabolic/optimization)
       NDMRD=CURSOL%NAR-1
       IF(ABS(CURSOL%ISW)==2)THEN
          IF(CURSOL%ITP/10==3)THEN
             NDMRD=NDMRD/3 ! HB (old method)
          ELSE
             NDMRD=(NDMRD-1)/2 ! PD/BP/LP/TR
          ENDIF
       ENDIF
    ENDIF
    IF(NDM>NDMRD)THEN
       ! system is extended; call STPNT for extension
       ALLOCATE(UX(NDM),P(NPAR))
       P(:)=PAR(:NPAR)
       UX(1:NDMRD)=U(1:NDMRD)
       UX(NDMRD+1:NDM)=0.d0
       CALL STPNT(NDM,UX,P,0d0)
       U(NDMRD+1:NDM)=UX(NDMRD+1:NDM)
       DEALLOCATE(UX,P)
    ENDIF

  END SUBROUTINE READLB

! ---------- ------
  SUBROUTINE READBV(AP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOTRS,UPS, &
       UDOTPS,TM,ITPRS,NDIM)

    USE AUTO_CONSTANTS, ONLY: PARVALS, parnames
    USE SUPPORT, ONLY: NAMEIDX
    INTEGER, INTENT(IN) :: NDIM
    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: ICPRS(*),NTSRS,NCOLRS,NDIMRD,ITPRS
    DOUBLE PRECISION, INTENT(OUT) :: RLDOTRS(*),UPS(NDIM,0:*),UDOTPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(OUT) :: TM(0:*),PAR(*)
! Local
    INTEGER I,J,N,NFPR,NFPRS,NPARR,NPARIR,NPAR,NPARI,NDM,NDMRD
    DOUBLE PRECISION, ALLOCATABLE :: U(:),P(:)

    NPARI=AP%NPARI
    NFPR=AP%NFPR
    NPAR=AP%NPAR
    NDM=AP%NDM
    IF(AP%IPS==12)NDM=NDM/2
    ITPRS=CURSOL%ITP
    NFPRS=CURSOL%NFPR
    NTSRS=CURSOL%NTST
    NCOLRS=CURSOL%NCOL
    NPARR=CURSOL%NPAR
    NPARIR=CURSOL%NPARI

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

! Read the internal parameter values
 
    IF(NPARI>0)THEN
       IF(NPARIR>0)THEN
          N=MIN(NPARIR,NPARI)
          PAR(NPAR-NPARI+1:NPAR-NPARI+N)= &
               CURSOL%PAR(NPARR-NPARIR+1:NPARR-NPARIR+N)
       ELSEIF(CURSOL%NDM==0)THEN ! old style solution file, copy from PAR(12:)
          N=MIN(NPARR-11,NPARI)
          IF(N>0)THEN
             PAR(NPAR-NPARI+1:NPAR-NPARI+N)=CURSOL%PAR(12:11+N)
          ENDIF
       ENDIF
    ENDIF

! Read the other parameter values.

    PAR(1:NPARR-NPARIR)=CURSOL%PAR(1:NPARR-NPARIR)

! override parameter values with values from constants file

    DO I=1,SIZE(PARVALS)
       PAR(NAMEIDX(PARVALS(I)%INDEX,parnames))=PARVALS(I)%VAR
    ENDDO

    NDMRD=CURSOL%NDM
    IF(NDMRD==0)THEN
       ! old-style solution file: try to get NDM: this works for
       ! periodic/general BVP problems (not HomCont/parabolic/optimization)
       NDMRD=CURSOL%NAR-1
       IF(ABS(CURSOL%ISW)==2)THEN
          IF(CURSOL%ITP/10==8)THEN
             NDMRD=NDMRD/3 ! TR
          ELSEIF(CURSOL%ITP/10==6.AND.CURSOL%ISW==-2)THEN
             NDMRD=NDMRD/4 ! BP start
          ELSE
             NDMRD=NDMRD/2 ! PD/BP/LP
          ENDIF
       ENDIF
    ENDIF
    IF(NDM>NDMRD)THEN
       ! system is extended; call STPNT for extension
       ALLOCATE(U(NDM),P(NPAR))
       P(:)=PAR(:NPAR)
       U(NDMRD+1:NDM)=0.d0
       DO J=0,NTSRS*NCOLRS
          U(1:NDMRD)=UPS(1:NDMRD,J)
          CALL STPNT(NDM,U,P,CURSOL%TM(J))
          UPS(NDMRD+1:NDM,J)=U(NDMRD+1:NDM)
       ENDDO
       DEALLOCATE(U,P)
    ENDIF

  END SUBROUTINE READBV

! ---------- -----
  SUBROUTINE SKIPS(UNIT,NSKIP,EOF3)

! Skips the specified number of lines on the solution file unit

    INTEGER, INTENT(IN) :: UNIT,NSKIP
    LOGICAL, INTENT(OUT) :: EOF3
    CHARACTER(12) FMT
    INTEGER I

    EOF3=.TRUE.
    IF(NSKIP<=1)THEN
       DO I=1,NSKIP
         READ(UNIT,'(A)',END=2)
       ENDDO
    ELSE
       WRITE(FMT,'(A,I9,A)')'(',NSKIP-1,'/)'
       READ(UNIT,FMT,END=2)
    ENDIF
    EOF3=.FALSE.
2   RETURN

  END SUBROUTINE SKIPS

END MODULE IO
