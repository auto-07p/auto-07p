AC_INIT(plaut04/src/gmain.c++)

AC_CONFIG_HEADERS(include/config.h)

AC_ARG_VAR(FC, [Choose a Fortran (>=90) compiler other than the default])
AC_ARG_VAR(MPIFC, [Command to use to compile MPI programs (default:  mpif90)])

AC_ARG_WITH(optimization-flags,
[  --with-optimization-flags=FLAGS       Choose an optimization flag to give to the compilers (default: -O)],
[OPT_FLAG=$withval],[OPT_FLAG="-O"])

AC_ARG_WITH(openmp,
[  --with-openmp           Try to use OpenMP (default: yes)],
[openmp=$withval],[openmp="yes"])

AC_ARG_WITH(mpi,
[  --with-mpi              Try to use MPI (default: no)],
[mpi=$withval],[mpi="no"])

AC_ARG_ENABLE(plaut,
[  --enable-plaut          Compile the Plaut plotting package (default: yes)],
[ENABLE_PLAUT=$enableval],ENABLE_PLAUT=yes)

AC_ARG_ENABLE(plaut04,
[  --enable-plaut04        Compile the Plaut04 plotting package (default: yes)],
[ENABLE_PLAUT04=$enableval],ENABLE_PLAUT04=yes)

AC_ARG_ENABLE(plaut04-qt,
[  --enable-plaut04-qt     Try to use QT instead of Motif/LessTif for Plaut04 (default: yes)],
[ENABLE_PLAUT04_QT=$enableval],ENABLE_PLAUT04_QT=yes)

AC_ARG_ENABLE(gui,
[  --enable-gui            Compile the Motif based GUI package (default: no)],
[ENABLE_GUI=$enableval],ENABLE_GUI=no)

AC_ARG_ENABLE(tek2ps,
[  --enable-tek2ps         Compile the Tek2ps conversion utility (default: yes)],
[ENABLE_TEK2PS=$enableval],ENABLE_TEK2PS=yes)

AC_SUBST(OPT_FLAG)

dnl Checks for programs.

MPIFILE=nompi.f90
if test x$mpi = "xyes"; then
  # We first check for the MPI compilation script.  If it exists
  # we try and us it to override the default compiler.
  if test x$MPIFC = "x"; then
    # sometimes (FC4/5) mpif77 actually calls f95...
    AC_CHECK_PROGS(MPIFC,[mpif90 mpif95 mpif77],"no")
    if test x$MPIFC = "xmpif77"; then
      if ! ( $MPIFC -v 2>&1 | grep f95 > /dev/null ) ; then
	MPIFC=no
      fi
    fi
  fi
  if test x$MPIFC != "xno"; then
    MPI="yes"
    MPIFILE=mpi.f90
    FC=$MPIFC
  fi
fi
AC_SUBST(MPIFILE)

AC_PROG_CC
# adjust include path if gfortran is installed without MinGW on Windows
AC_COMPILE_IFELSE([#include <stdio.h>],,
  [CPPFLAGS="$CPPFLAGS -iwithprefix ../../../../include"])

AC_PROG_INSTALL

AC_LANG_PUSH(Fortran)
if test x$FCFLAGS != "x"; then
  FFLAGS=$FCFLAGS
fi
AC_PROG_FC
AC_FC_SRCEXT(f90)
AC_FC_FREEFORM(,
  echo Fortran 77 compilers no longer work to compile AUTO.
  echo Please check with the manual.
  exit 1
)
# ifc needs -Vaxlib to use FLUSH and ETIME.
COMPAT="f2003"
AC_DEFUN([F2KPROG], [
  program main
  real t
  character*80 s
  print *,'hello'
  flush(6)
  call cpu_time(t)
  t=command_argument_count()
  call get_command_argument(0,s)
  end
])
AC_DEFUN([UNIXPROG], [
  program main
  real t(2),r,etime
  character*80 s
  print *,'hello'
  call flush(6)
  r=etime(t)
  t=iargc()
  call getarg(0,s)
  end
])
AC_MSG_CHECKING([for F2003 or pre-F2003 compatibility functions])
AC_LINK_IFELSE([F2KPROG],,[
 COMPAT="unix"; AC_LINK_IFELSE([UNIXPROG],,
  [tmp=$FCFLAGS; FCFLAGS="$FCFLAGS -Vaxlib"
   AC_LINK_IFELSE([UNIXPROG],
   FFLAGS="$FFLAGS -Vaxlib",[FCFLAGS=$tmp;COMPAT="compat"])])])
AC_SUBST(COMPAT)
AC_MSG_RESULT([$COMPAT.f90])
AC_FC_DUMMY_MAIN([fc_dummy="yes"],[fc_dummy="no"])
realcc=$CC
if test x$fc_dummy = "xno"; then
    # Use gfortran/g95 to link wrapper functions if possible.
    # It appears that otherwise AC_FC_WRAPPERS gets confused on Windows,
    # because of spaces in names such as 'Program Files'
    CC=$FC
    # avoid caching previous result
    $as_unset ac_cv_fc_dummy_main
    AC_FC_DUMMY_MAIN([fc_dummy="yes"],[fc_dummy="no"])
fi
if test x$fc_dummy = "xno"; then
    AC_MSG_WARN(["Warning: can't link C with Fortran"])
fi
AC_FC_WRAPPERS
CC=$realcc
if test x$openmp = "xyes"; then
  # Check for OpenMP.  If it exists we try to use it.
  AX_OPENMP(FFLAGS="$FFLAGS $OPENMP_FCFLAGS",openmp="no")
fi
AC_LANG_POP

AC_PROG_MAKE_SET

dnl Checks for libraries.
#Check for the default X stuff
AC_PATH_XTRA

# We also need Motif.  If we don't find it, X is not useful for us
# so we set the same no_x variable as the above macro
#Now we want to check to see is Motif is installed
AC_CHECK_LIB(Xm,XmCreateMainWindow,X_LIBS="$X_LIBS -lXm -lXt -lX11",
             no_x=yes,$X_PRE_LIBS $X_LIBS -lXt -lX11 $X_EXTRA_LIBS)

dnl Checks for header files.
AC_HEADER_STDC
AC_SUBST(FFLAGS)
CFLAGS="$CPPFLAGS $CFLAGS"

if test x$ENABLE_PLAUT = xyes; then
   PLAUT=plaut
else
   PLAUT=""
fi
AC_SUBST(PLAUT)

if test x$no_x != xyes && test x$ENABLE_GUI = xyes; then
   GUI=gui
else
   GUI=""
fi
AC_SUBST(GUI)

if test x$ENABLE_TEK2PS = xyes; then
   TEK2PS=tek2ps
else
   TEK2PS=""
fi
AC_SUBST(TEK2PS)

dnl AC_HAVE_LIBRARY([m])

AC_PROG_CXX
AC_LANG(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],[HAVE_CXX="yes"],[HAVE_CXX="no"])
PLAUT04_TEST=yes
AC_CHECK_PROG(HAVE_COIN_CONFIG, coin-config, true, false)
COIN_LIBS="-lCoin"
COIN_LDFLAGS=
COIN_INCLUDES=
if test x$HAVE_COIN_CONFIG = "xtrue"; then
	COIN_LIBS="`coin-config --libs`"
	COIN_LDFLAGS="`coin-config --ldflags`"
	COIN_INCLUDES="`coin-config --cppflags`"
fi
tmp_LIBS=$LIBS
LIBS="$LIBS $DEPS_LIBS $COIN_LIBS"
tmp_LDFLAGS=$LDFLAGS
LDFLAGS="$LDFLAGS $COIN_LDFLAGS"
tmp_CPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS $COIN_INCLUDES"
AC_LINK_IFELSE([#include <Inventor/SoDB.h>
                int main(void){SoDB::init();}],
		[COIN3D="yes"],[COIN3D="no"])
if test x$COIN3D = "xno" && test x$CXX = "xg++"; then
        tmp_CXX=$CXX
        # for 64-bit Mac OS try with -m32 to link with 32bit coin
        CXX="g++ -m32"
        AC_LINK_IFELSE([#include <Inventor/SoDB.h>
                        int main(void){SoDB::init();}],
		        [COIN3D="yes"],[COIN3D="no"])
        if test x$COIN3D = "xno"; then
               CXX=$tmp_CXX
        fi
fi
LIBS=$tmp_LIBS
LDFLAGS=$tmp_LDFLAGS
CPPFLAGS=$tmp_CPPFLAGS
if test x$COIN3D = "xno"; then
{
	echo "configure: error: COIN3D is not installed"; 
	AC_CHECK_LIB([Inventor], abort, OI="yes", OI="no")
	if test x$OI = "xno"; then
	{
		echo "configure: error: OI is not installed"; 
		PLAUT04_TEST=no
	}
	else
	{
		DEPS_LIBS="$DEPS_LIBS -lInventor"
		echo "configure: SGI Open Inventor OK. $DEPS_LIBS"; 
		AC_CHECK_LIB([InventorXt],abort,OIXTLIB="yes",OIXTLIB="no")
		if test x$OIXTLIB = "xno"; then
			{ echo "configure: error: SGI Open Inventor Xt Library is not installed"; PLAUT04_TEST=no; }
		else
		{
			DEPS_LIBS="$DEPS_LIBS -lInventorXt"
			echo "configure: SGI Open Inventor Xt OK. $DEPS_LIBS";  
		}
		fi
	}
	fi
}
else
{
	echo "configure: Coin3d OK.";
	SOXT_LDFLAGS=$COIN_LDFLAGS
	SOXT_LIBS=$COIN_LIBS
	SOXT_INCLUDES=$COIN_INCLUDES
	SOXTLIB="no"
	if test x$ENABLE_PLAUT04_QT = xyes; then
	    AC_CHECK_PROG(HAVE_SOQT_CONFIG, soqt-config, true, false)
	    if test x$HAVE_SOQT_CONFIG = "xtrue"; then
		SOXT_LIBS="`soqt-config --libs`"
		SOXT_LDFLAGS="`soqt-config --ldflags`"
		SOXT_INCLUDES="`soqt-config --cppflags` -DSOQT_DLL"
	    fi
	    tmp_LIBS=$LIBS
	    LIBS="$LIBS $DEPS_LIBS $SOXT_LIBS"
	    tmp_LDFLAGS=$LDFLAGS
	    LDFLAGS="$LDFLAGS $SOXT_LDFLAGS"
	    tmp_CPPFLAGS=$CPPFLAGS
	    CPPFLAGS="$CPPFLAGS $SOXT_INCLUDES"
	    AC_LINK_IFELSE([#include <Inventor/Qt/SoQt.h>
                    int main(void){(void)SoQt::init((const char *)0L);}],
                    [SOXTLIB="SoQt"],
              # -lXi is a workaround for Fedora Core 5
	      [LIBS="$LIBS -lXi"
	       SOXT_LIBS="$SOXT_LIBS -lXi"
	       AC_LINK_IFELSE([#include <Inventor/Qt/SoQt.h>
                    int main(void){(void)SoQt::init((const char *)0L);}],
               [SOXTLIB="SoQt"], [SOXTLIB="no"])])
	    LIBS=$tmp_LIBS
	    LDFLAGS=$tmp_LDFLAGS
	    CPPFLAGS=$tmp_CPPFLAGS
	fi
	if test x$SOXTLIB = xno; then
	    AC_CHECK_PROG(HAVE_SOXT_CONFIG, soxt-config, true, false)
	    if test x$HAVE_SOXT_CONFIG = "xtrue"; then
		SOXT_LIBS="`soxt-config --libs`"
		SOXT_LDFLAGS="`soxt-config --ldflags`"
		SOXT_INCLUDES="`soxt-config --cppflags`"
	    fi
	    tmp_LIBS=$LIBS
	    LIBS="$LIBS $DEPS_LIBS $SOXT_LIBS"
	    tmp_LDFLAGS=$LDFLAGS
	    LDFLAGS="$LDFLAGS $SOXT_LDFLAGS"
	    AC_CHECK_LIB([SoXt],abort,SOXTLIB="SoXt",SOXTLIB="no")
	    LIBS=$tmp_LIBS
	    LDFLAGS=$tmp_LDFLAGS
	fi
	if test x$SOXTLIB = "xno"; then
		{ echo "configure: error: Coin3d SoQt/SoXt Library is not installed"; PLAUT04_TEST=no; }
	else
	{
		if test x$HAVE_SOXT_CONFIG = "xfalse"; then
			SOXT_LIBS="-lCoin -l$SOXTLIB"
			if test x$HAVE_COIN_CONFIG = "xtrue"; then
				SOXT_LIBS="$COIN_LIBS -l$SOXTLIB"
				SOXT_LDFLAGS=$COIN_LDFLAGS
			fi
		fi
		DEPS_LIBS="$SOXT_LDFLAGS $DEPS_LIBS $SOXT_LIBS"
		DEPS_INCLUDES="$DEPS_INCLUDES $SOXT_INCLUDES"
		echo "configure: Coin3d $SOXTLIB OK. $DEPS_INCLUDES $DEPS_LIBS";
	}
	fi
}
fi

if (test x$HAVE_SOXT_CONFIG = "xtrue" || test x$HAVE_SOQT_CONFIG = "xtrue" || \
    test x$no_x != xyes) && \
   test x$ENABLE_PLAUT04 = xyes && test x$PLAUT04_TEST = xyes; then
    PLAUT04=plaut04
    if test x$SOXTLIB = "xSoXt"; then
	DEPS_LIBS="$X_PRE_LIBS $DEPS_LIBS $X_LIBS $X_EXTRA_LIBS"
	SOXT_OBJS=gmainxt.o
	SOXT_OBJS_R3B=gmainxt.o
    else
        MOCNAMES="moc-qt3 moc"
        if ( echo $SOXT_LIBS | grep QtGui > /dev/null ); then
            MOCNAMES="moc-qt4 moc"
        fi
	if test -n "$QTDIR"; then
	    mocpath=$QTDIR/bin:$PATH
	else
	    mocpath=$PATH
	fi
	AC_PATH_PROGS(MOC,[$MOCNAMES],false,$mocpath)
	if test x"$MOC" = x"false"; then
	    AC_MSG_WARN([''moc'' Qt preprocessor not found])
	    if test -z "$QTDIR"; then
		AC_MSG_WARN([(QTDIR environment variable not set)])
	    fi
	    PLAUT04=""
	    PLAUT04_TEST="no"
	else
	    SOXT_OBJS="gmainqt.moc.o gmainqt.o"
	    SOXT_OBJS_R3B="../gmainqt.moc.o gmainqt.o"
	    X_CFLAGS="$X_CFLAGS -DUSE_SOQT"
	fi
    fi
else
    PLAUT04=""
    PLAUT04_TEST="no"
fi
AC_SUBST(PLAUT04)
AC_SUBST(MOC)
AC_SUBST(SOXT_OBJS)
AC_SUBST(SOXT_OBJS_R3B)

AC_SUBST(DEPS_LIBS)
AC_SUBST(DEPS_INCLUDES)
AC_OUTPUT(Makefile src/Makefile util/Makefile gui/Makefile tek2ps/Makefile plaut/Makefile plaut04/Makefile plaut04/src/Makefile cmds/cmds.make cmds/Makefile gui/auto.makefile cmds/compat.sh)

if test x$no_x = xyes && test x$ENABLE_GUI = xyes; then
  echo "***************************************************"
  echo "It was requested that the GUI front end be compiled"
  echo "but the proper X libraries (libX and libXt) and/or "
  echo "Motif libraries (libXm) could not be found.  The   "
  echo "compilation of the GUI had been disabled.          "
  echo "***************************************************"
fi 
if test x$ENABLE_PLAUT04 = xyes && test x$PLAUT04_TEST = xno; then
  echo "***************************************************"
  echo "It was requested that the plotting utility PLAUT04 "
  echo "be compiled but the proper libraries (Coin3D and   "
  echo "SoQt or SoXt or Open Inventor and Open Inventor Xt)"
  echo "and perhaps Motif libraries (libXm) could not be   "
  echo "found. The compilation of PLAUT04 is disabled.     "
  if test x$HAVE_CXX = xno; then
    echo "You will also need to install a C++ compiler, e.g. g++."
  fi
  echo "***************************************************"
fi 
if test x$openmp = "xyes" && test x$MPI != "xyes"; then
  echo "***************************************************"
  echo "AUTO has been configured with support for OpenMP"
  echo "***************************************************"
fi
if test x$openmp != "xyes" && test x$MPI = "xyes"; then
  echo "***************************************************"
  echo "AUTO has been configured with support for MPI"   
  echo "***************************************************"
fi
if test x$openmp = "xyes" && test x$MPI = "xyes"; then
  echo "***************************************************"
  echo "AUTO has been configured with support for" 
  echo "OpenMP and MPI"   
  echo "***************************************************"
fi
