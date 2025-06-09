AC_INIT
AC_CONFIG_SRCDIR([plaut04/src/gmain.c++])

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

AC_ARG_ENABLE(debug,
[  --enable-debug          Enable extra floating point/bounds debugging (default: no)],
[ENABLE_DEBUG=$enableval],[ENABLE_DEBUG="no"])

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
AC_COMPILE_IFELSE([AC_LANG_SOURCE([#include <stdio.h>
	])],,[CPPFLAGS="$CPPFLAGS -iwithprefix ../../../../include"])

AC_PROG_INSTALL
AC_CHECK_PROG(PERL,perl,perl,)
AS_IF([test x"$PERL" != x"perl"], [AC_MSG_ERROR([Please install perl.])])
AC_SUBST(PERL)

AC_LANG_PUSH(Fortran)
if test x$FCFLAGS != "x"; then
  FFLAGS=$FCFLAGS
fi
AC_PROG_FC
if test x$FFLAGS = "x"; then
  if test $ac_cv_prog_fc_g = yes; then
    FFLAGS="-g"
  fi
fi
AC_FC_SRCEXT(f90)
AC_FC_FREEFORM(,
  echo Fortran 77 compilers no longer work to compile AUTO.
  echo Please check with the manual.
  exit 1
)
AC_DEFUN([F2KPROG], [
  program main
  use, intrinsic :: iso_c_binding
  real t
  character*80 s
  print *,'hello'
  flush(6)
  call cpu_time(t)
  t=command_argument_count()
  call get_command_argument(0,s)
  end
])
AC_MSG_CHECKING([for F2003])
AC_LINK_IFELSE([F2KPROG],
  [AC_MSG_RESULT([sufficient Fortran 2003 support found])],
  [AC_MSG_ERROR([AUTO needs a Fortran compiler with sufficient support for Fortran 2003])])
if test x$openmp = "xyes"; then
  # Check for OpenMP.  If it exists we try to use it.
  AX_OPENMP(FFLAGS="$FFLAGS $OPENMP_FCFLAGS",openmp="no")
fi
if test x$ENABLE_DEBUG = "xyes"; then
  if test $ac_cv_fc_compiler_gnu = yes; then
    FFLAGS="$FFLAGS -ffpe-trap=invalid,zero,overflow -finit-real=snan -fcheck=bounds"
  fi
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

dnl AC_CHECK_LIB([m],[main],[],[],[])ac_cv_lib_m=ac_cv_lib_m_main


AC_PROG_CXX
AC_LANG(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],[HAVE_CXX="yes"],[HAVE_CXX="no"])
PLAUT04_TEST=yes
COIN_LDFLAGS=
COIN_INCLUDES=
PKG_PROG_PKG_CONFIG(0.28)
PKG_CHECK_MODULES(COIN, [Coin], [COIN_INCLUDES=$COIN_CFLAGS; HAVE_COIN_CONFIG=true], [
    COIN_LIBS="-lCoin"
    AC_CHECK_PROG(HAVE_COIN_CONFIG, coin-config, true, false)
    if test x$HAVE_COIN_CONFIG = "xtrue"; then
	COIN_LIBS="`coin-config --libs`"
	# workaround if this returns a path without -L
	if test x"${COIN_LIBS#/}" != x"$COIN_LIBS"; then
	    COIN_LIBS="-L$COIN_LIBS"
	fi
	COIN_LDFLAGS="`coin-config --ldflags`"
	COIN_INCLUDES="`coin-config --cppflags`"
    fi])
tmp_LIBS=$LIBS
LIBS="$LIBS $DEPS_LIBS $COIN_LIBS"
tmp_LDFLAGS=$LDFLAGS
LDFLAGS="$LDFLAGS $COIN_LDFLAGS"
tmp_CPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS $COIN_INCLUDES"
AC_LINK_IFELSE([AC_LANG_SOURCE([#include <Inventor/SoDB.h>
                int main(void){SoDB::init();}])],
		[COIN3D="yes"],[COIN3D="no"])
if test x$COIN3D = "xno" && test x$CXX = "xg++"; then
        tmp_CXX=$CXX
        # for 64-bit Mac OS try with -m32 to link with 32bit coin
        CXX="g++ -m32"
        AC_LINK_IFELSE([AC_LANG_SOURCE([#include <Inventor/SoDB.h>
                        int main(void){SoDB::init();}])],
		        [COIN3D="yes"],[COIN3D="no"])
        if test x$COIN3D = "xno"; then
               CXX=$tmp_CXX
        fi
fi
LIBS=$tmp_LIBS
LDFLAGS=$tmp_LDFLAGS
CPPFLAGS=$tmp_CPPFLAGS
SOXTLIB="no"
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
			SOXTLIB="SoXt";
		}
		fi
	}
	fi
	ENABLE_PLAUT04_QT="no"
}
else
{
	echo "configure: Coin3d OK.";
	SOXT_LDFLAGS=$COIN_LDFLAGS
	SOXT_LIBS=$COIN_LIBS
	SOXT_INCLUDES=$COIN_INCLUDES
}
fi

if test x$ENABLE_PLAUT04_QT = xyes; then
    AC_CHECK_PROG(HAVE_SOQT_CONFIG, soqt-config, true, false)
    if test x$HAVE_SOQT_CONFIG = "xtrue"; then
	SOXT_LIBS="`soqt-config --libs`"
	SOXT_LDFLAGS="`soqt-config --ldflags`"
	SOXT_INCLUDES="`soqt-config --cppflags` -DSOQT_DLL"
    else
	SOXT_LIBS="$SOXT_LIBS -lSoQt"
    fi
    LIBS="$tmp_LIBS $DEPS_LIBS $SOXT_LIBS"
    LDFLAGS="$tmp_LDFLAGS $SOXT_LDFLAGS"
    CPPFLAGS="$tmp_CPPFLAGS $SOXT_INCLUDES"
    AC_LINK_IFELSE([AC_LANG_SOURCE([#include <Inventor/Qt/SoQt.h>
                    int main(void){(void)SoQt::init((const char *)0L);}])],
                    [SOXTLIB="SoQt"],
              # -lXi is a workaround for Fedora Core 5
	      [tmp_LIBS1="$LIBS"
               LIBS="$LIBS -lXi"
	       tmp_SOXT_LIBS="$SOXT_LIBS"
	       SOXT_LIBS="$SOXT_LIBS -lXi"
	       AC_LINK_IFELSE([AC_LANG_SOURCE([#include <Inventor/Qt/SoQt.h>
                    int main(void){(void)SoQt::init((const char *)0L);}])],
               [SOXTLIB="SoQt"],[LIBS="$tmp_LIBS1"; SOXT_LIBS="$tmp_SOXT_LIBS"])])
fi

HAVE_QT5="no"
if (test x$ENABLE_PLAUT04_QT = xyes && test x$SOXTLIB = xno); then
    #explicitly look for Qt4 (sometimes necessary on Mac OS)
    PKG_CHECK_MODULES(QT, [QtCore QtGui QtOpenGL],
       [LIBS="$LIBS $QT_LIBS"
	CPPFLAGS="$CPPFLAGS $QT_CFLAGS"
	AC_RUN_IFELSE([AC_LANG_SOURCE([#include <Inventor/Qt/SoQt.h>
                        int main(void){(void)SoQt::init((const char *)0L);}])],
        [SOXTLIB="SoQt"
	 SOXT_INCLUDES="$SOXT_INCLUDES $QT_CFLAGS"
	 SOXT_LIBS="$SOXT_LIBS $QT_LIBS"
	])],[SOXTLIB="no"],[SOXTLIB="no"])
    if test x$SOXTLIB = xno; then
        #check Qt5/Qt6
        #Qt6 may use this path for moc
        if test -x /usr/local/opt/qt/share/qt/libexec/moc; then
            PATH=$PATH:/usr/local/opt/qt/share/qt/libexec
        fi
        AX_HAVE_QT
        if (test x$have_qt == xyes); then
            #allow for SoQt in /usr/local/lib on Mac
            unset MAC_LIBS
            if test -L /usr/local/lib/libSoQt.dylib; then
                MAC_LIBS="-L/usr/local/lib -Wl,-rpath,/usr/local/lib"
            fi
            LIBS="$QT_LIBS $MAC_LIBS $LIBS"
            CPPFLAGS="$QT_CXXFLAGS $CPPFLAGS"
            AC_LINK_IFELSE([AC_LANG_SOURCE([#include <Inventor/Qt/SoQt.h>
                         int main(void){(void)SoQt::init((const char *)0L);}])],
                        [SOXTLIB="SoQt"
			 SOXT_LIBS="$QT_LIBS $MAC_LIBS $SOXT_LIBS"
			 SOXT_INCLUDES="$QT_CXXFLAGS $SOXT_INCLUDES"
                         HAVE_QT5="yes"])
	else
	    SOXTLIB="no"
	fi
    fi
fi

LIBS=$tmp_LIBS
LDFLAGS=$tmp_LDFLAGS
CPPFLAGS=$tmp_CPPFLAGS

if (test x$COIN3D = xyes && test x$SOXTLIB = xno); then
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

if (test x$HAVE_SOXT_CONFIG = "xtrue" || test x$HAVE_SOQT_CONFIG = "xtrue" || \
    test x$no_x != xyes || test x$SOXTLIB != "xno") && \
   test x$ENABLE_PLAUT04 = xyes && test x$PLAUT04_TEST = xyes; then
    PLAUT04=plaut04
    if test x$SOXTLIB = "xSoXt"; then
	DEPS_LIBS="$X_PRE_LIBS $DEPS_LIBS $X_LIBS $X_EXTRA_LIBS"
	SOXT_OBJS=gmainxt.o
	SOXT_OBJS_R3B=gmainxt.o
    else
        MOCNAMES="moc-qt3 moc"
        MOCPATH=""
        if test x$HAVE_QT5 = xyes; then
            MOCNAMES="moc-qt5 moc"
            PKG_CHECK_VAR(MOCPATH,Qt5Core,host_bins)
        elif ( echo $SOXT_LIBS | grep QtGui > /dev/null ); then
            MOCNAMES="moc-qt4 moc"
        fi
	if test -n "$QTDIR"; then
	    mocpath=$QTDIR/bin:$PATH
	elif test -n "$MOCPATH"; then
	    mocpath=$MOCPATH:$PATH
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
AC_CONFIG_FILES([Makefile src/Makefile util/Makefile gui/Makefile plaut/Makefile plaut04/Makefile plaut04/src/Makefile cmds/auto.env cmds/auto.env.sh cmds/cmds.make cmds/Makefile gui/auto.makefile])
AC_OUTPUT

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
