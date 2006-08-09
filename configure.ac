AC_INIT(plaut04/src/gmain.c++)

PACKAGE=AUTO
VERSION=07p

AM_INIT_AUTOMAKE($PACKAGE,$VERSION)

dnl AM_CONFIG_HEADER(config.h)

AC_ARG_WITH(CC,
[  --with-CC=COMMAND       Choose a C compiler other then the default],
[CC=$withval],)

AC_ARG_WITH(FC,
[  --with-FC=COMMAND       Choose a Fortran (>=90) compiler other then the default],
[F77=$withval],)

AC_ARG_WITH(mpi-compiler,
[  --with-mpi-compiler=COMMAND       Command to use to compile MPI programs (default:  mpicc)],
[MPI_COMPILER=${withval}],[MPI_COMPILER=mpicc])

AC_ARG_WITH(optimization-flags,
[  --with-optimization-flags=FLAGS       Choose an optimization flag to give to the compilers (default: -O)],
[OPT_FLAG=$withval],[OPT_FLAG="-O"])

#AC_ARG_WITH(mpi,
#[  --with-mpi              Try to use MPI (default: no)],
#[mpi=$withval],[mpi="no"])

AC_ARG_ENABLE(plaut,
[  --enable-plaut          Compile the Plaut plotting package (default: yes)],
[ENABLE_PLAUT=$enableval],ENABLE_PLAUT=yes)

AC_ARG_ENABLE(plaut04,
[  --enable-plaut04        Compile the Plaut04 plotting package (default: yes)],
[ENABLE_PLAUT04=$enableval],ENABLE_PLAUT04=yes)

AC_ARG_ENABLE(gui,
[  --enable-gui          Compile the Motif based GUI package (default: no)],
[ENABLE_GUI=$enableval],ENABLE_GUI=no)

AC_ARG_ENABLE(tek2ps,
[  --enable-tek2ps          Compile the Tek2ps conversion utility (default: yes)],
[ENABLE_TEK2PS=$enableval],ENABLE_TEK2PS=yes)

AC_SUBST(OPT_FLAG)

dnl Checks for programs.

CFLAGS=""

#if test x$mpi = "xyes"; then
  # We first check for the MPI compilation script.  If it exists
  # we try and us it to override the default compiler.
#  AC_PATH_PROG(MPICC,$MPI_COMPILER,"no")
#  if test x$MPICC != "xno"; then
#    MPI="yes"
#    CFLAGS="$CFLAGS -DMPI" 
#    CC=$MPICC
#  fi
#fi

AC_PROG_CC

AC_PROG_CXX

AC_PROG_INSTALL
AC_LANG_CPLUSPLUS

if test "$FC" != "no"; then
  AC_PROG_FC(,[Fortran 90])
  AC_FC_WRAPPERS
else
  FC="true"
  FFLAGS=""
  AC_SUBST(FC)
  AC_SUBST(FFLAGS)
fi
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
AC_SUBST(CFLAGS)

if test x$ENABLE_PLAUT = xyes; then
   PLAUT=plaut
else
   PLAUT=""
fi
AC_SUBST(PLAUT)

if test x$ENABLE_PLAUT04 = xyes; then
   PLAUT04=plaut04
else
   PLAUT04=""
fi
AC_SUBST(PLAUT04)

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

AC_HAVE_LIBRARY([Coin],COIN3D="yes",COIN3D="no")
if test x$COIN3D = "xno"; then
{
	echo "configure: error: COIN3D is not installed"; 
	AC_HAVE_LIBRARY([Inventor], OI="yes", OI="no")
	if test x$OI = "xno"; then
	{
		echo "configure: error: OI is not installed"; 
		exit 1; 
	}
	else
	{
		DEPS_LIBS="$DEPS_LIBS -lInventor"
		echo "configure: SGI Open Inventor OK. $DEPS_LIBS"; 
		AC_HAVE_LIBRARY([InventorXt],OIXTLIB="yes",OIXTLIB="no")
		if test x$OIXTLIB = "xno"; then
			{ echo "configure: error: SGI Open Inventor Xt Library is not installed"; exit 1; }
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
	DEPS_LIBS="$DEPS_LIBS -lCoin"
	echo "configure: Coin3d OK. $DEPS_LIBS"; 

	AC_HAVE_LIBRARY([SoXt],SOXTLIB="yes",SOXTLIB="no")
	if test x$SOXTLIB = "xno"; then
		{ echo "configure: error: Coin3d SoXt Library is not installed"; exit 1; }
	else
	{
		DEPS_LIBS="$DEPS_LIBS -lSoXt"
		echo "configure: Coin3d SoXt OK. $DEPS_LIBS";  
	}
	fi
}
fi

AC_SUBST(DEPS_LIBS)
dnl AC_SUBST(DEPS_LIBS)
AC_OUTPUT(Makefile src/Makefile gui/Makefile tek2ps/Makefile plaut/Makefile plaut04/Makefile plaut04/src/Makefile plaut04/src/r3b/Makefile cmds/cmds.make cmds/Makefile gui/auto.makefile)

if test x$no_x = xyes && test x$ENABLE_GUI = xyes; then
  echo "***************************************************"
  echo "It was requested that the GUI front end be compiled"
  echo "but the proper X libraries (libX and libXt) and/or "
  echo "Motif libraries (libXm) could not be found.  The   "
  echo "compilation of the GUI had been disabled.          "
  echo "***************************************************"
fi 

if test x$PTHREADS = "xyes" && test x$MPI != "xyes"; then
  echo "***************************************************"
  echo "AUTO has been configured with support for Pthreads"   
  echo "***************************************************"
fi
if test x$PTHREADS != "xyes" && test x$MPI = "xyes"; then
  echo "***************************************************"
  echo "AUTO has been configured with support for MPI"   
  echo "***************************************************"
fi
if test x$PTHREADS = "xyes" && test x$MPI = "xyes"; then
  echo "***************************************************"
  echo "AUTO has been configured with support for" 
  echo "Pthreads and MPI"   
  echo "***************************************************"
fi
