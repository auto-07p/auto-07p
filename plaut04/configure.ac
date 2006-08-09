AC_INIT(src/gmain.c++)

PACKAGE=plaut04
VERSION=0.1

AM_INIT_AUTOMAKE($PACKAGE,$VERSION)

AM_CONFIG_HEADER(config.h)

AC_PROG_CC

AC_PROG_CXX

AC_PROG_INSTALL
AC_LANG_CPLUSPLUS

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
AC_OUTPUT(Makefile src/Makefile)

