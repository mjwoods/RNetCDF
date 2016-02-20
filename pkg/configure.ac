#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF], [1.8-2])

#-------------------------------------------------------------------------------#
#  Find the compiler and compiler options to use   	                 	#
#-------------------------------------------------------------------------------#

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
R_CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
CPPFLAGS="$CPPFLAGS $R_CPPFLAGS"

#-------------------------------------------------------------------------------#
#  Find NetCDF library and header files                                         #
#-------------------------------------------------------------------------------#

AC_ARG_WITH([nc-config],
    AS_HELP_STRING([--without-nc-config],
        [do not use nc-config to get netcdf configuration]),
    [],
    [with_nc_config=yes])

AS_IF([test "x$with_nc_config" != xno],
      [AC_CHECK_PROG(have_nc_config, nc-config, yes, no, [], [])]
)

AS_IF([test "x$have_nc_config" == xyes],
  [
    # Find libraries and cflags used to build netcdf:
    AC_MSG_CHECKING(netcdf linker flags)
    NETCDF_LIBS=`nc-config --libs`
    AC_MSG_RESULT($NETCDF_LIBS)
    LDFLAGS="$NETCDF_LIBS $LDFLAGS"
    AC_MSG_CHECKING(netcdf compiler flags)
    NETCDF_CFLAGS=`nc-config --cflags`
    AC_MSG_RESULT($NETCDF_CFLAGS)
    CFLAGS="$NETCDF_CFLAGS $CFLAGS"
  ], [
    # Check that netcdf header files can be compiled:
    AC_CHECK_HEADERS(netcdf.h, [],
       AC_MSG_ERROR("netcdf.h was not compiled - defining CPPFLAGS may help"))
    # Add netcdf library to LIBS if it can be linked (and is not already being linked):
    AC_SEARCH_LIBS(nc_open, netcdf, [],
        AC_MSG_ERROR("netcdf library was not linked - defining LDFLAGS may help"))
  ]
)

#-------------------------------------------------------------------------------#
#  Find UDUNITS library and header files                                        #
#-------------------------------------------------------------------------------#

# If the expat library is available, prepend it to LIBS
# so that udunits2 can be linked if it is installed.
# If udunits2 contains its own expat functions,
# these will be used in preference to the external library.
AC_SEARCH_LIBS(XML_StopParser, expat)

# Identify available version of udunits library (1 or 2),
# and prepend the library to LIBS if it is not already linked.
# Also check for udunits.h in places that depend on the udunits version,
# and define macro HAVE_UDUNITS_H or HAVE_UDUNITS2_UDUNITS_H accordingly.
udunits=0
AC_SEARCH_LIBS(utFree, udunits2,
  [
    AC_CHECK_HEADERS(udunits2/udunits.h udunits.h, [udunits=2; break])
  ], [
    AC_SEARCH_LIBS(utScan, udunits,
      [
        AC_CHECK_HEADERS(udunits.h, [udunits=1; break])
      ]
    )
  ]
)

# Define variables UDUNITS_LIB, UDUNITS_DBFILE and UDUNITS_DBVAR,
# based on the version of udunits detected above.
# Also define preprocessor macro HAVE_LIBUDUNITS or HAVE_LIBUDUNITS2.
AS_IF([test $udunits == 2],
  [
    UDUNITS_LIB=udunits2
    UDUNITS_DBFILE=udunits2.xml
    UDUNITS_DBVAR="UDUNITS2_XML_PATH"
    AC_DEFINE(HAVE_LIBUDUNITS2)
  ],
  [test $udunits == 1],
  [
    UDUNITS_LIB=udunits
    UDUNITS_DBFILE=udunits.dat
    UDUNITS_DBVAR="UDUNITS_PATH"
    AC_DEFINE(HAVE_LIBUDUNITS)
  ],
  [AC_MSG_ERROR("unable to use udunits2 or udunits")]
)

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_SUBST(UDUNITS_LIB)
AC_SUBST(UDUNITS_DBFILE)
AC_SUBST(UDUNITS_DBVAR)
AC_OUTPUT(R/load.R
          src/Makevars)

#-------------------------------------------------------------------------------#
#  Done                                                                         #
#-------------------------------------------------------------------------------#

