AC_INIT([RNetCDF], 1.2-1, [michna@giub.unibe.ch])


## Optional include paths for netcdf and/or udunits libraries

AC_ARG_WITH([netcdf-include],
    AC_HELP_STRING([--with-netcdf-include=INCLUDE_PATH],
                   [the location of netcdf header files]),
    [netcdf_include_path=$withval])
if test [ -n "$netcdf_include_path" ] ; then
   AC_SUBST([CPPFLAGS],["-I${netcdf_include_path} ${CPPFLAGS}"])
fi

AC_ARG_WITH([netcdf-lib],
    AC_HELP_STRING([--with-netcdf-lib=LIB_PATH],
                   [the location of netcdf libraries]),
    [netcdf_lib_path=$withval])
if test [ -n "$netcdf_lib_path" ] ; then
   AC_SUBST([LIBS],["-L${netcdf_lib_path} ${LIBS}"])
fi

AC_ARG_WITH([udunits-include],
    AC_HELP_STRING([--with-udunits-include=INCLUDE_PATH],
                   [the location of udunits header files]),
    [udunits_include_path=$withval])
if test [ -n "$udunits_include_path" ] ; then
   AC_SUBST([CPPFLAGS],["-I${udunits_include_path} ${CPPFLAGS}"])
fi

AC_ARG_WITH([udunits-lib],
    AC_HELP_STRING([--with-udunits-lib=LIB_PATH],
                   [the location of udunits libraries]),
    [udunits_lib_path=$withval])
if test [ -n "$udunits_lib_path" ] ; then
   AC_SUBST([LIBS],["-L${udunits_lib_path} ${LIBS}"])
fi


## Find the compiler and compiler options to use

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "Could not determine R_HOME"
    exit 1
fi
CC=`${R_HOME}/bin/R CMD config CC`
CFLAGS=`${R_HOME}/bin/R CMD config CFLAGS`


AC_HAVE_LIBRARY(netcdf, [HAVE_NETCDF=TRUE], [HAVE_NETCDF=FALSE])
if test "${HAVE_NETCDF}" = FALSE; then
    AC_MSG_ERROR(netcdf library not found)
    exit 1
fi
 
AC_HAVE_LIBRARY(udunits, [HAVE_UDUNITS=TRUE], [HAVE_UDUNITS=FALSE])
if test "${HAVE_UDUNITS}" = FALSE; then
    AC_MSG_ERROR(udunits library not found)
    exit 1
fi


AC_CHECK_HEADERS(netcdf.h)
if test "${ac_cv_header_netcdf_h}" = no; then
    AC_MSG_ERROR(netcdf header netcdf.h not found)
    exit 1
fi

AC_CHECK_HEADERS(udunits.h)
if test "${ac_cv_header_udunits_h}" = no; then
    AC_MSG_ERROR(udunits header udunits.h not found)
    exit 1
fi

AC_SUBST([LIBS],["${LIBS} -lnetcdf -ludunits"])


## Do substitution

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_OUTPUT(src/Makevars)

