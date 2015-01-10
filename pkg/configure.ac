AC_INIT(R/load.R.in)

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


if test -z "${NETCDF_PATH}"; then
    AC_CHECK_FILE(/usr/local/include/netcdf.h,
        [USR_LOCAL_NETCDF=TRUE], [USR_LOCAL_NETCDF=FALSE])
        if test "${USR_LOCAL_NETCDF}" = TRUE; then
	    NETCDF_INCDIR="/usr/local/include"
	    NETCDF_LIBDIR="/usr/local/lib"
	    NETCDF_LIBNAME="netcdf"
        fi
    AC_CHECK_FILE(/usr/include/netcdf.h,
        [USR_NETCDF=TRUE], [USR_NETCDF=FALSE])
        if test "${USR_NETCDF}" = TRUE; then
	    NETCDF_INCDIR="/usr/include"
	    NETCDF_LIBDIR="/usr/lib"
	    NETCDF_LIBNAME="netcdf"
        fi
else
    NETCDF_INCDIR="${NETCDF_PATH}/include"
    NETCDF_LIBDIR="${NETCDF_PATH}/lib"
    NETCDF_LIBNAME="netcdf"
fi

if test -z "${UDUNITS_PATH}"; then
    AC_CHECK_FILE(/usr/local/include/udunits.h,
        [USR_LOCAL_UDUNITS=TRUE], [USR_LOCAL_UDUNITS=FALSE])
        if test "${USR_LOCAL_UDUNITS}" = TRUE; then
	    UDUNITS_INCDIR="/usr/local/include"
	    UDUNITS_LIBDIR="/usr/local/lib"
	    UDUNITS_LIBNAME="udunits"
        fi
    AC_CHECK_FILE(/usr/include/udunits.h,
        [USR_UDUNITS=TRUE], [USR_UDUNITS=FALSE])
        if test "${USR_UDUNITS}" = TRUE; then
	    UDUNITS_INCDIR="/usr/include"
	    UDUNITS_LIBDIR="/usr/lib"
	    UDUNITS_LIBNAME="udunits"
        fi
else
    UDUNITS_INCDIR="${UDUNITS_PATH}/include"
    UDUNITS_LIBDIR="${UDUNITS_PATH}/lib"
    UDUNITS_LIBNAME="udunits"
fi


AC_CHECK_FILE(${NETCDF_INCDIR}/netcdf.h, [HAVE_NETCDF_H=TRUE], [HAVE_NETCDF_H=FALSE])
if test "${HAVE_NETCDF_H}" = FALSE; then
    AC_MSG_ERROR(netcdf.h not found)
    exit 1
fi

AC_CHECK_FILE(${UDUNITS_INCDIR}/udunits.h, [HAVE_UDUNITS_H=TRUE], [HAVE_UDUNITS_H=FALSE])
if test "${HAVE_UDUNITS_H}" = FALSE; then
    AC_MSG_ERROR(udunits.h not found)
    exit 1
fi


AC_SUBST(HAVE_NETCDF)
AC_SUBST(NETCDF_INCDIR)
AC_SUBST(NETCDF_LIBDIR)
AC_SUBST(NETCDF_LIBNAME)

AC_SUBST(HAVE_UDUNITS)
AC_SUBST(UDUNITS_INCDIR)
AC_SUBST(UDUNITS_LIBDIR)
AC_SUBST(UDUNITS_LIBNAME)

AC_OUTPUT(R/load.R
          src/Makevars)
