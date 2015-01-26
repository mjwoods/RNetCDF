#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF], 1.7.1-1)


#-------------------------------------------------------------------------------#
#  Optional include paths for NetCDF and/or UDUNITS libraries    		#
#-------------------------------------------------------------------------------#

#-- NetCDF include and library path --------------------------------------------#
AC_ARG_WITH([netcdf-include],
    AC_HELP_STRING([--with-netcdf-include=DIR],
                   [directory with netcdf header files]),
    [CPPFLAGS="-I${withval} ${CPPFLAGS}"])

AC_ARG_WITH([netcdf-lib],
    AC_HELP_STRING([--with-netcdf-lib=DIR],
                   [directory with netcdf libraries]),
    [LDFLAGS="-L${withval} ${LDFLAGS}"])

#-- Optional include path for HDF5 ---------------------------------------------#
AC_ARG_WITH([hdf5-lib],
    AC_HELP_STRING([--with-hdf5-lib=DIR],
                   [directory with hdf5 libraries]),
    [LDFLAGS="-L${withval} ${LDFLAGS}"])

#-- UDUNITS include and library path -------------------------------------------#
AC_ARG_WITH([udunits-include],
    AC_HELP_STRING([--with-udunits-include=DIR],
                   [directory with udunits header files]),
    [CPPFLAGS="-I${withval} ${CPPFLAGS}"])

AC_ARG_WITH([udunits-lib],
    AC_HELP_STRING([--with-udunits-lib=DIR],
                   [directory with udunits libraries]),
    [LDFLAGS="-L${withval} ${LDFLAGS}"])

#-------------------------------------------------------------------------------#
#  Find the compiler and compiler options to use   	                 	#
#-------------------------------------------------------------------------------#

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "Could not determine R_HOME"
    exit 1
fi
CC=`${R_HOME}/bin/R CMD config CC`
CFLAGS=`${R_HOME}/bin/R CMD config CFLAGS`

#-- Check for NetCDF and UDUNITS library ---------------------------------------#

# Use tests from autoconf-archive.
# If aclocal.m4 does not exist or requires updating,
# install the autoconf-archive (possibly via your system package manager),
# then run the aclocal command to scan configure.ac for macro references.
# After that, the autoconf command can create the configure script.
AX_LIB_HDF5()
AX_LIB_EXPAT()
LIBS="$LIBS $HDF5_LIBS $EXPAT_LIBS"

# Test netcdf library:
AC_SEARCH_LIBS(nc_open, netcdf, [],
    AC_MSG_ERROR("netcdf library not found"))

# Test udunits2 then udunits library:
AC_SEARCH_LIBS(utScan, udunits2, [have_udunits2=yes], [have_udunits2=no])
if test "$have_udunits2" == yes; then
    UDUNITS_DATABASE=udunits2.xml
else
    AC_SEARCH_LIBS(utScan, udunits, [have_udunits1=yes], [have_udunits1=no])
    if test "$have_udunits1" == yes ; then
        UDUNITS_DATABASE=udunits.dat
    else
        AC_MSG_ERROR("neither udunits2 nor udunits library was found")
    fi
fi

#-- Check for netcdf.h and udunits.h -------------------------------------------#

AC_CHECK_HEADERS(netcdf.h, [], AC_MSG_ERROR("netcdf header netcdf.h not found"))

AC_CHECK_HEADERS(udunits.h, [], AC_MSG_ERROR("udunits header udunits.h not found"))

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_SUBST(UDUNITS_DATABASE)
AC_OUTPUT(R/load.R
          src/Makevars)

#-------------------------------------------------------------------------------#

