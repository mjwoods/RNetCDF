#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF], 1.5.0-1, [michna@giub.unibe.ch])


#-------------------------------------------------------------------------------#
#  Optional include paths for NetCDF and/or UDUNITS libraries    		#
#-------------------------------------------------------------------------------#

#-- NetCDF include and library path --------------------------------------------#
AC_ARG_WITH([netcdf-include],
    AC_HELP_STRING([--with-netcdf-include=DIR],
                   [location of netcdf header files]),
    [netcdf_include_path=$withval])
if test [ -n "$netcdf_include_path" ] ; then
   AC_SUBST([CPPFLAGS],["-I${netcdf_include_path} ${CPPFLAGS}"])
fi

AC_ARG_WITH([netcdf-lib],
    AC_HELP_STRING([--with-netcdf-lib=DIR],
                   [location of netcdf libraries]),
    [netcdf_lib_path=$withval])
if test [ -n "$netcdf_lib_path" ] ; then
   AC_SUBST([LIBS],["-L${netcdf_lib_path} ${LIBS}"])
fi

#-- UDUNITS include and library path -------------------------------------------#
AC_ARG_WITH([udunits-include],
    AC_HELP_STRING([--with-udunits-include=DIR],
                   [location of udunits header files]),
    [udunits_include_path=$withval])
if test [ -n "$udunits_include_path" ] ; then
   AC_SUBST([CPPFLAGS],["-I${udunits_include_path} ${CPPFLAGS}"])
fi

AC_ARG_WITH([udunits-lib],
    AC_HELP_STRING([--with-udunits-lib=DIR],
                   [location of udunits libraries]),
    [udunits_lib_path=$withval])
if test [ -n "$udunits_lib_path" ] ; then
   AC_SUBST([LIBS],["-L${udunits_lib_path} ${LIBS}"])
fi

#-- Optional include path for udunits.dat (UDUNITS 1) --------------------------#
AC_ARG_WITH([udunits-dat],
    AC_HELP_STRING([--with-udunits-dat=FILE],
                   [location of udunits.dat file]),
    [udunits_dat_file=$withval])

#-- Optional include path for HDF5 ---------------------------------------------#
AC_ARG_WITH([hdf5-lib],
    AC_HELP_STRING([--with-hdf5-lib=DIR],
                   [location of hdf5 libraries]),
    [hdf5_lib_path=$withval])
if test [ -n "$hdf5_lib_path" ] ; then
   AC_SUBST([LIBS],["-L${hdf5_lib_path} ${LIBS}"])
fi
    

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

#-- Check for NetCDF library ---------------------------------------------------#
AC_HAVE_LIBRARY(netcdf, [have_netcdf=TRUE], [have_netcdf=FALSE])
if test "${have_netcdf}" = FALSE; then
    AC_MSG_ERROR(netcdf library not found)
    exit 1
fi

#-- Check for UDUNITS library (including deprecated version 1) -----------------#
AC_HAVE_LIBRARY(udunits2, [have_udunits_2=TRUE], [have_udunits_2=FALSE])
if test "${have_udunits_2}" = FALSE; then
    AC_HAVE_LIBRARY(udunits, [have_udunits=TRUE], [have_udunits=FALSE])
        if test "${have_udunits}" = FALSE; then
        AC_MSG_ERROR(udunits2 or udunits library not found)
        exit 1
    fi
fi

#-- Check for netcdf.h and udunits.h -------------------------------------------#
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

#-- Get udunits version for AC_SUBST -------------------------------------------#
if test "${have_udunits_2}" = TRUE; then
    AC_SUBST([LIBS],["${LIBS} -lnetcdf -ludunits2"])
fi

if test "${have_udunits}" = TRUE; then
    AC_SUBST([LIBS],["${LIBS} -lnetcdf -ludunits"])
fi

#-- Check if HDF5 linking is needed --------------------------------------------#
AC_HAVE_LIBRARY(hdf5, [have_hdf5=TRUE], [have_hdf5=FALSE])
if test "${have_hdf5}" = TRUE; then
    AC_SUBST([LIBS],["${LIBS} -lhdf5 -lhdf5_hl"])
fi


#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_SUBST(udunits_dat_file)
AC_OUTPUT(R/load.R
          src/Makevars)

#-------------------------------------------------------------------------------#

