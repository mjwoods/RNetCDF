#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF], 1.6.3-2, [michna@giub.unibe.ch])


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

#-- Optional local copy of units file(s) (UDUNITS 1 and 2) ---------------------#
AC_ARG_ENABLE([units-copy],
    AC_HELP_STRING([--enable-units-copy@<:@=PATH@:>@],
                   [place a local copy of units data file(s) in the package @<:@no@:>@]),
    [do_units_copy="${enableval}"], [do_units_copy=no])


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
AC_CHECK_LIB(netcdf, nc_open, [],
[
    AC_CHECK_LIB(netcdf, nc_create, [have_netcdf_hdf5=TRUE],
                 AC_MSG_ERROR(netcdf library not found),
                 [-lhdf5 -lhdf5_hl -lcurl -lz])
    if test "${have_netcdf_hdf5}" = TRUE; then
        LIBS="${LIBS} -lnetcdf -lhdf5 -lhdf5_hl -lcurl -lz"
    fi
])

AC_CHECK_LIB(udunits2, utInit, [], [have_udunits=FALSE])
if test "${have_udunits}" = FALSE; then
    AC_CHECK_LIB(udunits2, utScan, [], [have_udunits_expat=FALSE], [-lexpat])
    if test "${have_udunits_expat}" = FALSE; then
        AC_CHECK_LIB(udunits, main, [], AC_MSG_ERROR(udunits library not found))
    else
        LIBS="${LIBS} -lexpat"
    fi
fi

#-- Check for netcdf.h and udunits.h -------------------------------------------#
AC_CHECK_HEADERS(netcdf.h, [], AC_MSG_ERROR(netcdf header netcdf.h not found))

AC_CHECK_HEADERS(udunits.h, [], AC_MSG_ERROR(udunits header udunits.h not found))

#-- Make local copy of units file(s) (UDUNITS 1 and 2) if requested ------------#
if test "${do_units_copy}" != no ; then
    if test "${do_units_copy}" = yes ; then 
        do_units_copy="${udunits_dat_file}"
    fi
     
    if test -z "${do_units_copy}" ; then
        for dir in /usr/share/udunits /usr/local/share/udunits \
             /usr/etc/udunits.dat /usr/local/etc/udunits.dat ; do
            if test -e "${dir}" ; then
	        do_units_copy="${dir}"
	        break
	    fi
        done
        if test -z "${do_units_copy}" ; then
	    AC_MSG_ERROR([cannot find units file(s) to copy - use --enable-units-copy=PATH])
        fi
    fi

    if test -d "${do_units_copy}" ; then
        mkdir -p inst/udunits 2>/dev/null
        cp -p "${do_units_copy}"/udunits*.xml inst/udunits/
    else
        mkdir inst 2>/dev/null
        cp -p "${do_units_copy}" inst/udunits.dat
    fi
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

