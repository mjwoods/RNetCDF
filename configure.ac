#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF], [2.0-2])

#-------------------------------------------------------------------------------#
#  Find the compiler and compiler options to use for tests                   	#
#-------------------------------------------------------------------------------#

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`

# Prepend user-specified CPPFLAGS to those from R:
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
    # Prepend linker flags to LDFLAGS:
    AC_MSG_CHECKING(netcdf linker flags)
    NETCDF_LIBS=`nc-config --libs`
    AC_MSG_RESULT($NETCDF_LIBS)
    LDFLAGS="$NETCDF_LIBS $LDFLAGS"
    # Prepend preprocessor and compiler flags to CPPFLAGS:
    AC_MSG_CHECKING(netcdf preprocessor and compiler flags)
    NETCDF_CFLAGS=`nc-config --cflags`
    AC_MSG_RESULT($NETCDF_CFLAGS)
    CPPFLAGS="$NETCDF_CFLAGS $CPPFLAGS"
  ]
)

# Check that netcdf header files can be compiled:
AC_CHECK_HEADERS(netcdf.h, [],
    AC_MSG_ERROR("netcdf.h was not compiled - defining CPPFLAGS may help"))

# Check that netcdf library can be found.
# Linker flags are prepended to LIBS if needed.
AC_SEARCH_LIBS(nc_open, netcdf, [],
    AC_MSG_ERROR("netcdf library was not linked - defining LDFLAGS may help"))

# Check for the existence of optional netcdf routines.
# C preprocessor macros HAVE_routine are defined for existing routines.
AC_CHECK_FUNCS([nc_rename_grp nc_get_var_chunk_cache nc_inq_var_szip nc_inq_var_endian nc_inq_var_filter])

#-------------------------------------------------------------------------------#
#  Find UDUNITS2 library and header files                                       #
#-------------------------------------------------------------------------------#

# Prepend the library to LIBS if it is not already being linked,
# and define preprocessor macro HAVE_LIBUDUNITS2.
# Also search for udunits2.h,
# and define macro HAVE_UDUNITS2_H or HAVE_UDUNITS2_UDUNITS2_H accordingly.
AC_SEARCH_LIBS(ut_decode_time, udunits2, AC_DEFINE(HAVE_LIBUDUNITS2))
AC_CHECK_HEADERS(udunits2.h udunits2/udunits2.h, [break])

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_OUTPUT(src/Makevars)

#-------------------------------------------------------------------------------#
#  Done                                                                         #
#-------------------------------------------------------------------------------#

