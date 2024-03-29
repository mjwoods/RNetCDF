#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF],[2.9-2])

: ${R_HOME=`R RHOME`}
AS_IF([test -z "${R_HOME}"],
  AC_MSG_ERROR([could not determine R_HOME])
)


#-------------------------------------------------------------------------------#
#  Get compiler/linker variables from environment                               #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Find compiler/linker variables from environment:])

AC_MSG_CHECKING([CFLAGS])
PKG_CFLAGS="$CFLAGS"
AC_MSG_RESULT([${PKG_CFLAGS}])

AC_MSG_CHECKING([CPPFLAGS])
PKG_CPPFLAGS="$CPPFLAGS"
AC_MSG_RESULT([${PKG_CPPFLAGS}])

AC_MSG_CHECKING([LDFLAGS])
PKG_LDFLAGS="$LDFLAGS"
AC_MSG_RESULT([${PKG_LDFLAGS}])

AC_MSG_CHECKING([LIBS])
AC_MSG_RESULT([$LIBS])


#-------------------------------------------------------------------------------#
#  Select compiler from R unless --with-mpicc has non-empty value               #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Select C compiler:])

AC_MSG_CHECKING([C compiler from --with-mpicc])

AC_ARG_WITH([mpicc],
  AS_HELP_STRING([--with-mpicc],
    [command for C compiler from MPI, otherwise C compiler from R is used]),
  [CC="$withval"],
  [CC=""])

AC_MSG_RESULT([$CC])

AS_IF([test -z "$CC"],
  [AC_MSG_CHECKING([C compiler from R])
   R_CC=`"${R_HOME}/bin/R" CMD config CC`
   CC="${R_CC}"
   AC_MSG_RESULT([$CC])])


#-------------------------------------------------------------------------------#
#  Get compiler/linker variables from R                                         #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Find compiler/linker variables from R:])

AC_MSG_CHECKING([R_CFLAGS])
R_CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
AC_MSG_RESULT([${R_CFLAGS}])

AC_MSG_CHECKING([R_CPPFLAGS])
R_CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_MSG_RESULT([${R_CPPFLAGS}])

AC_MSG_CHECKING([R_LDFLAGS])
R_LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`
AC_MSG_RESULT([${R_LDFLAGS}])

# No LIBS from R


#-------------------------------------------------------------------------------#
#  Identify OS                                                                  #
#-------------------------------------------------------------------------------#

# Initial compiler test with CC found earlier and compiler flags from R.
# Original variable contents were copied to PKG_ variables earlier.
CFLAGS="${R_CFLAGS}"
CPPFLAGS="${R_CPPFLAGS}"
LDFLAGS="${R_LDFLAGS}"

AC_CHECK_DECLS([_WIN32],
  [platform=Windows],
  [AC_CHECK_DECLS([__APPLE__],
     [platform=macOS],
     [platform=Unix-alike])])
AC_MSG_NOTICE([Operating system is $platform])


#-------------------------------------------------------------------------------#
#  Prepend compiler/linker variables from nc-config (if specified)              #
#-------------------------------------------------------------------------------#

# Enable nc-config by default,
# except on Windows which currently needs special handling (see later):
AC_ARG_WITH([nc-config],
  AS_HELP_STRING([--with-nc-config],
    [get compiler options from nc-config (default except on Windows)]),
  [],
  [with_nc_config=unset])

AS_IF([test "x${with_nc_config}" = xunset],
  [AS_IF([test "x$platform" = xWindows],
     [with_nc_config=no],
     [with_nc_config=yes])])

AS_IF([test "x${with_nc_config}" = xyes],
  [AC_CHECK_PROG(have_nc_config, nc-config, yes, no, [], [])],
  [have_nc_config=no])

AC_ARG_WITH([nc-config-static],
  AS_HELP_STRING([--with-nc-config-static],
    [use static libraries from nc-config (default on Windows & macOS)]),
  [],
  [with_nc_config_static=unset])

AS_IF([test "x${with_nc_config_static}" = xunset],
  [AS_IF([test "x$platform" = xWindows || test "x$platform" = xmacOS],
     [with_nc_config_static=yes],
     [with_nc_config_static=no])
  ]
)

AS_IF([test "x${have_nc_config}" = xyes],
  [
    AC_MSG_NOTICE([Check compiler/linker variables from nc-config:])

    AC_MSG_CHECKING([NC_CC])
    NC_CC=`nc-config --cc`
    AC_MSG_RESULT([${NC_CC}])

    AC_MSG_CHECKING([NC_CC matches selected C compiler])
    AS_IF([test "$CC" != "${NC_CC}"],
          [AC_MSG_RESULT([no ... specify --with-mpicc if needed])],
          [AC_MSG_RESULT([yes])])

    AC_MSG_CHECKING([nc-config --cflags])
    NC_CFLAGS=`nc-config --cflags`
    AC_MSG_RESULT([${NC_CFLAGS}])

    nc_config_static_flag=""
    AS_IF([test "x${with_nc_config_static}" = xyes],
      [AC_MSG_CHECKING([nc-config allows --static option])
       AS_IF([nc-config --libs --static >/dev/null 2>&1],
         [nc_config_static_flag="--static"
          AC_MSG_RESULT([yes])],
         [AC_MSG_RESULT([no])])
      ])

    AC_MSG_CHECKING([nc-config --libs ${nc_config_static_flag}])
    NC_LIBS=`nc-config --libs ${nc_config_static_flag}`
    AC_MSG_RESULT([${NC_LIBS}])

    # Prepend to PKG variables:
    PKG_CFLAGS="${NC_CFLAGS} ${PKG_CFLAGS}"
    LIBS="${NC_LIBS} $LIBS"

  ], [
    AC_MSG_NOTICE([Skipping nc-config])
  ]
)


#-------------------------------------------------------------------------------#
#  Append R compiler/linker variables to PKG variables for feature tests        #
#-------------------------------------------------------------------------------#

# CC is unchanged
CFLAGS="${PKG_CFLAGS} ${R_CFLAGS}"
CPPFLAGS="${PKG_CPPFLAGS} ${R_CPPFLAGS}"
LDFLAGS="${PKG_LDFLAGS} ${R_LDFLAGS}"
# No LIBS from R


#-------------------------------------------------------------------------------#
#  Set libraries to link on Windows                                             #
#-------------------------------------------------------------------------------#
# As of Rtools43, nc-config does not link all required libraries,
# and it also tries to link libraries that are not installed.
# If nc-config is fixed, this section could be removed,
# and nc-config could be re-enabled above.

AS_IF([test "x$platform" = xWindows], [

  AC_MSG_NOTICE([Find libraries that may be needed on Windows:])

  LIBS="-ladvapi32 -lgdi32 -lcrypt32 -lwldap32 -lwsock32 -lws2_32 $LIBS"

  AC_SEARCH_LIBS(atan, m)

  AC_SEARCH_LIBS(pthread_mutex_init, pthread)

  AC_SEARCH_LIBS(lzma_lzma_decoder_init, lzma)
  AC_SEARCH_LIBS(deflate, z)
  AC_SEARCH_LIBS(ZSTD_decompress, zstd)
  AC_SEARCH_LIBS(SZ_BufftoBuffDecompress, sz)

  AC_SEARCH_LIBS(BCryptDecrypt, bcrypt)
  AC_SEARCH_LIBS(gpg_strerror, gpg-error)
  AC_SEARCH_LIBS(gcry_md_open, gcrypt)
  AC_SEARCH_LIBS(EVP_CIPHER_CTX_new, crypto)
  AC_SEARCH_LIBS(SSL_CTX_new, ssl)
  AC_SEARCH_LIBS(libssh2_init, ssh2)

  AC_SEARCH_LIBS(libiconv_open, iconv)
  AC_SEARCH_LIBS(locale_charset, charset)
  AC_SEARCH_LIBS(u8_to_u16, unistring)
  AC_SEARCH_LIBS(idn2_to_ascii_8z, idn2)

  AC_SEARCH_LIBS(H5Fopen, hdf5)
  AC_SEARCH_LIBS(H5TBmake_table, hdf5_hl)

  AC_SEARCH_LIBS(xdr_int, portablexdr)

  AC_SEARCH_LIBS(xmlReadMemory, xml2)

  AC_SEARCH_LIBS(nghttp2_submit_request, nghttp2)
  AC_SEARCH_LIBS(curl_easy_init, curl)

  AC_SEARCH_LIBS(jpeg_start_compress, jpeg)

  AC_SEARCH_LIBS(Hopen, df)
  AC_SEARCH_LIBS(SDstart, mfhdf)

])


#-------------------------------------------------------------------------------#
#  Check type sizes and language features supported by compiler                 #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Check compiler type sizes and language features])

AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([size_t])
AC_C_RESTRICT()


#-------------------------------------------------------------------------------#
#  Check NetCDF features                                                        #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Check NetCDF features:])

# Check that netcdf header files can be compiled:
AC_CHECK_HEADERS(netcdf.h, [],
    AC_MSG_ERROR([netcdf.h was not compiled - defining CPPFLAGS may help]))

# Check that netcdf library can be found.
# Linker flags are prepended to LIBS if needed.
AC_SEARCH_LIBS(nc_open, netcdf, [],
    AC_MSG_ERROR([netcdf library was not linked - defining LDFLAGS may help]))

# Check for the existence of optional netcdf routines.
# C preprocessor macros HAVE_routine are defined for existing routines.
AC_CHECK_FUNCS([nc_rename_grp nc_get_var_chunk_cache nc_inq_var_szip \
  nc_inq_var_endian nc_def_var_filter nc_inq_var_filter_ids nc_inq_var_filter_info \
  nc_reclaim_data])

# Check for filter header file.
AC_CHECK_HEADERS(netcdf_filter.h, [], [],
  [#include <netcdf.h>
  ])

# Check for optional features that depend only on preprocessor declarations:
AC_CHECK_DECLS([NC_64BIT_DATA, NC_FORMAT_64BIT_DATA, NC_DISKLESS, NC_PERSIST],
  [], [],
  [#include <netcdf.h>
  ])


#-------------------------------------------------------------------------------#
#  Check for parallel netcdf features                                           #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Check for parallel NetCDF features:])

AC_ARG_WITH([mpiexec],
  [AS_HELP_STRING(
    [--with-mpiexec],
    [command to run small parallel MPI tests after installation (none by default)]
  )],
  [mpiexec="$withval"],
  [mpiexec=""])


# MPI support:
has_mpi=TRUE
AC_CHECK_HEADERS([mpi.h], [], [has_mpi=FALSE])

AC_CHECK_FUNCS([MPI_Init], [], [has_mpi=FALSE])

AS_IF([test "$has_mpi" != TRUE],
  [AC_MSG_WARN([MPI not available. Specify --with-mpicc if needed.])])

# Parallel I/O support in NetCDF:
AS_IF([test "$has_mpi" = TRUE],
  [
    has_parallel=TRUE
    AC_CHECK_HEADERS([netcdf_par.h], [], [has_parallel=FALSE],
      [#include <netcdf.h>
      ])

    AC_CHECK_DECLS([NC_COLLECTIVE, NC_INDEPENDENT], [], [has_parallel=FALSE],
      [#include <netcdf.h>
       #include <netcdf_par.h>
      ])

    AC_CHECK_DECLS([MPI_INFO_NULL, MPI_Info_c2f], [], [has_parallel=FALSE],
      [#include <mpi.h>
      ])

    AC_CHECK_FUNCS(
      [nc_create_par_fortran nc_open_par_fortran nc_var_par_access],
      [], [has_parallel=FALSE])

    AS_IF([test "$has_parallel" = TRUE],
      [AC_DEFINE(HAVE_NETCDF_MPI)],
      [AC_MSG_WARN([Parallel I/O not supported by NetCDF installation])])

  ], [
    has_parallel=FALSE
    AC_MSG_WARN([NetCDF parallel I/O requires MPI])
  ])


#-------------------------------------------------------------------------------#
#  Find UDUNITS2 library and header files                                       #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Check for udunits2 features:])

# The udunits2 library depends on expat, which may need to be linked explicitly:
AC_SEARCH_LIBS(XML_ErrorString, expat)

# Check that selected routines from udunits2 can be used in programs,
# including udunits2 in LIBS if needed.
# Also search for udunits2.h on its own or in a subdirectory,
# and define macro HAVE_UDUNITS2_H or HAVE_UDUNITS2_UDUNITS2_H accordingly.
has_udunits=FALSE
AC_SEARCH_LIBS(ut_read_xml, udunits2,
  AC_CHECK_FUNC(ut_offset_by_time,
    AC_CHECK_FUNC(ut_decode_time,
      AC_CHECK_FUNC(ut_encode_time,
        AC_CHECK_HEADERS(udunits2.h udunits2/udunits2.h,
          [has_udunits=TRUE; break]
        )
      )
    )
  )
)

# Define HAVE_LIBUDUNITS2 if all udunits2 checks were successful:
AS_IF([test "$has_udunits" = "TRUE"],
  AC_DEFINE(HAVE_LIBUDUNITS2),
  AC_MSG_WARN([disabling calendar functions in RNetCDF])
)


#-------------------------------------------------------------------------------#
#  Indicate optional features to R test script                                  #
#-------------------------------------------------------------------------------#

AS_IF([test "$ac_cv_have_decl_NC_64BIT_DATA" = "yes" -a \
         "$ac_cv_have_decl_NC_FORMAT_64BIT_DATA" = "yes"],
      [has_data64=TRUE],
      [has_data64=FALSE])
AC_SUBST(has_data64)

AS_IF([test "$ac_cv_have_decl_NC_DISKLESS" = "yes" -a \
         "$ac_cv_have_decl_NC_PERSIST" = "yes"],
      [has_diskless=TRUE],
      [has_diskless=FALSE])
AC_SUBST(has_diskless)

AC_SUBST(has_udunits)

AC_SUBST(has_parallel)

AC_SUBST(mpiexec)


#-------------------------------------------------------------------------------#
#  Final configuration variables                                                #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Final compiler and linker variables for make:
CC=$CC
PKG_CFLAGS=${PKG_CFLAGS}
PKG_CPPFLAGS=${PKG_CPPFLAGS}
PKG_LDFLAGS=${PKG_LDFLAGS}
LIBS=${LIBS}])

AC_SUBST(PKG_CFLAGS)
AC_SUBST(PKG_CPPFLAGS)
AC_SUBST(PKG_LDFLAGS)

# CC and LIBS are substituted automatically.


#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_CONFIG_FILES([src/Makefile.common R/config.R])
AC_OUTPUT


#-------------------------------------------------------------------------------#
#  Done                                                                         #
#-------------------------------------------------------------------------------#

