#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF],[2.7-1])


#-------------------------------------------------------------------------------#
#  Store original environment variables for later use                           #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Store original environment variables for later use:])

AC_MSG_CHECKING([ENV_CC])
ENV_CC="$CC"
AC_MSG_RESULT([${ENV_CC}])

AC_MSG_CHECKING([ENV_CFLAGS])
ENV_CFLAGS="$CFLAGS"
AC_MSG_RESULT([${ENV_CFLAGS}])

AC_MSG_CHECKING([ENV_CPPFLAGS])
ENV_CPPFLAGS="$CPPFLAGS"
AC_MSG_RESULT([${ENV_CPPFLAGS}])

AC_MSG_CHECKING([ENV_LDFLAGS])
ENV_LDFLAGS="$LDFLAGS"
AC_MSG_RESULT([${ENV_LDFLAGS}])

AC_MSG_CHECKING([ENV_LIBS])
ENV_LIBS="$LIBS"
AC_MSG_RESULT([${ENV_LIBS}])

# Clear variables to ensure they are redefined later:
CC=
CFLAGS=
CPPFLAGS=
LDFLAGS=
LIBS=


#-------------------------------------------------------------------------------#
#  Use the compiler and compiler options from R for initial tests               #
#-------------------------------------------------------------------------------#

: ${R_HOME=`R RHOME`}
AS_IF([test -z "${R_HOME}"],
  AC_MSG_ERROR([could not determine R_HOME])
)

AC_MSG_NOTICE([Find compilers and flags from R:])

AC_MSG_CHECKING([R_CC])
R_CC=`"${R_HOME}/bin/R" CMD config CC`
AC_MSG_RESULT([${R_CC}])
CC="${R_CC}"

AC_MSG_CHECKING([R_CFLAGS])
R_CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
AC_MSG_RESULT([${R_CFLAGS}])
CFLAGS="${R_CFLAGS}"

AC_MSG_CHECKING([R_CPPFLAGS])
R_CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_MSG_RESULT([${R_CPPFLAGS}])
CPPFLAGS="${R_CPPFLAGS}"

# No need for R_LDFLAGS; no R_LIBS defined

#-------------------------------------------------------------------------------#
#  Identify OS                                                                  #
#-------------------------------------------------------------------------------#

AC_CHECK_DECLS([_WIN32],
  [platform=Windows],
  [AC_CHECK_DECLS([__APPLE__],
     [platform=macOS],
     [platform=Unix-alike])])
AC_MSG_NOTICE([Operating system is $platform])


#-------------------------------------------------------------------------------#
#  Get package compilers and flags from nc-config or environment variables      #
#-------------------------------------------------------------------------------#

AC_ARG_WITH([nc-config],
  AS_HELP_STRING([--without-nc-config],
    [use environment variables instead of nc-config to get compiler and linker options]),
  [],
  [with_nc_config=yes])

AS_IF([test "x$with_nc_config" != xno],
  [AC_CHECK_PROG(have_nc_config, nc-config, yes, no, [], [])])

AC_ARG_WITH([nc-config-static],
  AS_HELP_STRING([--with-nc-config-static],
    [use static libraries from nc-config (default on Windows & macOS)]),
  [],
  [with_nc_config_static=unset])


AS_IF([test "x$have_nc_config" = xyes],
  [
    AC_MSG_NOTICE([Find package compilers and flags from nc-config:])

    AC_MSG_CHECKING([PKG_CC])
    PKG_CC=`nc-config --cc`
    AS_IF([test -z "${PKG_CC}"],
      [PKG_CC="${R_CC}"]
    )
    AC_MSG_RESULT([${PKG_CC}])

    AC_MSG_CHECKING([PKG_CFLAGS])
    PKG_CFLAGS=`nc-config --cflags`
    AC_MSG_RESULT([${PKG_CFLAGS}])

    AC_MSG_CHECKING([PKG_CPPFLAGS])
    PKG_CPPFLAGS=
    AC_MSG_RESULT([${PKG_CPPFLAGS}])

    AC_MSG_CHECKING([PKG_LDFLAGS])
    PKG_LDFLAGS=
    AC_MSG_RESULT([${PKG_LDFLAGS}])

    AS_IF([test "${with_nc_config_static}" = unset],
      [AS_IF([test "$platform" = Windows -o "$platform" = macOS],
         [with_nc_config_static=yes],
         [with_nc_config_static=no])
      ]
    )

    AS_IF([test "x$with_nc_config_static" = xyes],
      [nc_config_static_flag="--static"
       nc_config_static_label=" (static)"],
      [nc_config_static_flag=""
       nc_config_static_label=""]
    )
    AC_MSG_CHECKING([PKG_LIBS${nc_config_static_label}])
    PKG_LIBS=`nc-config ${nc_config_static_flag} --libs`
    AC_MSG_RESULT([${PKG_LIBS}])
   
  ], [
    AC_MSG_NOTICE([Find package compilers and flags from environment variables:])

    AC_MSG_CHECKING([PKG_CC])
    PKG_CC="${ENV_CC}"
    AS_IF([test -z "${PKG_CC}"],
      [PKG_CC="${R_CC}"])
    AC_MSG_RESULT([${PKG_CC}])

    AC_MSG_CHECKING([PKG_CFLAGS])
    PKG_CFLAGS="${ENV_CFLAGS}"
    AC_MSG_RESULT([${PKG_CFLAGS}])

    AC_MSG_CHECKING([PKG_CPPFLAGS])
    PKG_CPPFLAGS="${ENV_CPPFLAGS}"
    AC_MSG_RESULT([${PKG_CPPFLAGS}])

    AC_MSG_CHECKING([PKG_LDFLAGS])
    PKG_LDFLAGS="${ENV_LDFLAGS}"
    AC_MSG_RESULT([${PKG_LDFLAGS}])

    AC_MSG_CHECKING([PKG_LIBS])
    PKG_LIBS="${ENV_LIBS}"
    AC_MSG_RESULT([${PKG_LIBS}])

  ]
)


AC_MSG_NOTICE([Combine flags from package and R for configure tests:])

AC_MSG_CHECKING([CC])
CC="${PKG_CC}"
AC_MSG_RESULT([$CC])

AC_MSG_CHECKING([CFLAGS])
CFLAGS="${PKG_CFLAGS} ${R_CFLAGS}"
AC_MSG_RESULT([$CFLAGS])

AC_MSG_CHECKING([CPPFLAGS])
CPPFLAGS="${PKG_CPPFLAGS} ${R_CPPFLAGS}"
AC_MSG_RESULT([$CPPFLAGS])

AC_MSG_CHECKING([LIBS])
LIBS="${PKG_LIBS}" # No R_LIBS defined
AC_MSG_RESULT([$LIBS])

AC_MSG_CHECKING([LDFLAGS])
LDFLAGS="${PKG_LDFLAGS}" # No need for R_LDFLAGS
AC_MSG_RESULT([$LDFLAGS])


#-------------------------------------------------------------------------------#
#  Find extra libraries that may be needed on Windows                           #
#-------------------------------------------------------------------------------#
# (nc-config seems not to capture some of these static libraries on Windows)

AS_IF([test "$platform" = Windows], [

  AC_MSG_NOTICE([Find extra libraries that may be needed on Windows:])

  # Start with empty LIBS, storing original LIBS to prepend later:
  PRELIBS="$LIBS"
  LIBS=

  AC_SEARCH_LIBS(atan, m)

  AC_SEARCH_LIBS(getnameinfo, ws2_32)
  AC_SEARCH_LIBS(sethostname, wsock32)
  AC_SEARCH_LIBS(ldap_init, wldap32)
  AC_SEARCH_LIBS(CryptDecodeMessage, crypt32)
  AC_SEARCH_LIBS(GdiConvertBitmap, gdi32)
  AC_SEARCH_LIBS(RegOpenKeyExA, advapi32)

  AC_SEARCH_LIBS(pthread_mutex_init, pthread)

  AC_SEARCH_LIBS(lzma_lzma_decoder_init, lzma)
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

  AC_SEARCH_LIBS(xdr_int, portablexdr)

  AC_SEARCH_LIBS(jpeg_start_compress, jpeg)

  # Prepend original LIBS:
  LIBS="$PRELIBS $LIBS"

])


#-------------------------------------------------------------------------------#
#  Find sizes of selected data types                                            #
#-------------------------------------------------------------------------------#

AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([size_t])


#-------------------------------------------------------------------------------#
#  Check NetCDF features                                                        #
#-------------------------------------------------------------------------------#

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

AC_ARG_WITH([mpitest-cmd],
  [AS_HELP_STRING(
    [--with-mpitest-cmd],
    [MPI command to run small parallel tests after installation - e.g. 'mpiexec -np 2']
  )],
  [mpitest_cmd="$withval"],
  [mpitest_cmd=""])


# MPI support:
has_mpi=TRUE
AC_CHECK_HEADERS([mpi.h], [], [has_mpi=FALSE])

AC_CHECK_FUNCS([MPI_Init], [], [has_mpi=FALSE])

AS_IF([test "$has_mpi" != TRUE],
  [AC_MSG_WARN([MPI not available. To enable MPI, try setting CC=mpicc in ~/.R/Makevars(.win)])])

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

    AC_CHECK_DECLS([MPI_INFO_NULL], [], [has_parallel=FALSE],
      [#include <mpi.h>
      ])

    AC_CHECK_FUNCS(
      [nc_create_par_fortran nc_open_par_fortran nc_var_par_access MPI_Info_c2f],
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

AC_SUBST(mpitest_cmd)


#-------------------------------------------------------------------------------#
#  Final configuration variables                                                #
#-------------------------------------------------------------------------------#

# LIBS is modified during configure, so update PKG_LIBS accordingly.
# Note that LIBS should not include R libraries.
PKG_LIBS="$LIBS" 

# Display PKG variables:
AC_MSG_NOTICE([Final package variables for make:
PKG_CC=${PKG_CC}
PKG_CFLAGS=${PKG_CFLAGS}
PKG_CPPFLAGS=${PKG_CPPFLAGS}
PKG_LDFLAGS=${PKG_LDFLAGS}
PKG_LIBS=${PKG_LIBS}])

AC_SUBST(PKG_CC)
AC_SUBST(PKG_CFLAGS)
AC_SUBST(PKG_CPPFLAGS)
AC_SUBST(PKG_LDFLAGS)
AC_SUBST(PKG_LIBS)
# We also need DEFS, which is substituted automatically

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_CONFIG_FILES([src/Makefile.common R/config.R])
AC_OUTPUT


#-------------------------------------------------------------------------------#
#  Done                                                                         #
#-------------------------------------------------------------------------------#

