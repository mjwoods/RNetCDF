#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF],[2.7-1])


#-------------------------------------------------------------------------------#
#  Show compiler/linker variables from environment                              #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Show compiler/linker variables from environment:])

AC_MSG_CHECKING([CFLAGS])
AC_MSG_RESULT([${CFLAGS-(unset)}])

AC_MSG_CHECKING([CPPFLAGS])
AC_MSG_RESULT([${CPPFLAGS-(unset)}])

AC_MSG_CHECKING([LDFLAGS])
AC_MSG_RESULT([${LDFLAGS-(unset)}])

AC_MSG_CHECKING([LIBS])
AC_MSG_RESULT([${LIBS-(unset)}])


#-------------------------------------------------------------------------------#
#  Show compiler/linker variables from R                                        #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Show compiler/linker variables from R:])

: ${R_HOME=`R RHOME`}
AS_IF([test -z "${R_HOME}"],
  AC_MSG_ERROR([could not determine R_HOME])
)

AC_MSG_CHECKING([CC])
R_CC=`"${R_HOME}/bin/R" CMD config CC`
AC_MSG_RESULT([${R_CC}])

AC_MSG_CHECKING([CFLAGS])
R_CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
AC_MSG_RESULT([${R_CFLAGS}])

AC_MSG_CHECKING([CPPFLAGS])
R_CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_MSG_RESULT([${R_CPPFLAGS}])

AC_MSG_CHECKING([LDFLAGS])
R_LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`
AC_MSG_RESULT([${R_LDFLAGS}])

# No LIBS from R


#-------------------------------------------------------------------------------#
#  Find first working compiler from nc-config (optional) then R                 #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Find first working compiler from nc-config (optional) then R:])

AC_ARG_WITH([nc-config],
  AS_HELP_STRING([--with-nc-config],
    [get compiler options from nc-config (default)]),
  [],
  [with_nc_config=yes])

AS_IF([test "x${with_nc_config}" = xyes],
  [AC_CHECK_PROG(have_nc_config, nc-config, yes, no, [], [])],
  [have_nc_config=no])

# Check compiler from nc-config, if enabled:
CC=
AC_LANG(C)
AS_IF([test "x${have_nc_config}" = xyes], [
  AC_MSG_CHECKING([nc-config compiler])
  PKG_CC=`nc-config --cc`
  AC_MSG_RESULT([${PKG_CC}])

  # Append any compiler flags from R_CC to PKG_CC,
  # assuming that PKG_CC is an MPI wrapper for R_CC.
  # The compiler pathname could include spaces,
  # so assume first flag begins with "-".
  AC_MSG_CHECKING([append flags from R compiler to nc-config compiler])
  set -- ${R_CC}
  for word; do
    case "$word" in
      (-*) PKG_CC="${PKG_CC} $@"; break ;;
      (*) shift ;;
    esac
  done
  AC_MSG_RESULT([${PKG_CC}])

  # Check that nc-config compiler works:
  CC="${PKG_CC}"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [])], [],
    [CC=
     AC_MSG_FAILURE([nc-config compiler test failed])
    ])
])

# Check compiler from R, if no working compiler found so far:
AS_IF([test -z "$CC"], [
  # Check that R compiler works:
  CC="${R_CC}"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [])], [],
    [AC_MSG_ERROR([R compiler test failed])])
])

AC_MSG_CHECKING([CC])
AC_MSG_RESULT([$CC])


#-------------------------------------------------------------------------------#
#  Concatenate compiler/linker variables from R then environment                #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Concatenate compiler/linker variables from R then environment:])

AC_MSG_CHECKING([CFLAGS])
CFLAGS="${R_CFLAGS} $CFLAGS"
AC_MSG_RESULT([$CFLAGS])

AC_MSG_CHECKING([CPPFLAGS])
CPPFLAGS="${R_CPPFLAGS} $CPPFLAGS"
AC_MSG_RESULT([$CPPFLAGS])

AC_MSG_CHECKING([LDFLAGS])
LDFLAGS="${R_LDFLAGS} $LDFLAGS"
AC_MSG_RESULT([$LDFLAGS])


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
#  Prepend compiler/linker variables from nc-config (if specified)              #
#-------------------------------------------------------------------------------#

AC_ARG_WITH([nc-config-static],
  AS_HELP_STRING([--with-nc-config-static],
    [use static libraries from nc-config (default on Windows & macOS)]),
  [],
  [with_nc_config_static=unset])

AS_IF([test "x${with_nc_config_static}" = xunset],
  [AS_IF([test "x$platform" = xWindows || test "x$platform" = xmacOS],
     [with_nc_config_static=no],
     [with_nc_config_static=yes])
  ]
)

AS_IF([test "x${have_nc_config}" = xyes],
  [
    AC_MSG_NOTICE([Find compiler/linker variables from nc-config:])

    AC_MSG_CHECKING([PKG_CFLAGS])
    PKG_CFLAGS=`nc-config --cflags`
    AC_MSG_RESULT([${PKG_CFLAGS}])

    AS_IF([test "x${with_nc_config_static}" = xyes],
      [nc_config_static_flag="--static"
       nc_config_static_label=" (static)"],
      [nc_config_static_flag=""
       nc_config_static_label=""]
    )
    AC_MSG_CHECKING([LIBS${nc_config_static_label}])
    PKG_LIBS=`nc-config ${nc_config_static_flag} --libs`
    AC_MSG_RESULT([${PKG_LIBS}])

    AC_MSG_NOTICE([Prepend compiler/linker variables with variables from nc-config:])

    AC_MSG_CHECKING([CFLAGS])
    CFLAGS="${PKG_CFLAGS} $CFLAGS"
    AC_MSG_RESULT([$CFLAGS])

    AC_MSG_CHECKING([LIBS])
    LIBS="${PKG_LIBS} $LIBS"
    AC_MSG_RESULT([$LIBS])
   
  ]
)


#-------------------------------------------------------------------------------#
#  Find extra libraries that may be needed on Windows                           #
#-------------------------------------------------------------------------------#
# (nc-config seems not to capture some of these static libraries on Windows)

AS_IF([test "x$platform" = xWindows], [

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

  # Append to previous LIBS:
  AC_MSG_CHECKING([append extra libraries to previous LIBS])
  LIBS="$PRELIBS $LIBS"
  AC_MSG_RESULT([$LIBS])

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
  [AC_MSG_WARN([MPI not available. If needed, check that nc-config returns compiler with MPI support.])])

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

AC_MSG_NOTICE([Final compiler and linker variables for make:
CC=$CC
CFLAGS=$CFLAGS
CPPFLAGS=$CPPFLAGS
LDFLAGS=$LDFLAGS
LIBS=$LIBS])

# The above are substituted automatically in output files.
# DEFS is also substituted, but it cannot be displayed in configure.


#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_CONFIG_FILES([src/Makefile.common R/config.R])
AC_OUTPUT


#-------------------------------------------------------------------------------#
#  Done                                                                         #
#-------------------------------------------------------------------------------#

