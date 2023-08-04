#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF],[2.7-1])

#-------------------------------------------------------------------------------#
#  Find the compiler and compiler options                                       #
#-------------------------------------------------------------------------------#

: ${R_HOME=`R RHOME`}
AS_IF([test -z "${R_HOME}"],
  AC_MSG_ERROR([could not determine R_HOME])
)

# Get compilers and flags used when building R:
R_CC=`"${R_HOME}/bin/R" CMD config CC`
R_CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
R_CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
R_LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`

# Use compiler and flags from command-line, then environment,
# defaulting to those from R:
AC_ARG_WITH([mpicc],
    AS_HELP_STRING([--with-mpicc],
                   [C compiler command for MPI support]),
    [CC="$withval"])

AS_IF([test -z "$CC"], [CC="${R_CC}"])
AS_IF([test -z "$CFLAGS"], [CFLAGS="${R_CFLAGS}"])

# Prepend preprocessor and linker flags from environment to those from R:
CPPFLAGS="$CPPFLAGS ${R_CPPFLAGS}"
LDFLAGS="$LDFLAGS ${R_LDFLAGS}"


#-------------------------------------------------------------------------------#
#  Find sizes of selected data types                                            #
#-------------------------------------------------------------------------------#

AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([size_t])

#-------------------------------------------------------------------------------#
#  Find possible dependencies of NetCDF on Windows,                             #
#  or try nc-config on other platforms                                          #
#-------------------------------------------------------------------------------#

AC_CHECK_DECLS([_WIN32], [

  AC_CHECK_LIB(m, atan)

  AC_CHECK_LIB(ws2_32, getnameinfo)
  AC_CHECK_LIB(wsock32, sethostname)
  AC_CHECK_LIB(wldap32, ldap_init)
  AC_CHECK_LIB(crypt32, CryptDecodeMessage)
  AC_CHECK_LIB(gdi32, GdiConvertBitmap)
  AC_CHECK_LIB(advapi32, RegOpenKeyExA)

  AC_CHECK_LIB(pthread, pthread_mutex_init)

  AC_CHECK_LIB(lzma, lzma_lzma_decoder_init)
  AC_CHECK_LIB(z, deflate)
  AC_CHECK_LIB(zstd, ZSTD_decompress)
  AC_CHECK_LIB(sz, SZ_BufftoBuffDecompress)

  AC_CHECK_LIB(bcrypt, BCryptDecrypt)
  AC_CHECK_LIB(gpg-error, gpg_strerror)
  AC_CHECK_LIB(gcrypt, gcry_md_open)
  AC_CHECK_LIB(crypto, EVP_CIPHER_CTX_new)
  AC_CHECK_LIB(ssl, SSL_CTX_new)
  AC_CHECK_LIB(ssh2, libssh2_init)

  AC_CHECK_LIB(iconv, libiconv_open)
  AC_CHECK_LIB(charset, locale_charset)
  AC_CHECK_LIB(unistring, u8_to_u16)
  AC_CHECK_LIB(idn2, idn2_to_ascii_8z)

  AC_CHECK_LIB(hdf5, H5Fopen)
  AC_CHECK_LIB(hdf5_hl, H5TBmake_table)

  AC_CHECK_LIB(portablexdr, xdr_int)

  AC_CHECK_LIB(xml2, xmlReadMemory)

  AC_CHECK_LIB(curl, curl_easy_init)

  AC_CHECK_LIB(jpeg, jpeg_start_compress)

  AC_CHECK_LIB(df, Hopen)
  AC_CHECK_LIB(mfhdf, SDstart)

], [

  AC_ARG_WITH([nc-config],
    AS_HELP_STRING([--without-nc-config],
      [do not use nc-config to get netcdf configuration]),
    [],
    [with_nc_config=yes])

  AS_IF([test "x$with_nc_config" != xno],
    [AC_CHECK_PROG(have_nc_config, nc-config, yes, no, [], [])])

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
    ])

])

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
AC_CHECK_HEADERS(netcdf_filter.h, [], [], [#include <netcdf.h>])

# Check for optional features that depend only on preprocessor declarations:
AC_CHECK_DECLS([NC_64BIT_DATA, NC_FORMAT_64BIT_DATA, NC_DISKLESS, NC_PERSIST],
  [], [], [#include <netcdf.h>])


#-------------------------------------------------------------------------------#
#  Check for parallel netcdf features                                           #
#-------------------------------------------------------------------------------#

has_parallel=TRUE
AC_CHECK_FUNCS([nc_create_par_fortran nc_open_par_fortran nc_var_par_access],
               [],
               [has_parallel=FALSE])
AC_CHECK_HEADERS([netcdf_par.h],
  [],
  [has_parallel=FALSE],
  [#include <netcdf.h>])
AS_IF([test "$has_parallel" != "TRUE"],
  [AC_MSG_WARN([disabling parallel I/O functions in RNetCDF])]
)

has_info_null=FALSE
AC_CHECK_HEADERS([mpi.h],
  [AC_CHECK_DECLS([MPI_INFO_NULL],
    [AC_CHECK_FUNCS([MPI_Info_c2f], [has_info_null=TRUE], [], [#include <mpi.h>])],
    [],
    [#include <mpi.h>])
  ])
AS_IF([test "$has_info_null" != "TRUE"],
  [AC_MSG_WARN([disabling support for MPI_INFO_NULL in RNetCDF])]
)


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


#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_CONFIG_FILES([src/Makevars R/config.R])
AC_OUTPUT

#-------------------------------------------------------------------------------#
#  Done                                                                         #
#-------------------------------------------------------------------------------#

