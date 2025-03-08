#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([RNetCDF],[2.9-2])

: ${R_HOME=`R RHOME`}
AS_IF([test -z "${R_HOME}"],
  AC_MSG_ERROR([could not determine R_HOME])
)

# TODO: create config.h instead of passing defs via command line

#-------------------------------------------------------------------------------#
#  Get compiler/linker variables from environment                               #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Find compiler/linker variables from environment:])

AC_MSG_CHECKING([CFLAGS])
ENV_CFLAGS="$CFLAGS"
AC_MSG_RESULT([${ENV_CFLAGS}])

AC_MSG_CHECKING([CPPFLAGS])
ENV_CPPFLAGS="$CPPFLAGS"
AC_MSG_RESULT([${ENV_CPPFLAGS}])

AC_MSG_CHECKING([LDFLAGS])
ENV_LDFLAGS="$LDFLAGS"
AC_MSG_RESULT([${ENV_LDFLAGS}])

AC_MSG_CHECKING([LIBS])
ENV_LIBS="$LIBS"
AC_MSG_RESULT([${ENV_LIBS}])


#-------------------------------------------------------------------------------#
#  Select compiler from R unless --with-cc has non-empty value                  #
#-------------------------------------------------------------------------------#

AC_MSG_NOTICE([Select C compiler:])

AC_MSG_CHECKING([C compiler from --with-cc])

AC_ARG_WITH([cc],
  AS_HELP_STRING([--with-cc],
    [command for C compiler, otherwise C compiler from R is used]),
  [ARG_CC="$withval"],
  [ARG_CC=""])
AC_MSG_RESULT([${ARG_CC}])

AS_IF([test -n "${ARG_CC}"],
  [CC="${ARG_CC}"],
  [AC_MSG_CHECKING([C compiler from R])
   R_CC=`"${R_HOME}/bin/R" CMD config CC`
   CC="${R_CC}"
   AC_MSG_RESULT([$CC])]
)


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

# No LIBS needed from R

#-------------------------------------------------------------------------------#
#  Initialise variables for tests and results                                   #
#-------------------------------------------------------------------------------#

# Initial compiler test with CC found earlier and compiler flags from R.
# Original variable contents were copied to ENV_ variables earlier.
CFLAGS="${R_CFLAGS}"
CPPFLAGS="${R_CPPFLAGS}"
LDFLAGS="${R_LDFLAGS}"
LIBS=""

# Results of package configuration (excluding settings inherited from R):
PKG_CFLAGS=""
PKG_CPPFLAGS=""
PKG_LDFLAGS=""

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
#  Prepend compiler/linker variables from nc-config (if enabled)                #
#-------------------------------------------------------------------------------#

# Enable nc-config by default:
AC_ARG_WITH([nc-config],
  AS_HELP_STRING([--with-nc-config],
    [try compiler options from nc-config (default)]),
  [],
  [with_nc_config=yes])

# Check that nc-config program exists:
AS_IF([test "x${with_nc_config}" = xyes],
  [AC_CHECK_PROG(have_nc_config, nc-config, yes, no, [], [])],
  [have_nc_config=no])

# Get cflags and libs from nc-config and check if they work:
AS_IF([test "x${have_nc_config}" = xyes],
  [
    AC_MSG_NOTICE([Check compiler/linker variables from nc-config:])

    SAVE_CC="$CC"
    AS_IF([test -z "${ARG_CC}"],
      [
        AC_MSG_CHECKING([nc-config --cc])
        NC_CC=`nc-config --cc`
        AC_MSG_RESULT([${NC_CC}])
        
        CC="${NC_CC}"
        AC_MSG_NOTICE([Specify --with-cc if a different compiler is needed])
      ]
    )

    AC_MSG_CHECKING([nc-config --cflags])
    NC_CFLAGS=`nc-config --cflags`
    AC_MSG_RESULT([${NC_CFLAGS}])

    AC_MSG_CHECKING([nc-config --libs])
    NC_LIBS=`nc-config --libs`
    AC_MSG_RESULT([${NC_LIBS}])

    # Check that settings from nc-config work.
    nc_config_links=no
    AC_LANG([C])
    AC_MSG_CHECKING([build with nc-config])
    CFLAGS="${NC_CFLAGS} ${R_CFLAGS}"
    LIBS="${NC_LIBS}"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <netcdf.h>], [return nc_close (0);])],
      [nc_config_links=yes])
    AC_MSG_RESULT([${nc_config_links}])

    # If settings did not work, try static libs:
    AS_IF([test "x${nc_config_links}" != xyes],
      [
	AC_MSG_CHECKING([nc-config accepts --static])
	AS_IF([nc-config --static --libs >/dev/null 2>&1],
	  [
            AC_MSG_RESULT([yes])

            AC_MSG_CHECKING([nc-config --static --libs])
	    NC_LIBS=`nc-config --static --libs`
	    AC_MSG_RESULT([${NC_LIBS}])

	    AC_MSG_CHECKING([build with static libs from nc-config])
	    LIBS="${NC_LIBS}"
	    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <netcdf.h>], [return nc_close (0);])],
	      [nc_config_links=yes])
            AC_MSG_RESULT([${nc_config_links}])
	  ],
	  [AC_MSG_RESULT([no])]
	)
      ]
    )

    # Finalise nc-config checks:
    AS_IF([test "x${nc_config_links}" == xyes],
      [
        AC_MSG_NOTICE([Proceeding with settings from nc-config])
        # Keep CFLAGS and LIBS as-is, copy nc-config settings to results:
        PKG_CFLAGS="${NC_CFLAGS}"
      ], [
        AC_MSG_WARN([Ignoring broken nc-config])
        # Show later code that nc-config settings were not used:
        have_nc_config=no
        # Restore CC, CFLAGS and LIBS for later tests:
        CC="${SAVE_CC}"
        CFLAGS="${R_CFLAGS}"
        LIBS=""
      ]
    )

  ], [
    AC_MSG_NOTICE([Skipping nc-config])
  ]
)


#-------------------------------------------------------------------------------#
# If nc-config was skipped or broken, try settings from pkg-config (if enabled) #
#-------------------------------------------------------------------------------#

# Enable pkg-config by default:
AC_ARG_WITH([pkg-config],
  AS_HELP_STRING([--with-pkg-config],
    [try compiler options from pkg-config (default unless nc-config succeeds)]),
  [],
  [with_pkg_config=yes])

AS_IF([test "x${have_nc_config}" == xyes],
  [with_pkg_config=no]
)

# Check that pkg-config has a netcdf module:

have_pkg_config=no
AS_IF([test "x${with_pkg_config}" == xyes],
  [
    AC_MSG_CHECKING([pkg-config has netcdf module])
    AS_IF([pkg-config --exists netcdf],
      [have_pkg_config=yes],
      [have_pkg_config=no])
    AC_MSG_RESULT([${have_pkg_config}])
  ])

# Get cflags and libs from pkg-config and check if they work:
AS_IF([test "x${have_pkg_config}" = xyes],
  [
    AC_MSG_NOTICE([Check compiler/linker variables from pkg-config:])

    AC_MSG_CHECKING([pkg-config --cflags netcdf])
    PC_CFLAGS=`pkg-config --cflags netcdf`
    AC_MSG_RESULT([${PC_CFLAGS}])

    AC_MSG_CHECKING([pkg-config --libs netcdf])
    PC_LIBS=`pkg-config --libs netcdf`
    AC_MSG_RESULT([${PC_LIBS}])

    # Check that settings from pkg-config work.
    pkg_config_links=no
    AC_LANG([C])
    AC_MSG_CHECKING([build with pkg-config])
    CFLAGS="${PC_CFLAGS} ${R_CFLAGS}"
    LIBS="${PC_LIBS}"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <netcdf.h>], [return nc_close (0);])],
      [pkg_config_links=yes])
    AC_MSG_RESULT([${pkg_config_links}])

    # If settings did not work, try static libs:
    AS_IF([test "x${pkg_config_links}" != xyes],
      [
	AC_MSG_CHECKING([pkg-config accepts --static])
	AS_IF([pkg-config --static --libs netcdf >/dev/null 2>&1],
	  [
            AC_MSG_RESULT([yes])

            AC_MSG_CHECKING([pkg-config --static --libs netcdf])
	    PC_LIBS=`pkg-config --static --libs netcdf`
	    AC_MSG_RESULT([${PC_LIBS}])

	    AC_MSG_CHECKING([build with static libs from pkg-config])
	    LIBS="${PC_LIBS}"
	    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <netcdf.h>], [return nc_close (0);])],
	      [pkg_config_links=yes])
            AC_MSG_RESULT([${pkg_config_links}])
	  ],
	  [AC_MSG_RESULT([no])]
	)
      ]
    )

    # Finalise pkg-config checks:
    AS_IF([test "x${pkg_config_links}" == xyes],
      [
        AC_MSG_NOTICE([Proceeding with settings from pkg-config])
        # Keep CFLAGS and LIBS as-is, copy pkg-config settings to results:
        PKG_CFLAGS="${PC_CFLAGS}"
      ], [
        AC_MSG_WARN([Ignoring broken pkg-config])
        have_pkg_config=no
        # Restore CFLAGS and LIBS for later tests:
        CFLAGS="${R_CFLAGS}"
        LIBS=""
      ]
    )

  ], [
    AC_MSG_NOTICE([Skipping pkg-config])
  ]
)


#-------------------------------------------------------------------------------#
#  If nc-config and pkg-config did not succeed,                                 #
#  search for library dependencies with settings from environment               #
#-------------------------------------------------------------------------------#

with_env_search=no
AS_IF([test "x${have_nc_config}" != xyes],
  [AS_IF([test "x${have_pkg_config}" != xyes], [with_env_search=yes])]
)

AS_IF([test "x${with_env_search}" == xyes], [

  AC_MSG_NOTICE([Find typical library dependencies for linking netcdf:])

  # Only use env variables if nc-config and/or pkg-config fails:
  # CC is unchanged
  CFLAGS="${ENV_CFLAGS} ${R_CFLAGS}"
  CPPFLAGS="${ENV_CPPFLAGS} ${R_CPPFLAGS}"
  LDFLAGS="${ENV_LDFLAGS} ${R_LDFLAGS}"
  LIBS="${ENV_LIBS}"

  PKG_CFLAGS="${ENV_CFLAGS}"
  PKG_CPPFLAGS="${ENV_CPPFLAGS}"
  PKG_LDFLAGS="${ENV_LDFLAGS}"

  # Windows only:
  AS_IF([test "x$platform" = xWindows], 
    [LIBS="-ladvapi32 -lgdi32 -lcrypt32 -lwldap32 -lwsock32 -lws2_32 $LIBS"]
  )

  # Possible dependencies on all platforms:
  AC_SEARCH_LIBS(atan, m)

  AC_SEARCH_LIBS(pthread_mutex_init, pthread)

  AC_SEARCH_LIBS(lzma_lzma_decoder_init, lzma)
  AC_SEARCH_LIBS(deflate, z)
  AC_SEARCH_LIBS(ZSTD_decompress, zstd)
  AC_SEARCH_LIBS(SZ_BufftoBuffDecompress, sz)
  AC_SEARCH_LIBS(BZ2_bzDecompress, bz2)
  AC_SEARCH_LIBS(LZ4_compress_default, lz4)
  AC_SEARCH_LIBS(blosc_init, blosc)

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
  [AC_MSG_WARN([MPI not available. Specify --with-cc if needed.])])

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

