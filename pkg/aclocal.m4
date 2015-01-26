# generated automatically by aclocal 1.15 -*- Autoconf -*-

# Copyright (C) 1996-2014 Free Software Foundation, Inc.

# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

m4_ifndef([AC_CONFIG_MACRO_DIRS], [m4_defun([_AM_CONFIG_MACRO_DIRS], [])m4_defun([AC_CONFIG_MACRO_DIRS], [_AM_CONFIG_MACRO_DIRS($@)])])
# ===========================================================================
#       http://www.gnu.org/software/autoconf-archive/ax_check_zlib.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_CHECK_ZLIB([action-if-found], [action-if-not-found])
#
# DESCRIPTION
#
#   This macro searches for an installed zlib library. If nothing was
#   specified when calling configure, it searches first in /usr/local and
#   then in /usr, /opt/local and /sw. If the --with-zlib=DIR is specified,
#   it will try to find it in DIR/include/zlib.h and DIR/lib/libz.a. If
#   --without-zlib is specified, the library is not searched at all.
#
#   If either the header file (zlib.h) or the library (libz) is not found,
#   shell commands 'action-if-not-found' is run. If 'action-if-not-found' is
#   not specified, the configuration exits on error, asking for a valid zlib
#   installation directory or --without-zlib.
#
#   If both header file and library are found, shell commands
#   'action-if-found' is run. If 'action-if-found' is not specified, the
#   default action appends '-I${ZLIB_HOME}/include' to CPFLAGS, appends
#   '-L$ZLIB_HOME}/lib' to LDFLAGS, prepends '-lz' to LIBS, and calls
#   AC_DEFINE(HAVE_LIBZ). You should use autoheader to include a definition
#   for this symbol in a config.h file. Sample usage in a C/C++ source is as
#   follows:
#
#     #ifdef HAVE_LIBZ
#     #include <zlib.h>
#     #endif /* HAVE_LIBZ */
#
# LICENSE
#
#   Copyright (c) 2008 Loic Dachary <loic@senga.org>
#   Copyright (c) 2010 Bastien Chevreux <bach@chevreux.org>
#
#   This program is free software; you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation; either version 2 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.

#serial 14

AU_ALIAS([CHECK_ZLIB], [AX_CHECK_ZLIB])
AC_DEFUN([AX_CHECK_ZLIB],
#
# Handle user hints
#
[AC_MSG_CHECKING(if zlib is wanted)
zlib_places="/usr/local /usr /opt/local /sw"
AC_ARG_WITH([zlib],
[  --with-zlib=DIR         root directory path of zlib installation @<:@defaults to
                          /usr/local or /usr if not found in /usr/local@:>@
  --without-zlib          to disable zlib usage completely],
[if test "$withval" != no ; then
  AC_MSG_RESULT(yes)
  if test -d "$withval"
  then
    zlib_places="$withval $zlib_places"
  else
    AC_MSG_WARN([Sorry, $withval does not exist, checking usual places])
  fi
else
  zlib_places=
  AC_MSG_RESULT(no)
fi],
[AC_MSG_RESULT(yes)])

#
# Locate zlib, if wanted
#
if test -n "${zlib_places}"
then
	# check the user supplied or any other more or less 'standard' place:
	#   Most UNIX systems      : /usr/local and /usr
	#   MacPorts / Fink on OSX : /opt/local respectively /sw
	for ZLIB_HOME in ${zlib_places} ; do
	  if test -f "${ZLIB_HOME}/include/zlib.h"; then break; fi
	  ZLIB_HOME=""
	done

  ZLIB_OLD_LDFLAGS=$LDFLAGS
  ZLIB_OLD_CPPFLAGS=$CPPFLAGS
  if test -n "${ZLIB_HOME}"; then
        LDFLAGS="$LDFLAGS -L${ZLIB_HOME}/lib"
        CPPFLAGS="$CPPFLAGS -I${ZLIB_HOME}/include"
  fi
  AC_LANG_SAVE
  AC_LANG_C
  AC_CHECK_LIB([z], [inflateEnd], [zlib_cv_libz=yes], [zlib_cv_libz=no])
  AC_CHECK_HEADER([zlib.h], [zlib_cv_zlib_h=yes], [zlib_cv_zlib_h=no])
  AC_LANG_RESTORE
  if test "$zlib_cv_libz" = "yes" && test "$zlib_cv_zlib_h" = "yes"
  then
    #
    # If both library and header were found, action-if-found
    #
    m4_ifblank([$1],[
                CPPFLAGS="$CPPFLAGS -I${ZLIB_HOME}/include"
                LDFLAGS="$LDFLAGS -L${ZLIB_HOME}/lib"
                LIBS="-lz $LIBS"
                AC_DEFINE([HAVE_LIBZ], [1],
                          [Define to 1 if you have `z' library (-lz)])
               ],[
                # Restore variables
                LDFLAGS="$ZLIB_OLD_LDFLAGS"
                CPPFLAGS="$ZLIB_OLD_CPPFLAGS"
                $1
               ])
  else
    #
    # If either header or library was not found, action-if-not-found
    #
    m4_default([$2],[
                AC_MSG_ERROR([either specify a valid zlib installation with --with-zlib=DIR or disable zlib usage with --without-zlib])
                ])
  fi
fi
])

# ===========================================================================
#    http://www.gnu.org/software/autoconf-archive/ax_compare_version.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_COMPARE_VERSION(VERSION_A, OP, VERSION_B, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   This macro compares two version strings. Due to the various number of
#   minor-version numbers that can exist, and the fact that string
#   comparisons are not compatible with numeric comparisons, this is not
#   necessarily trivial to do in a autoconf script. This macro makes doing
#   these comparisons easy.
#
#   The six basic comparisons are available, as well as checking equality
#   limited to a certain number of minor-version levels.
#
#   The operator OP determines what type of comparison to do, and can be one
#   of:
#
#    eq  - equal (test A == B)
#    ne  - not equal (test A != B)
#    le  - less than or equal (test A <= B)
#    ge  - greater than or equal (test A >= B)
#    lt  - less than (test A < B)
#    gt  - greater than (test A > B)
#
#   Additionally, the eq and ne operator can have a number after it to limit
#   the test to that number of minor versions.
#
#    eq0 - equal up to the length of the shorter version
#    ne0 - not equal up to the length of the shorter version
#    eqN - equal up to N sub-version levels
#    neN - not equal up to N sub-version levels
#
#   When the condition is true, shell commands ACTION-IF-TRUE are run,
#   otherwise shell commands ACTION-IF-FALSE are run. The environment
#   variable 'ax_compare_version' is always set to either 'true' or 'false'
#   as well.
#
#   Examples:
#
#     AX_COMPARE_VERSION([3.15.7],[lt],[3.15.8])
#     AX_COMPARE_VERSION([3.15],[lt],[3.15.8])
#
#   would both be true.
#
#     AX_COMPARE_VERSION([3.15.7],[eq],[3.15.8])
#     AX_COMPARE_VERSION([3.15],[gt],[3.15.8])
#
#   would both be false.
#
#     AX_COMPARE_VERSION([3.15.7],[eq2],[3.15.8])
#
#   would be true because it is only comparing two minor versions.
#
#     AX_COMPARE_VERSION([3.15.7],[eq0],[3.15])
#
#   would be true because it is only comparing the lesser number of minor
#   versions of the two values.
#
#   Note: The characters that separate the version numbers do not matter. An
#   empty string is the same as version 0. OP is evaluated by autoconf, not
#   configure, so must be a string, not a variable.
#
#   The author would like to acknowledge Guido Draheim whose advice about
#   the m4_case and m4_ifvaln functions make this macro only include the
#   portions necessary to perform the specific comparison specified by the
#   OP argument in the final configure script.
#
# LICENSE
#
#   Copyright (c) 2008 Tim Toolan <toolan@ele.uri.edu>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 11

dnl #########################################################################
AC_DEFUN([AX_COMPARE_VERSION], [
  AC_REQUIRE([AC_PROG_AWK])

  # Used to indicate true or false condition
  ax_compare_version=false

  # Convert the two version strings to be compared into a format that
  # allows a simple string comparison.  The end result is that a version
  # string of the form 1.12.5-r617 will be converted to the form
  # 0001001200050617.  In other words, each number is zero padded to four
  # digits, and non digits are removed.
  AS_VAR_PUSHDEF([A],[ax_compare_version_A])
  A=`echo "$1" | sed -e 's/\([[0-9]]*\)/Z\1Z/g' \
                     -e 's/Z\([[0-9]]\)Z/Z0\1Z/g' \
                     -e 's/Z\([[0-9]][[0-9]]\)Z/Z0\1Z/g' \
                     -e 's/Z\([[0-9]][[0-9]][[0-9]]\)Z/Z0\1Z/g' \
                     -e 's/[[^0-9]]//g'`

  AS_VAR_PUSHDEF([B],[ax_compare_version_B])
  B=`echo "$3" | sed -e 's/\([[0-9]]*\)/Z\1Z/g' \
                     -e 's/Z\([[0-9]]\)Z/Z0\1Z/g' \
                     -e 's/Z\([[0-9]][[0-9]]\)Z/Z0\1Z/g' \
                     -e 's/Z\([[0-9]][[0-9]][[0-9]]\)Z/Z0\1Z/g' \
                     -e 's/[[^0-9]]//g'`

  dnl # In the case of le, ge, lt, and gt, the strings are sorted as necessary
  dnl # then the first line is used to determine if the condition is true.
  dnl # The sed right after the echo is to remove any indented white space.
  m4_case(m4_tolower($2),
  [lt],[
    ax_compare_version=`echo "x$A
x$B" | sed 's/^ *//' | sort -r | sed "s/x${A}/false/;s/x${B}/true/;1q"`
  ],
  [gt],[
    ax_compare_version=`echo "x$A
x$B" | sed 's/^ *//' | sort | sed "s/x${A}/false/;s/x${B}/true/;1q"`
  ],
  [le],[
    ax_compare_version=`echo "x$A
x$B" | sed 's/^ *//' | sort | sed "s/x${A}/true/;s/x${B}/false/;1q"`
  ],
  [ge],[
    ax_compare_version=`echo "x$A
x$B" | sed 's/^ *//' | sort -r | sed "s/x${A}/true/;s/x${B}/false/;1q"`
  ],[
    dnl Split the operator from the subversion count if present.
    m4_bmatch(m4_substr($2,2),
    [0],[
      # A count of zero means use the length of the shorter version.
      # Determine the number of characters in A and B.
      ax_compare_version_len_A=`echo "$A" | $AWK '{print(length)}'`
      ax_compare_version_len_B=`echo "$B" | $AWK '{print(length)}'`

      # Set A to no more than B's length and B to no more than A's length.
      A=`echo "$A" | sed "s/\(.\{$ax_compare_version_len_B\}\).*/\1/"`
      B=`echo "$B" | sed "s/\(.\{$ax_compare_version_len_A\}\).*/\1/"`
    ],
    [[0-9]+],[
      # A count greater than zero means use only that many subversions
      A=`echo "$A" | sed "s/\(\([[0-9]]\{4\}\)\{m4_substr($2,2)\}\).*/\1/"`
      B=`echo "$B" | sed "s/\(\([[0-9]]\{4\}\)\{m4_substr($2,2)\}\).*/\1/"`
    ],
    [.+],[
      AC_WARNING(
        [illegal OP numeric parameter: $2])
    ],[])

    # Pad zeros at end of numbers to make same length.
    ax_compare_version_tmp_A="$A`echo $B | sed 's/./0/g'`"
    B="$B`echo $A | sed 's/./0/g'`"
    A="$ax_compare_version_tmp_A"

    # Check for equality or inequality as necessary.
    m4_case(m4_tolower(m4_substr($2,0,2)),
    [eq],[
      test "x$A" = "x$B" && ax_compare_version=true
    ],
    [ne],[
      test "x$A" != "x$B" && ax_compare_version=true
    ],[
      AC_WARNING([illegal OP parameter: $2])
    ])
  ])

  AS_VAR_POPDEF([A])dnl
  AS_VAR_POPDEF([B])dnl

  dnl # Execute ACTION-IF-TRUE / ACTION-IF-FALSE.
  if test "$ax_compare_version" = "true" ; then
    m4_ifvaln([$4],[$4],[:])dnl
    m4_ifvaln([$5],[else $5])dnl
  fi
]) dnl AX_COMPARE_VERSION

# ===========================================================================
#        http://www.gnu.org/software/autoconf-archive/ax_lib_curl.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_LIB_CURL([VERSION],[ACTION-IF-SUCCESS],[ACTION-IF-FAILURE])
#
# DESCRIPTION
#
#   Checks for minimum curl library version VERSION. If successful executes
#   ACTION-IF-SUCCESS otherwise ACTION-IF-FAILURE.
#
#   Defines CURL_LIBS and CURL_CFLAGS.
#
#   A simple example:
#
#     AX_LIB_CURL([7.19.4],,[
#       AC_MSG_ERROR([Your system lacks libcurl >= 7.19.4])
#     ])
#
#   This macro is a rearranged version of AC_LIB_CURL from Akos Maroy.
#
# LICENSE
#
#   Copyright (c) 2009 Francesco Salvestrini <salvestrini@users.sourceforge.net>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 8

AU_ALIAS([AC_CHECK_CURL], [AX_LIB_CURL])
AC_DEFUN([AX_LIB_CURL], [
  AX_PATH_GENERIC([curl],[$1],'s/^libcurl\ \+//',[$2],[$3])
])

# ===========================================================================
#       http://www.gnu.org/software/autoconf-archive/ax_lib_expat.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_LIB_EXPAT([MINIMUM-VERSION])
#
# DESCRIPTION
#
#   This macro provides tests of availability of Expat XML Parser of
#   particular version or newer. This macro checks for Expat XML Parser
#   headers and libraries and defines compilation flags
#
#   Macro supports following options and their values:
#
#   1) Single-option usage:
#
#     --with-expat      -- yes, no, or path to Expat XML Parser
#                          installation prefix
#
#   2) Three-options usage (all options are required):
#
#     --with-expat=yes
#     --with-expat-inc  -- path to base directory with Expat headers
#     --with-expat-lib  -- linker flags for Expat
#
#   This macro calls:
#
#     AC_SUBST(EXPAT_CFLAGS)
#     AC_SUBST(EXPAT_LIBS)
#     AC_SUBST(EXPAT_LDFLAGS)
#     AC_SUBST(EXPAT_VERSION)  -- only if version requirement is used
#
#   And sets:
#
#     HAVE_EXPAT
#
# LICENSE
#
#   Copyright (c) 2008 Mateusz Loskot <mateusz@loskot.net>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 10

AC_DEFUN([AX_LIB_EXPAT],
[
    AC_ARG_WITH([expat],
        AS_HELP_STRING([--with-expat=@<:@ARG@:>@],
            [use Expat XML Parser from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -f /usr/local/include/expat.h ; then
                expat_prefix=/usr/local
            elif test -f /usr/include/expat.h ; then
                expat_prefix=/usr
            else
                expat_prefix=""
            fi
            expat_requested="yes"
        elif test -d "$withval"; then
            expat_prefix="$withval"
            expat_requested="yes"
        else
            expat_prefix=""
            expat_requested="no"
        fi
        ],
        [
        dnl Default behavior is implicit yes
        if test -f /usr/local/include/expat.h ; then
            expat_prefix=/usr/local
        elif test -f /usr/include/expat.h ; then
            expat_prefix=/usr
        else
            expat_prefix=""
        fi
        ]
    )

    AC_ARG_WITH([expat-inc],
        AS_HELP_STRING([--with-expat-inc=@<:@DIR@:>@],
            [path to Expat XML Parser headers]
        ),
        [expat_include_dir="$withval"],
        [expat_include_dir=""]
    )
    AC_ARG_WITH([expat-lib],
        AS_HELP_STRING([--with-expat-lib=@<:@ARG@:>@],
            [link options for Expat XML Parser libraries]
        ),
        [expat_lib_flags="$withval"],
        [expat_lib_flags=""]
    )

    EXPAT_CFLAGS=""
    EXPAT_LIBS=""
    EXPAT_VERSION=""

    dnl
    dnl Collect include/lib paths and flags
    dnl
    run_expat_test="no"

    if test -n "$expat_prefix"; then
        expat_include_dir="$expat_prefix/include"
        expat_ld_flags="-L$expat_prefix/lib"
        expat_lib_flags="-lexpat"
        run_expat_test="yes"
    elif test "$expat_requested" = "yes"; then
        if test -n "$expat_include_dir" -a -n "$expat_lib_flags"; then
            run_expat_test="yes"
        fi
    else
        run_expat_test="no"
    fi

    dnl
    dnl Check Expat XML Parser files
    dnl
    if test "$run_expat_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$expat_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $expat_lib_flags"

        saved_LDFLAGS="$LDFLAGS"
        LDFLAGS="$LDFLAGS $expat_ld_flags"

        dnl
        dnl Check Expat headers
        dnl
        AC_MSG_CHECKING([for Expat XML Parser headers in $expat_include_dir])

        AC_LANG_PUSH([C++])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <expat.h>
                ]],
                [[]]
            )],
            [
            EXPAT_CFLAGS="-I$expat_include_dir"
            expat_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            expat_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C++])

        dnl
        dnl Check Expat libraries
        dnl
        if test "$expat_header_found" = "yes"; then

            AC_MSG_CHECKING([for Expat XML Parser libraries])

            AC_LANG_PUSH([C++])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <expat.h>
                    ]],
                    [[
XML_Parser p = XML_ParserCreate(NULL);
XML_ParserFree(p);
p = NULL;
                    ]]
                )],
                [
                EXPAT_LIBS="$expat_lib_flags"
                EXPAT_LDFLAGS="$expat_ld_flags"
                expat_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                expat_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C++])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LDFLAGS="$saved_LDFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for Expat XML Parser])

    if test "$run_expat_test" = "yes"; then
        if test "$expat_header_found" = "yes" -a "$expat_lib_found" = "yes"; then

            AC_SUBST([EXPAT_CFLAGS])
            AC_SUBST([EXPAT_LDFLAGS])
            AC_SUBST([EXPAT_LIBS])

            HAVE_EXPAT="yes"
        else
            HAVE_EXPAT="no"
        fi

        AC_MSG_RESULT([$HAVE_EXPAT])

        dnl
        dnl Check Expat version
        dnl
        if test "$HAVE_EXPAT" = "yes"; then

            expat_version_req=ifelse([$1], [], [], [$1])

            if test  -n "$expat_version_req"; then

                AC_MSG_CHECKING([if Expat XML Parser version is >= $expat_version_req])

                if test -f "$expat_include_dir/expat.h"; then

                    expat_major=`cat $expat_include_dir/expat.h | \
                                    grep '^#define.*XML_MAJOR_VERSION.*[0-9]$' | \
                                    sed -e 's/#define XML_MAJOR_VERSION.//'`

                    expat_minor=`cat $expat_include_dir/expat.h | \
                                    grep '^#define.*XML_MINOR_VERSION.*[0-9]$' | \
                                    sed -e 's/#define XML_MINOR_VERSION.//'`

                    expat_revision=`cat $expat_include_dir/expat.h | \
                                    grep '^#define.*XML_MICRO_VERSION.*[0-9]$' | \
                                    sed -e 's/#define XML_MICRO_VERSION.//'`

                    EXPAT_VERSION="$expat_major.$expat_minor.$expat_revision"
                    AC_SUBST([EXPAT_VERSION])

                    dnl Decompose required version string and calculate numerical representation
                    expat_version_req_major=`expr $expat_version_req : '\([[0-9]]*\)'`
                    expat_version_req_minor=`expr $expat_version_req : '[[0-9]]*\.\([[0-9]]*\)'`
                    expat_version_req_revision=`expr $expat_version_req : '[[0-9]]*\.[[0-9]]*\.\([[0-9]]*\)'`
                    if test "x$expat_version_req_revision" = "x"; then
                        expat_version_req_revision="0"
                    fi

                    expat_version_req_number=`expr $expat_version_req_major \* 10000 \
                                               \+ $expat_version_req_minor \* 100 \
                                               \+ $expat_version_req_revision`

                    dnl Calculate numerical representation of detected version
                    expat_version_number=`expr $expat_major \* 10000 \
                                          \+ $expat_minor \* 100 \
                                           \+ $expat_revision`

                    expat_version_check=`expr $expat_version_number \>\= $expat_version_req_number`
                    if test "$expat_version_check" = "1"; then
                        AC_MSG_RESULT([yes])
                    else
                        AC_MSG_RESULT([no])
                        AC_MSG_WARN([Found Expat XML Parser $EXPAT_VERSION, which is older than required. Possible compilation failure.])
                    fi
                else
                    AC_MSG_RESULT([no])
                    AC_MSG_WARN([Missing expat.h header. Unable to determine Expat version.])
                fi
            fi
        fi

    else
        HAVE_EXPAT="no"
        AC_MSG_RESULT([$HAVE_EXPAT])

        if test "$expat_requested" = "yes"; then
            AC_MSG_WARN([Expat XML Parser support requested but headers or library not found. Specify valid prefix of Expat using --with-expat=@<:@DIR@:>@ or provide include directory and linker flags using --with-expat-inc and --with-expat-lib])
        fi
    fi
])

# ===========================================================================
#        http://www.gnu.org/software/autoconf-archive/ax_lib_hdf5.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_LIB_HDF5([serial/parallel])
#
# DESCRIPTION
#
#   This macro provides tests of the availability of HDF5 library.
#
#   The optional macro argument should be either 'serial' or 'parallel'. The
#   former only looks for serial HDF5 installations via h5cc. The latter
#   only looks for parallel HDF5 installations via h5pcc. If the optional
#   argument is omitted, serial installations will be preferred over
#   parallel ones.
#
#   The macro adds a --with-hdf5 option accepting one of three values:
#
#     no   - do not check for the HDF5 library.
#     yes  - do check for HDF5 library in standard locations.
#     path - complete path to the HDF5 helper script h5cc or h5pcc.
#
#   If HDF5 is successfully found, this macro calls
#
#     AC_SUBST(HDF5_VERSION)
#     AC_SUBST(HDF5_CC)
#     AC_SUBST(HDF5_CFLAGS)
#     AC_SUBST(HDF5_CPPFLAGS)
#     AC_SUBST(HDF5_LDFLAGS)
#     AC_SUBST(HDF5_LIBS)
#     AC_SUBST(HDF5_FC)
#     AC_SUBST(HDF5_FFLAGS)
#     AC_SUBST(HDF5_FLIBS)
#     AC_DEFINE(HAVE_HDF5)
#
#   and sets with_hdf5="yes".  Additionally, the macro sets
#   with_hdf5_fortran="yes" if a matching Fortran wrapper script is found.
#   Note that Autconf's Fortran support is not used to perform this check.
#   H5CC and H5FC will contain the appropriate serial or parallel HDF5
#   wrapper script locations.
#
#   If HDF5 is disabled or not found, this macros sets with_hdf5="no" and
#   with_hdf5_fortran="no".
#
#   Your configuration script can test $with_hdf to take any further
#   actions. HDF5_{C,CPP,LD}FLAGS may be used when building with C or C++.
#   HDF5_F{FLAGS,LIBS} should be used when building Fortran applications.
#
#   To use the macro, one would code one of the following in "configure.ac"
#   before AC_OUTPUT:
#
#     1) dnl Check for HDF5 support
#        AX_LIB_HDF5()
#
#     2) dnl Check for serial HDF5 support
#        AX_LIB_HDF5([serial])
#
#     3) dnl Check for parallel HDF5 support
#        AX_LIB_HDF5([parallel])
#
#   One could test $with_hdf5 for the outcome or display it as follows
#
#     echo "HDF5 support:  $with_hdf5"
#
#   You could also for example, override the default CC in "configure.ac" to
#   enforce compilation with the compiler that HDF5 uses:
#
#     AX_LIB_HDF5([parallel])
#     if test "$with_hdf5" = "yes"; then
#             CC="$HDF5_CC"
#     else
#             AC_MSG_ERROR([Unable to find HDF5, we need parallel HDF5.])
#     fi
#
# LICENSE
#
#   Copyright (c) 2009 Timothy Brown <tbrown@freeshell.org>
#   Copyright (c) 2010 Rhys Ulerich <rhys.ulerich@gmail.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 10

AC_DEFUN([AX_LIB_HDF5], [

AC_REQUIRE([AC_PROG_SED])
AC_REQUIRE([AC_PROG_AWK])
AC_REQUIRE([AC_PROG_GREP])

dnl Check first argument is one of the recognized values.
dnl Fail eagerly if is incorrect as this simplifies case statements below.
if   test "m4_normalize(m4_default([$1],[]))" = ""        ; then
    : # Recognized value
elif test "m4_normalize(m4_default([$1],[]))" = "serial"  ; then
    : # Recognized value
elif test "m4_normalize(m4_default([$1],[]))" = "parallel"; then
    : # Recognized value
else
    AC_MSG_ERROR([
Unrecognized value for AX[]_LIB_HDF5 within configure.ac.
If supplied, argument 1 must be either 'serial' or 'parallel'.
])
fi

dnl Add a default --with-hdf5 configuration option.
AC_ARG_WITH([hdf5],
  AS_HELP_STRING(
    [--with-hdf5=[yes/no/PATH]],
    m4_case(m4_normalize([$1]),
            [serial],   [location of h5cc for serial HDF5 configuration],
            [parallel], [location of h5pcc for parallel HDF5 configuration],
            [location of h5cc or h5pcc for HDF5 configuration])
  ),
  [if test "$withval" = "no"; then
     with_hdf5="no"
   elif test "$withval" = "yes"; then
     with_hdf5="yes"
   else
     with_hdf5="yes"
     H5CC="$withval"
   fi],
   [with_hdf5="yes"]
)

dnl Set defaults to blank
HDF5_CC=""
HDF5_VERSION=""
HDF5_CFLAGS=""
HDF5_CPPFLAGS=""
HDF5_LDFLAGS=""
HDF5_LIBS=""
HDF5_FC=""
HDF5_FFLAGS=""
HDF5_FLIBS=""

dnl Try and find hdf5 compiler tools and options.
if test "$with_hdf5" = "yes"; then
    if test -z "$H5CC"; then
        dnl Check to see if H5CC is in the path.
        AC_PATH_PROGS(
            [H5CC],
            m4_case(m4_normalize([$1]),
                [serial],   [h5cc],
                [parallel], [h5pcc],
                [h5cc h5pcc]),
            [])
    else
        AC_MSG_CHECKING([Using provided HDF5 C wrapper])
        AC_MSG_RESULT([$H5CC])
    fi
    AC_MSG_CHECKING([for HDF5 libraries])
    if test ! -f "$H5CC" || test ! -x "$H5CC"; then
        AC_MSG_RESULT([no])
        AC_MSG_WARN(m4_case(m4_normalize([$1]),
            [serial],  [
Unable to locate serial HDF5 compilation helper script 'h5cc'.
Please specify --with-hdf5=<LOCATION> as the full path to h5cc.
HDF5 support is being disabled (equivalent to --with-hdf5=no).
],            [parallel],[
Unable to locate parallel HDF5 compilation helper script 'h5pcc'.
Please specify --with-hdf5=<LOCATION> as the full path to h5pcc.
HDF5 support is being disabled (equivalent to --with-hdf5=no).
],            [
Unable to locate HDF5 compilation helper scripts 'h5cc' or 'h5pcc'.
Please specify --with-hdf5=<LOCATION> as the full path to h5cc or h5pcc.
HDF5 support is being disabled (equivalent to --with-hdf5=no).
]))
        with_hdf5="no"
        with_hdf5_fortran="no"
    else
        dnl Get the h5cc output
        HDF5_SHOW=$(eval $H5CC -show)

        dnl Get the actual compiler used
        HDF5_CC=$(eval $H5CC -show | $AWK '{print $[]1}')
        if test "$HDF5_CC" = "ccache"; then
            HDF5_CC=$(eval $H5CC -show | $AWK '{print $[]2}')
        fi

        dnl h5cc provides both AM_ and non-AM_ options
        dnl depending on how it was compiled either one of
        dnl these are empty. Lets roll them both into one.

        dnl Look for "HDF5 Version: X.Y.Z"
        HDF5_VERSION=$(eval $H5CC -showconfig | $GREP 'HDF5 Version:' \
            | $AWK '{print $[]3}')

        dnl A ideal situation would be where everything we needed was
        dnl in the AM_* variables. However most systems are not like this
        dnl and seem to have the values in the non-AM variables.
        dnl
        dnl We try the following to find the flags:
        dnl (1) Look for "NAME:" tags
        dnl (2) Look for "H5_NAME:" tags
        dnl (3) Look for "AM_NAME:" tags
        dnl
        HDF5_tmp_flags=$(eval $H5CC -showconfig \
            | $GREP 'FLAGS\|Extra libraries:' \
            | $AWK -F: '{printf("%s "), $[]2}' )

        dnl Find the installation directory and append include/
        HDF5_tmp_inst=$(eval $H5CC -showconfig \
            | $GREP 'Installation point:' \
            | $AWK -F: '{print $[]2}' )

        dnl Add this to the CPPFLAGS
        HDF5_CPPFLAGS="-I${HDF5_tmp_inst}/include"

        dnl Now sort the flags out based upon their prefixes
        for arg in $HDF5_SHOW $HDF5_tmp_flags ; do
          case "$arg" in
            -I*) echo $HDF5_CPPFLAGS | $GREP -e "$arg" 2>&1 >/dev/null \
                  || HDF5_CPPFLAGS="$arg $HDF5_CPPFLAGS"
              ;;
            -L*) echo $HDF5_LDFLAGS | $GREP -e "$arg" 2>&1 >/dev/null \
                  || HDF5_LDFLAGS="$arg $HDF5_LDFLAGS"
              ;;
            -l*) echo $HDF5_LIBS | $GREP -e "$arg" 2>&1 >/dev/null \
                  || HDF5_LIBS="$arg $HDF5_LIBS"
              ;;
          esac
        done

        HDF5_LIBS="$HDF5_LIBS -lhdf5"
        AC_MSG_RESULT([yes (version $[HDF5_VERSION])])

        dnl See if we can compile
        ax_lib_hdf5_save_CC=$CC
        ax_lib_hdf5_save_CPPFLAGS=$CPPFLAGS
        ax_lib_hdf5_save_LIBS=$LIBS
        ax_lib_hdf5_save_LDFLAGS=$LDFLAGS
        CC=$HDF5_CC
        CPPFLAGS=$HDF5_CPPFLAGS
        LIBS=$HDF5_LIBS
        LDFLAGS=$HDF5_LDFLAGS
        AC_CHECK_HEADER([hdf5.h], [ac_cv_hadf5_h=yes], [ac_cv_hadf5_h=no])
        AC_CHECK_LIB([hdf5], [H5Fcreate], [ac_cv_libhdf5=yes],
                     [ac_cv_libhdf5=no])
        if test "$ac_cv_hadf5_h" = "no" && test "$ac_cv_libhdf5" = "no" ; then
          AC_MSG_WARN([Unable to compile HDF5 test program])
        fi
        dnl Look for HDF5's high level library
        AC_HAVE_LIBRARY([hdf5_hl], [HDF5_LIBS="$HDF5_LIBS -lhdf5_hl"], [], [])

        CC=$ax_lib_hdf5_save_CC
        CPPFLAGS=$ax_lib_hdf5_save_CPPFLAGS
        LIBS=$ax_lib_hdf5_save_LIBS
        LDFLAGS=$ax_lib_hdf5_save_LDFLAGS

        AC_MSG_CHECKING([for matching HDF5 Fortran wrapper])
        dnl Presume HDF5 Fortran wrapper is just a name variant from H5CC
        H5FC=$(eval echo -n $H5CC | $SED -n 's/cc$/fc/p')
        if test -x "$H5FC"; then
            AC_MSG_RESULT([$H5FC])
            with_hdf5_fortran="yes"
            AC_SUBST([H5FC])

            dnl Again, pry any remaining -Idir/-Ldir from compiler wrapper
            for arg in `$H5FC -show`
            do
              case "$arg" in #(
                -I*) echo $HDF5_FFLAGS | $GREP -e "$arg" >/dev/null \
                      || HDF5_FFLAGS="$arg $HDF5_FFLAGS"
                  ;;#(
                -L*) echo $HDF5_FFLAGS | $GREP -e "$arg" >/dev/null \
                      || HDF5_FFLAGS="$arg $HDF5_FFLAGS"
                     dnl HDF5 installs .mod files in with libraries,
                     dnl but some compilers need to find them with -I
                     echo $HDF5_FFLAGS | $GREP -e "-I${arg#-L}" >/dev/null \
                      || HDF5_FFLAGS="-I${arg#-L} $HDF5_FFLAGS"
                  ;;
              esac
            done

            dnl Make Fortran link line by inserting Fortran libraries
            for arg in $HDF5_LIBS
            do
              case "$arg" in #(
                -lhdf5_hl) HDF5_FLIBS="$HDF5_FLIBS -lhdf5hl_fortran $arg"
                  ;; #(
                -lhdf5)    HDF5_FLIBS="$HDF5_FLIBS -lhdf5_fortran $arg"
                  ;; #(
                *) HDF5_FLIBS="$HDF5_FLIBS $arg"
                  ;;
              esac
            done
        else
            AC_MSG_RESULT([no])
            with_hdf5_fortran="no"
        fi

	AC_SUBST([HDF5_VERSION])
	AC_SUBST([HDF5_CC])
	AC_SUBST([HDF5_CFLAGS])
	AC_SUBST([HDF5_CPPFLAGS])
	AC_SUBST([HDF5_LDFLAGS])
	AC_SUBST([HDF5_LIBS])
	AC_SUBST([HDF5_FC])
	AC_SUBST([HDF5_FFLAGS])
	AC_SUBST([HDF5_FLIBS])
	AC_DEFINE([HAVE_HDF5], [1], [Defined if you have HDF5 support])
    fi
fi
])

# ===========================================================================
#      http://www.gnu.org/software/autoconf-archive/ax_path_generic.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_PATH_GENERIC(LIBRARY,[MINIMUM-VERSION,[SED-EXPR-EXTRACTOR]],[ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND],[CONFIG-SCRIPTS],[CFLAGS-ARG],[LIBS-ARG])
#
# DESCRIPTION
#
#   Runs the LIBRARY-config script and defines LIBRARY_CFLAGS and
#   LIBRARY_LIBS unless the user had predefined them in the environment.
#
#   The script must support `--cflags' and `--libs' args. If MINIMUM-VERSION
#   is specified, the script must also support the `--version' arg. If the
#   `--with-library-[exec-]prefix' arguments to ./configure are given, it
#   must also support `--prefix' and `--exec-prefix'. Prefereable use
#   CONFIG-SCRIPTS as config script, CFLAGS-ARG instead of `--cflags` and
#   LIBS-ARG instead of `--libs`, if given.
#
#   The SED-EXPR-EXTRACTOR parameter representes the expression used in sed
#   to extract the version number. Use it if your 'foo-config --version'
#   dumps something like 'Foo library v1.0.0 (alfa)' instead of '1.0.0'.
#
#   The macro respects LIBRARY_CONFIG, LIBRARY_CFLAGS and LIBRARY_LIBS
#   variables. If the first one is defined, it specifies the name of the
#   config script to use. If the latter two are defined, the script is not
#   ran at all and their values are used instead (if only one of them is
#   defined, the empty value of the remaining one is still used).
#
#   Example:
#
#     AX_PATH_GENERIC(Foo, 1.0.0)
#
#   would run `foo-config --version' and check that it is at least 1.0.0, if
#   successful the following variables would be defined and substituted:
#
#     FOO_CFLAGS to `foo-config --cflags`
#     FOO_LIBS   to `foo-config --libs`
#
#   Example:
#
#     AX_PATH_GENERIC([Bar],,,[
#        AC_MSG_ERROR([Cannot find Bar library])
#     ])
#
#   would check for bar-config program, defining and substituting the
#   following variables:
#
#     BAR_CFLAGS to `bar-config --cflags`
#     BAR_LIBS   to `bar-config --libs`
#
#   Example:
#
#     ./configure BAZ_LIBS=/usr/lib/libbaz.a
#
#   would link with a static version of baz library even if `baz-config
#   --libs` returns just "-lbaz" that would normally result in using the
#   shared library.
#
#   This macro is a rearranged version of AC_PATH_GENERIC from Angus Lees.
#
# LICENSE
#
#   Copyright (c) 2009 Francesco Salvestrini <salvestrini@users.sourceforge.net>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 11

AU_ALIAS([AC_PATH_GENERIC], [AX_PATH_GENERIC])
AC_DEFUN([AX_PATH_GENERIC],[
  AC_REQUIRE([AC_PROG_SED])

  dnl we're going to need uppercase and lowercase versions of the
  dnl string `LIBRARY'
  pushdef([UP],   translit([$1], [a-z], [A-Z]))dnl
  pushdef([DOWN], translit([$1], [A-Z], [a-z]))dnl

  AC_ARG_WITH(DOWN-prefix,[AS_HELP_STRING([--with-]DOWN[-prefix=PREFIX], [Prefix where $1 is installed (optional)])],
    DOWN[]_config_prefix="$withval", DOWN[]_config_prefix="")
  AC_ARG_WITH(DOWN-exec-prefix,[AS_HELP_STRING([--with-]DOWN[-exec-prefix=EPREFIX], [Exec prefix where $1 is installed (optional)])],
    DOWN[]_config_exec_prefix="$withval", DOWN[]_config_exec_prefix="")

  AC_ARG_VAR(UP[]_CONFIG, [config script used for $1])
  AC_ARG_VAR(UP[]_CFLAGS, [CFLAGS used for $1])
  AC_ARG_VAR(UP[]_LIBS,   [LIBS used for $1])

  AS_IF([test x$UP[]_CFLAGS != x -o x$UP[]_LIBS != x],[
    dnl Don't run config script at all, use user-provided values instead.
    AC_SUBST(UP[]_CFLAGS)
    AC_SUBST(UP[]_LIBS)
    :
    $4
  ],[
    AS_IF([test x$DOWN[]_config_exec_prefix != x],[
      DOWN[]_config_args="$DOWN[]_config_args --exec-prefix=$DOWN[]_config_exec_prefix"
      AS_IF([test x${UP[]_CONFIG+set} != xset],[
	UP[]_CONFIG=$DOWN[]_config_exec_prefix/bin/DOWN-config
      ])
    ])
    AS_IF([test x$DOWN[]_config_prefix != x],[
      DOWN[]_config_args="$DOWN[]_config_args --prefix=$DOWN[]_config_prefix"
      AS_IF([test x${UP[]_CONFIG+set} != xset],[
	UP[]_CONFIG=$DOWN[]_config_prefix/bin/DOWN-config
      ])
    ])

    AC_PATH_PROGS(UP[]_CONFIG,[$6 DOWN-config],[no])
    AS_IF([test "$UP[]_CONFIG" == "no"],[
      :
      $5
    ],[
      dnl Get the CFLAGS from LIBRARY-config script
      AS_IF([test x"$7" == x],[
	UP[]_CFLAGS="`$UP[]_CONFIG $DOWN[]_config_args --cflags`"
      ],[
	UP[]_CFLAGS="`$UP[]_CONFIG $DOWN[]_config_args $7`"
      ])

      dnl Get the LIBS from LIBRARY-config script
      AS_IF([test x"$8" == x],[
	UP[]_LIBS="`$UP[]_CONFIG $DOWN[]_config_args --libs`"
      ],[
	UP[]_LIBS="`$UP[]_CONFIG $DOWN[]_config_args $8`"
      ])

      AS_IF([test x"$2" != x],[
	dnl Check for provided library version
	AS_IF([test x"$3" != x],[
	  dnl Use provided sed expression
	  DOWN[]_version="`$UP[]_CONFIG $DOWN[]_config_args --version | $SED -e $3`"
	],[
	  DOWN[]_version="`$UP[]_CONFIG $DOWN[]_config_args --version | $SED -e 's/^\ *\(.*\)\ *$/\1/'`"
	])

	AC_MSG_CHECKING([for $1 ($DOWN[]_version) >= $2])
	AX_COMPARE_VERSION($DOWN[]_version,[ge],[$2],[
	  AC_MSG_RESULT([yes])

	  AC_SUBST(UP[]_CFLAGS)
	  AC_SUBST(UP[]_LIBS)
	  :
	  $4
	],[
	  AC_MSG_RESULT([no])
	  :
	  $5
	])
      ],[
	AC_SUBST(UP[]_CFLAGS)
	AC_SUBST(UP[]_LIBS)
	:
	$4
      ])
    ])
  ])

  popdef([UP])
  popdef([DOWN])
])

