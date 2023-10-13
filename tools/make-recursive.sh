#!/bin/sh
# Replicate Makevars behaviour by including the same makefiles,
# but include Makefile.common last so it can override the CC variable.
# Executed from src/Makefile.
# Requires the following environment variables:
# MAKE
# WINDOWS=TRUE|FALSE
# R_HOME
# R_ARCH

# Refer to lists of makefiles for Unix-alike and Windows at:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories

# Store make arguments in positional parameters to handle spaces properly.

# Find relevant sub-directories of R.
# We assume that R executables are stored in ${R_HOME}/bin.
R_bin="${R_HOME}/bin"
R_etc=`"${R_bin}/Rscript" -e 'cat(R.home("etc"))'`
R_share=`"${R_bin}/Rscript" -e 'cat(R.home("share"))'`

# First makefile is Makeconf from R:
set -- -f "${R_etc}/${R_ARCH}/Makeconf"

# Second makefile is site Makevars (if present):
makevars_site=`"${R_bin}/Rscript" -e 'cat(tools::makevars_site())'`
if test -n "${makevars_site}"; then
  set -- "$@" -f "${makevars_site}"
fi

# Third makefile is (win)shlib.mk from R, as used by Makevars:
if test "$WINDOWS" = TRUE; then
  file=winshlib.mk
else
  file=shlib.mk
fi
set -- "$@" -f "${R_share}/make/$file"

# Fourth makefile is user Makevars (if present):
makevars_user=`"${R_bin}/Rscript" -e 'cat(tools::makevars_user())'`
if test -n "${makevars_user}"; then
  set -- "$@" -f "${makevars_user}"
fi

# Last makefile is package Makefile.common:
set -- "$@" -f Makefile.common

# Append arguments for required variables:
OBJECTS=
for file in *.c; do
  OBJECTS="$OBJECTS ${file%.c}.o"
done

set -- "$@" OBJECTS="$OBJECTS" SHLIB="RNetCDF\$(SHLIB_EXT)"

# Append target:
set -- "$@" RNetCDF_all

# Run make:
echo "$MAKE" "$@"
"$MAKE" "$@"

