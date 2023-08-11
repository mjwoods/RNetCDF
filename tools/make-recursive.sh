#!/bin/sh
# Replicate Makevars behaviour by including the same makefiles,
# but include Makefile.common later so it can override the CC variable.
# Executed from src/Makefile.
# Requires the following environment variables:
# MAKE
# WINDOWS=TRUE|FALSE
# R_HOME
# R_ARCH

# Refer to lists of makefiles for Unix-alike and Windows at:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories

# First makefile is Makeconf from R_HOME:
MAKEARGS="-f ${R_HOME}/etc/${R_ARCH}/Makeconf"

# Second makefile is site Makevars (if present):
for file in "${R_MAKEVARS_SITE}" "${R_HOME}/etc/${R_ARCH}/Makevars.site"; do
  if test -f "$file"; then
    MAKEARGS="$MAKEARGS -f $file"
  fi
done

# Third makefile is package Makefile.common:
MAKEARGS="$MAKEARGS -f Makefile.common"

# Fourth makefile is (win)shlib.mk from R_HOME, as used by Makevars:
if test "$WINDOWS" = TRUE; then
  MAKEARGS="$MAKEARGS -f ${R_HOME}/share/make/winshlib.mk"
else
  MAKEARGS="$MAKEARGS -f ${R_HOME}/share/make/shlib.mk"
fi

# Last makefile is user Makevars for platform (if present):
if test "$WINDOWS" = TRUE; then
  for file in "${R_MAKEVARS_USER}" ~/.R/Makevars.ucrt ~/.R/Makevars.win64 ~/.R/Makevars.win; do
    if test -f "$file"; then
      MAKEARGS="$MAKEARGS -f $file"
    fi
  done
else
  for file in "${R_MAKEVARS_USER}" ~/.R/Makevars; do
    if test -f "$file"; then
      MAKEARGS="$MAKEARGS -f $file"
    fi
  done
fi

# Run make:
$MAKE $MAKEARGS

