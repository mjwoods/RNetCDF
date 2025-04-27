#!/bin/sh
# Update configure script(s)

set -e
set -u

# Set working directory to base directory of package:
thisdir="$( dirname "$0" )"
cd "$thisdir/.."

# Update configure for Unix-like platforms:
autoconf configure.ac > configure

# Update config.h.in:
autoheader

# Cleanup
rm -rf autom4te.cache

