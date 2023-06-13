#!/bin/sh
# Update src/convert.c from tools/convert.m4

set -e
set -u

# Set working directory to base directory of package:
thisdir="$( dirname "$0" )"
cd "$thisdir/.."

# Run m4:
m4 tools/convert.m4 > src/convert.c

