#!/bin/sh
# Prepare for new RNetCDF release

set -e
set -u

# Check inputs:
if [[ $# -ne 1 ]]; then
  echo "Usage: $0 x.y-z" >&2
  exit 1
fi

newver="$1"
if ! echo "$newver" | grep -q '^[0-9]\+.[0-9]\+-[0-9]\+$' ; then
  echo "ERROR: invalid format of release string" >&2
  exit 1
fi

# Find base directory of package from location of this script:
thisdir="$( dirname "$0" )"
basedir="$( cd "$thisdir/.." && pwd )"

# Check that generated files are up-to-date:
if [[ "$basedir/configure.ac" -nt "$basedir/configure" ]]; then
  echo "ERROR: configure.ac is newer than configure" >&2
  exit 2
fi

if [[ "$basedir/tools/convert.m4" -nt "$basedir/src/convert.c" ]]; then
  echo "ERROR: tools/convert.m4 is newer than src/convert.c" >&2
  exit 3
fi

# Check that NEWS contains a description of the release:
if ! grep -q "$newver" "$basedir/NEWS"; then
  echo "WARNING: NEWS has no entry for release $newver" >&2
fi

# Get existing version string:
oldver="$( grep 'Version: ' "$basedir/DESCRIPTION" | awk '{ print $2 }' )"

# Replace version string in all files (excluding hidden files).
# In-place option of sed is not portable between GNU and BSD versions.
find "$basedir" -name '.*' -prune -o -type f -print | while read file; do
    sed "s|$oldver|$newver|g" "$file" >"$file.sed"
    mv "$file.sed" "$file"
  done
