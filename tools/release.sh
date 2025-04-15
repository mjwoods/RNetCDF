#!/bin/sh
# Prepare for new RNetCDF release

set -e
set -u

# Check inputs:
if [ $# -ne 1 ]; then
  echo "Usage: $0 x.y-z" >&2
  exit 1
fi

newver="$1"
if ! echo "$newver" | grep -q '^[0-9]\+.[0-9]\+-[0-9]\+$' ; then
  echo "ERROR: invalid format of release string" >&2
  exit 1
fi

# Set working directory to base directory of package:
thisdir="$( dirname "$0" )"
cd "$thisdir/.."

# Use GNU versions of some utilities:
kernel=$( uname )
if [ "$kernel" = Darwin ]; then
  alias date=gdate # MacPorts coreutils
  alias find=gfind # MacPorts findutils
  alias sed=gsed   # MacPorts gsed
fi

# Define function to convert date formats:
DATEFMT() {
  date -d "$1" +%Y%m%d%H%M.%S
}

# Check that package changes have been committed to git:
if test -n "$( git status --porcelain )"; then
  echo "WARNING: uncommitted changes in package" >&2
fi

# Ensure that generated files are up-to-date:
tools/update-configure.sh
tools/update-convert-c.sh

# Check that NEWS contains a description of the release:
if ! grep -q "$newver" NEWS; then
  echo "WARNING: NEWS has no entry for release $newver" >&2
fi

# Get existing version string:
oldver="$( grep 'Version: ' DESCRIPTION | awk '{ print $2 }' )"

# Define copyright line:
year=$( date +%Y )
copyright="Copyright (C) 2004-$year Pavel Michna and Milton Woods."

# Replace version string in all files (excluding hidden files).
find . -mindepth 1 -name '.*' -prune -o -type f \
  ! -name NEWS ! -name release.sh -print | while read file; do
    sed -i "/RNetCDF\|[Vv][Ee][Rr][Ss][Ii][Oo][Nn]/ s|$oldver|$newver|g;
            /Copyright.*Michna/ s|\(.*\)Copyright.*|\1$copyright|" "$file"
  done

# Update release date in DESCRIPTION:
newdate="$( date +%Y-%m-%d )"
sed -i "s|Date:.*|Date: $newdate|" DESCRIPTION
