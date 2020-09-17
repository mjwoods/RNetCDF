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
if test -n "$( git status --porcelain )"; then
  echo "WARNING: uncommitted changes in package" >&2
fi

for file in configure configure.ac tools/convert.m4 src/convert.c ; do
  if test -n "$(git status --porcelain "$file")"; then
    echo "ERROR: $file has uncommitted changes" >&2
    exit 2
  else
    # Set timestamp on file to match last commit:
    time="$(git log --pretty=format:%cd -n 1 --date=iso -- "$file")"
    time="$(date -j -f '%Y-%m-%d %H:%M:%S %z' "$time" +%Y%m%d%H%M.%S)"
    touch -m -t "$time" "$file"
  fi
done

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
    if [[ -x "$file" ]]; then
      # Preserve execute permissions:
      chmod +x "$file.sed"
    fi
    mv "$file.sed" "$file"
  done
