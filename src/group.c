/*=============================================================================*\
 *
 *  Name:       group.c
 *
 *  Version:    2.7-0
 *
 *  Purpose:    NetCDF group functions for RNetCDF.
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright (C) 2004-2022 Pavel Michna and Milton Woods.
 *
 *=============================================================================*
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *=============================================================================*
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <float.h>

#include <R.h>
#include <Rinternals.h>

#include <netcdf.h>

#include "common.h"
#include "RNetCDF.h"


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_grp()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_grp (SEXP nc, SEXP grpname)
{
  int ncid, grpid;
  const char *cgrpname;

  /* Convert arguments to netcdf ids */
  ncid = asInteger (nc);

  cgrpname = R_nc_strarg (grpname);

  /* Enter define mode */
  R_nc_check( R_nc_redef (ncid));

  /* Define the group */
  R_nc_check (nc_def_grp (ncid, cgrpname, &grpid));

  return ScalarInteger (grpid);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_parent()
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_parent (SEXP nc)
{
  int ncid, grpid;

  /* Get parent group */
  ncid = asInteger (nc);
  R_nc_check (nc_inq_grp_parent (ncid, &grpid));

  return ScalarInteger (grpid);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_natts()
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_natts (SEXP nc)
{
  int ncid, natts;

  /* Get number of attributes in group */
  ncid = asInteger (nc);
  R_nc_check (nc_inq_natts (ncid, &natts));

  return ScalarInteger (natts);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grpname()
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grpname (SEXP nc, SEXP full)
{
  int ncid;
  size_t namelen;
  char *name, *fullname, namebuf[NC_MAX_NAME+1];

  ncid = asInteger (nc);

  if (asLogical (full) == TRUE) {
    R_nc_check (nc_inq_grpname_full (ncid, &namelen, NULL));

    fullname = R_alloc (namelen + 1, sizeof (char));
    R_nc_check (nc_inq_grpname_full (ncid, NULL, fullname));
    name = fullname;
  } else {
    R_nc_check (nc_inq_grpname (ncid, namebuf));
    name = namebuf;
  }

  return mkString (name);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_ncid()
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_ncid (SEXP nc, SEXP grpname, SEXP full)
{
  int ncid, grpid;
  const char *cgrpname;

  ncid = asInteger (nc);
  cgrpname = R_nc_strarg (grpname);

  if (asLogical (full) == TRUE) {
    R_nc_check (nc_inq_grp_full_ncid (ncid, cgrpname, &grpid));
  } else {
    R_nc_check (nc_inq_grp_ncid (ncid, cgrpname, &grpid));
  }

  return ScalarInteger (grpid);
}


/*-----------------------------------------------------------------------------*\
 *  Get lists of ncids for components of a group
\*-----------------------------------------------------------------------------*/

/* Template function returning a list of ncids for a group */
#define INQGRPIDS(RFUN, NCFUN) \
SEXP RFUN (SEXP nc) \
{ \
  int    ncid, count; \
  SEXP result; \
  ncid = asInteger (nc); \
  R_nc_check(NCFUN(ncid, &count, NULL)); \
  result = PROTECT(allocVector (INTSXP, count)); \
  R_nc_check(NCFUN(ncid, NULL, INTEGER(result))); \
  UNPROTECT(1); \
  return result; \
}

INQGRPIDS (R_nc_inq_grps, nc_inq_grps)
INQGRPIDS (R_nc_inq_typeids, nc_inq_typeids)
INQGRPIDS (R_nc_inq_varids, nc_inq_varids)


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dimids()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dimids (SEXP nc, SEXP ancestors)
{
  int ncid, full, count;
  SEXP result;

  ncid = asInteger (nc);
  full = (asLogical (ancestors) == TRUE);

  R_nc_check (nc_inq_dimids (ncid, &count, NULL, full));
  result = PROTECT(allocVector (INTSXP, count));
  R_nc_check (nc_inq_dimids (ncid, NULL, INTEGER (result), full));
  UNPROTECT(1);
  return result;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_grp()
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_rename_grp (SEXP nc, SEXP grpname)
{
#ifdef HAVE_NC_RENAME_GRP
  int ncid;
  const char *cgrpname;

  ncid = asInteger (nc);
  cgrpname = R_nc_strarg (grpname);

  /* Enter define mode */
  R_nc_check( R_nc_redef (ncid));

  /* Rename the group */
  R_nc_check (nc_rename_grp (ncid, cgrpname));

  return R_NilValue;

#else
  error ("nc_rename_grp not supported by netcdf library");
#endif
}

