/*=============================================================================*\
 *
 *  Name:       dimension.c
 *
 *  Version:    2.0-1
 *
 *  Purpose:    NetCDF dimension functions for RNetCDF
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright:  (C) 2004-2019 Pavel Michna, Milton Woods
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
 *  R_nc_def_dim()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_dim (SEXP nc, SEXP dimname, SEXP size, SEXP unlim)
{
  int ncid, dimid;
  const char *dimnamep;
  size_t nccnt;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  dimnamep = R_nc_strarg (dimname);

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Create the dimension ---------------------------------------------------*/
  if (asLogical(unlim) == TRUE) {
    nccnt = NC_UNLIMITED;
  } else {
    nccnt = R_nc_sizearg (size);
  }

  R_nc_check (nc_def_dim (ncid, dimnamep, nccnt, &dimid));

  result = R_nc_protect (ScalarInteger (dimid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_unlimids()
\*-----------------------------------------------------------------------------*/

/* Private function to find unlimited dimensions of a file or group.
   Returns netcdf status. If no error occurs, nunlim and unlimids are set.
   Note - some netcdf4 versions only return unlimited dimensions defined in a group,
     not those defined in the group and its ancestors as claimed in documentation.
 */
static int
R_nc_unlimdims (int ncid, int *nunlim, int **unlimids)
{
  int status, format;

  *nunlim = 0;

  status = nc_inq_format (ncid, &format);
  if (status != NC_NOERR) {
    return status;
  }

  if (format == NC_FORMAT_NETCDF4) {
    status = nc_inq_unlimdims (ncid, nunlim, NULL);
    if (status != NC_NOERR) {
      return status;
    }

    *unlimids = (void *) (R_alloc (*nunlim, sizeof (int)));

    status = nc_inq_unlimdims (ncid, NULL, *unlimids);

  } else {
    *unlimids = (void *) (R_alloc (1, sizeof (int)));
    status = nc_inq_unlimdim (ncid, *unlimids);
    if (status == NC_NOERR && **unlimids != -1) {
      *nunlim = 1;
    }
  }

  return status;
}


SEXP
R_nc_inq_unlimids (SEXP nc)
{
  int ncid, nunlim, *unlimids=NULL;
  SEXP result;

  ncid = asInteger (nc);

  R_nc_check (R_nc_unlimdims (ncid, &nunlim, &unlimids));

  result = R_nc_protect (allocVector (INTSXP, nunlim));

  /* Sort temporary results and copy to output structure */
  if (nunlim > 0) {
    R_isort(unlimids, nunlim);
    memcpy (INTEGER (result), unlimids, nunlim * sizeof (int));
  }

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dim()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dim (SEXP nc, SEXP dim)
{
  int ncid, nunlim, *unlimids=NULL, isunlim, dimid, ii;
  size_t dimlen;
  char dimname[NC_MAX_NAME + 1];
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_dim_id (dim, ncid, &dimid, 0));

  /*-- Inquire the dimension --------------------------------------------------*/
  R_nc_check (nc_inq_dim (ncid, dimid, dimname, &dimlen));

  /*-- Check if it is an unlimited dimension ----------------------------------*/
  R_nc_check (R_nc_unlimdims (ncid, &nunlim, &unlimids));

  isunlim = 0;
  for (ii = 0; ii < nunlim; ii++) {
    if (unlimids[ii] == dimid) {
      isunlim = 1;
      break;
    }
  }

  /*-- Returning the list -----------------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 4));
  SET_VECTOR_ELT (result, 0, ScalarInteger (dimid));
  SET_VECTOR_ELT (result, 1, mkString (dimname));
  /* Dimension length may be larger than integer, so return as double */
  SET_VECTOR_ELT (result, 2, ScalarReal (dimlen));
  SET_VECTOR_ELT (result, 3, ScalarLogical (isunlim));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_dim()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_dim (SEXP nc, SEXP dim, SEXP newname)
{
  int ncid, dimid;
  const char *newnamep;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_dim_id (dim, ncid, &dimid, 0));

  newnamep = R_nc_strarg (newname);

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the dimension ---------------------------------------------------*/
  R_nc_check (nc_rename_dim (ncid, dimid, newnamep));

  RRETURN(R_NilValue);
}


