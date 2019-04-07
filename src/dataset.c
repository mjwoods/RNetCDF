/*=============================================================================*\
 *
 *  Name:       dataset.c
 *
 *  Version:    2.0-1
 *
 *  Purpose:    NetCDF dataset functions for RNetCDF
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright:  (C) 2004-2017 Pavel Michna, Milton Woods
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
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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


/* Convert netcdf file format code to string label.
 */
static const char *
R_nc_format2str (int format)
{
  switch (format) {
  case NC_FORMAT_CLASSIC:
    return "classic";
#ifdef NC_FORMAT_64BIT
  case NC_FORMAT_64BIT:
#elif defined NC_FORMAT_64BIT_OFFSET
  case NC_FORMAT_64BIT_OFFSET:
#endif
    return "offset64";
#ifdef NC_FORMAT_CDF5
  case NC_FORMAT_CDF5:
    return "cdf5";
#endif
  case NC_FORMAT_NETCDF4:
    return "netcdf4";
  case NC_FORMAT_NETCDF4_CLASSIC:
    return "classic4";
  default:
    return "unknown";
  }
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_close()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_close (SEXP ptr)
{
  int *fileid;

  if (TYPEOF (ptr) != EXTPTRSXP) {
    RERROR ("Not a valid NetCDF object");
  }

  fileid = R_ExternalPtrAddr (ptr);
  if (!fileid) {
    RRETURN(R_NilValue);
  }

  R_nc_check (nc_close (*fileid));
  R_Free (fileid);
  R_ClearExternalPtr (ptr);

  RRETURN(R_NilValue);
}

/* Private function used as finalizer during garbage collection.
   It is required to have no return value. */
static void
R_nc_finalizer (SEXP ptr)
{
  R_nc_close (ptr);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_create()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_create (SEXP filename, SEXP clobber, SEXP share, SEXP prefill,
             SEXP format)
{
  int cmode, fillmode, old_fillmode, ncid, *fileid;
  SEXP Rptr, result;
  const char *filep;

  /*-- Determine the cmode ----------------------------------------------------*/
  if (asLogical(clobber) == TRUE) {
    cmode = NC_CLOBBER;
  } else {
    cmode = NC_NOCLOBBER;
  }

  /*-- Determine which buffer scheme shall be used ----------------------------*/
  if (asLogical(share) == TRUE) {
    cmode = cmode | NC_SHARE;
  }

  /*-- Determine the fillmode -------------------------------------------------*/
  if (asLogical(prefill) == TRUE) {
    fillmode = NC_FILL;
  } else {
    fillmode = NC_NOFILL;
  }

  /*-- Set file format (default is netcdf classic) ----------------------------*/
  if (R_nc_strcmp(format, "netcdf4")) {
    cmode = cmode | NC_NETCDF4;
  } else if (R_nc_strcmp(format, "classic4")) {
    cmode = cmode | NC_NETCDF4 | NC_CLASSIC_MODEL;
  } else if (R_nc_strcmp(format, "offset64")) {
    cmode = cmode | NC_64BIT_OFFSET;
  }

  /*-- Create the file --------------------------------------------------------*/
  filep = R_nc_strarg (filename);
  if (strlen (filep) > 0) {
    R_nc_check (nc_create (R_ExpandFileName (filep), cmode, &ncid));
  } else {
    RERROR ("Filename must be a non-empty string");
  }
  result = R_nc_protect (ScalarInteger (ncid));

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = R_nc_protect (R_MakeExternalPtr (fileid, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (result, install ("handle_ptr"), Rptr);

  /*-- Set the fill mode ------------------------------------------------------*/
  R_nc_check (nc_set_fill (ncid, fillmode, &old_fillmode));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_file (SEXP nc)
{
  int ncid, ndims, nvars, ngatts, unlimdimid, format;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  /*-- Inquire about the NetCDF dataset ---------------------------------------*/
  R_nc_check (nc_inq (ncid, &ndims, &nvars, &ngatts, &unlimdimid));
  if (unlimdimid == -1 ) {
    unlimdimid = NA_INTEGER;
  }

  /*-- Inquire about the NetCDF format ----------------------------------------*/
  R_nc_check (nc_inq_format (ncid, &format));

  /*-- Returning the list -----------------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 5)); 
  SET_VECTOR_ELT (result, 0, ScalarInteger (ndims));
  SET_VECTOR_ELT (result, 1, ScalarInteger (nvars));
  SET_VECTOR_ELT (result, 2, ScalarInteger (ngatts));
  SET_VECTOR_ELT (result, 3, ScalarInteger (unlimdimid));
  SET_VECTOR_ELT (result, 4, mkString (R_nc_format2str (format)));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_open()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill)
{
  int ncid, omode, fillmode, old_fillmode, *fileid;
  const char *filep;
  SEXP Rptr, result;

  /*-- Determine the omode ----------------------------------------------------*/
  if (asLogical(write) == TRUE) {
    omode = NC_WRITE;
  } else {
    omode = NC_NOWRITE;
  }

  if (asLogical(share) == TRUE) {
    omode = omode | NC_SHARE;
  }

  /*-- Determine the fillmode -------------------------------------------------*/
  if (asLogical(prefill) == TRUE) {
    fillmode = NC_FILL;
  } else {
    fillmode = NC_NOFILL;
  }

  /*-- Open the file ----------------------------------------------------------*/
  filep = R_nc_strarg (filename);
  if (strlen (filep) > 0) {
    R_nc_check (nc_open (R_ExpandFileName (filep), omode, &ncid));
  } else {
    RERROR ("Filename must be a non-empty string");
  }
  result = R_nc_protect (ScalarInteger (ncid));

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = R_nc_protect (R_MakeExternalPtr (fileid, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (result, install ("handle_ptr"), Rptr);

  /*-- Set the fill mode ------------------------------------------------------*/
  if (asLogical(write) == TRUE) {
    R_nc_check (nc_set_fill (ncid, fillmode, &old_fillmode));
  }

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_sync()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_sync (SEXP nc)
{
  int ncid;

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  ncid = asInteger(nc);
  R_nc_check( R_nc_enddef (ncid));

  /*-- Sync the file ----------------------------------------------------------*/
  R_nc_check (nc_sync (ncid));

  RRETURN(R_NilValue);
}

