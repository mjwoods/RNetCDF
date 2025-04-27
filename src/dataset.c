/*=============================================================================*\
 *
 *  Name:       dataset.c
 *
 *  Version:    2.10-3
 *
 *  Purpose:    NetCDF dataset functions for RNetCDF
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright (C) 2004-2025 Pavel Michna and Milton Woods.
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

#include "config.h"

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

#if defined HAVE_NETCDF_MPI
#include <mpi.h>
#include <netcdf_par.h>
#endif

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
    return "offset64";
#elif defined NC_FORMAT_64BIT_OFFSET
  case NC_FORMAT_64BIT_OFFSET:
    return "offset64";
#endif
#ifdef NC_FORMAT_64BIT_DATA
  case NC_FORMAT_64BIT_DATA:
    return "data64";
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
    error ("Not a valid NetCDF object");
  }

  fileid = R_ExternalPtrAddr (ptr);
  if (!fileid) {
    return R_NilValue;
  }

  R_nc_check (nc_close (*fileid));
  R_Free (fileid);
  R_ClearExternalPtr (ptr);

  return R_NilValue;
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
             SEXP format, SEXP diskless, SEXP persist,
             SEXP mpi_comm, SEXP mpi_info)
{
  int cmode, fillmode, old_fillmode, ncid, *fileid, icommf;
  SEXP Rptr, result;
  const char *filep;
#ifdef HAVE_NETCDF_MPI
  int iinfof;
#endif

  /*-- Determine the cmode ----------------------------------------------------*/
  if (asLogical(clobber) == TRUE) {
    cmode = NC_CLOBBER;
  } else {
    cmode = NC_NOCLOBBER;
  }

#if defined NC_DISKLESS && defined NC_PERSIST
  if (asLogical(diskless) == TRUE) {
    cmode = cmode | NC_DISKLESS;
  }
  if (asLogical(persist) == TRUE) {
    cmode = cmode | NC_PERSIST;
  }
#else
  if (asLogical(diskless) == TRUE) {
    error("NetCDF library does not support diskless files");
  }
#endif

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
  } else if (R_nc_strcmp(format, "data64")) {
#ifdef NC_64BIT_DATA
    cmode = cmode | NC_64BIT_DATA;
#else
    error("NetCDF library does not support data64 format");
#endif
  }

  /*-- Close any stale netcdf handles that could point to this file -----------*/
  R_gc();

  /*-- Create the file --------------------------------------------------------*/
  filep = R_nc_strarg (filename);
  if (strlen (filep) > 0) {
    icommf = asInteger(mpi_comm);
    if (icommf == NA_INTEGER) {
      R_nc_check (nc_create (R_ExpandFileName (filep), cmode, &ncid));
    } else {
#ifdef HAVE_NETCDF_MPI
      iinfof = asInteger(mpi_info);
      if (iinfof == NA_INTEGER) {
        iinfof = MPI_Info_c2f(MPI_INFO_NULL);
      }
      R_nc_check (nc_create_par_fortran (R_ExpandFileName (filep),
                    cmode, icommf, iinfof, &ncid));
#else
      error("MPI not supported");
#endif
    }
  } else {
    error ("Filename must be a non-empty string");
  }
  result = PROTECT(ScalarInteger (ncid));

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = PROTECT(R_MakeExternalPtr (fileid, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (result, install ("handle_ptr"), Rptr);

  /*-- Set the fill mode ------------------------------------------------------*/
  R_nc_check (nc_set_fill (ncid, fillmode, &old_fillmode));

  UNPROTECT(2);
  return result;
}

/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_file (SEXP nc)
{
  int ncid, ndims, nvars, ngatts, unlimdimid, format;
  const char *libvers;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  /*-- Inquire about the NetCDF dataset ---------------------------------------*/
  R_nc_check (nc_inq (ncid, &ndims, &nvars, &ngatts, &unlimdimid));
  if (unlimdimid == -1 ) {
    unlimdimid = NA_INTEGER;
  }

  /*-- Inquire about the NetCDF format and library version --------------------*/
  R_nc_check (nc_inq_format (ncid, &format));
  libvers = nc_inq_libvers ();

  /*-- Returning the list -----------------------------------------------------*/
  result = PROTECT(allocVector (VECSXP, 6)); 
  SET_VECTOR_ELT (result, 0, PROTECT(ScalarInteger (ndims)));
  SET_VECTOR_ELT (result, 1, PROTECT(ScalarInteger (nvars)));
  SET_VECTOR_ELT (result, 2, PROTECT(ScalarInteger (ngatts)));
  SET_VECTOR_ELT (result, 3, PROTECT(ScalarInteger (unlimdimid)));
  SET_VECTOR_ELT (result, 4, PROTECT(mkString (R_nc_format2str (format))));
  SET_VECTOR_ELT (result, 5, PROTECT(mkString (libvers)));

  UNPROTECT(7);
  return result;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_open()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill,
           SEXP diskless, SEXP persist, SEXP mpi_comm, SEXP mpi_info)
{
  int ncid, omode, fillmode, old_fillmode, *fileid, icommf;
  const char *filep;
  SEXP Rptr, result;
#ifdef HAVE_NETCDF_MPI
  int iinfof;
#endif

  /*-- Determine the omode ----------------------------------------------------*/
  if (asLogical(write) == TRUE) {
    omode = NC_WRITE;
  } else {
    omode = NC_NOWRITE;
  }

#if defined NC_DISKLESS && defined NC_PERSIST
  if (asLogical(diskless) == TRUE) {
    omode = omode | NC_DISKLESS;
  }
  if (asLogical(persist) == TRUE) {
    omode = omode | NC_PERSIST;
  }
#else
  if (asLogical(diskless) == TRUE) {
    error("NetCDF library does not support diskless files");
  }
#endif

  if (asLogical(share) == TRUE) {
    omode = omode | NC_SHARE;
  }

  /*-- Determine the fillmode -------------------------------------------------*/
  if (asLogical(prefill) == TRUE) {
    fillmode = NC_FILL;
  } else {
    fillmode = NC_NOFILL;
  }

  /*-- Close any stale netcdf handles that could point to this file -----------*/
  R_gc();

  /*-- Open the file ----------------------------------------------------------*/
  filep = R_nc_strarg (filename);
  if (strlen (filep) > 0) {
    icommf = asInteger(mpi_comm);
    if (icommf == NA_INTEGER) {
      R_nc_check (nc_open (R_ExpandFileName (filep), omode, &ncid));
    } else {
#ifdef HAVE_NETCDF_MPI
      iinfof = asInteger(mpi_info);
      if (iinfof == NA_INTEGER) {
        iinfof = MPI_Info_c2f(MPI_INFO_NULL);
      }
      R_nc_check (nc_open_par_fortran (R_ExpandFileName (filep),
                    omode, icommf, iinfof, &ncid)); 
#else
      error("MPI not supported");
#endif
    }
  } else {
    error ("Filename must be a non-empty string");
  }
  result = PROTECT(ScalarInteger (ncid));

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = PROTECT(R_MakeExternalPtr (fileid, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (result, install ("handle_ptr"), Rptr);

  /*-- Set the fill mode ------------------------------------------------------*/
  if (asLogical(write) == TRUE) {
    R_nc_check (nc_set_fill (ncid, fillmode, &old_fillmode));
  }

  UNPROTECT(2);
  return result;
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

  return R_NilValue;
}

