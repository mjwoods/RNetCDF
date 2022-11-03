/*=============================================================================*\
 *
 *  Name:       udunits.c
 *
 *  Version:    2.7-0
 *
 *  Purpose:    udunits2 functions for RNetCDF.
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


/*=============================================================================*\
 *  Includes
\*=============================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <float.h>

#include <netcdf.h>

#include <R.h>
#include <Rinternals.h>

#include "common.h"
#include "RNetCDF.h"


#if !(defined HAVE_LIBUDUNITS2 && \
      (defined HAVE_UDUNITS2_H || defined HAVE_UDUNITS2_UDUNITS2_H))

/*=============================================================================*\
 *  UDUNITS2 is NOT installed
\*=============================================================================*/

SEXP
R_nc_calendar (SEXP unitstring, SEXP values)
{
  error ("RNetCDF was built without UDUNITS-2");
}

SEXP
R_nc_utinit (SEXP path)
{
  return R_NilValue;
}

SEXP
R_nc_inv_calendar (SEXP unitstring, SEXP values)
{
  error ("RNetCDF was built without UDUNITS-2");
}

SEXP
R_nc_utterm ()
{
  return R_NilValue;
}

#else

/*=============================================================================*\
 *  UDUNITS2 is installed
\*=============================================================================*/

#ifdef HAVE_UDUNITS2_UDUNITS2_H
# include <udunits2/udunits2.h>
#else
# include <udunits2.h>
#endif

/* Static variables */
static ut_system *R_nc_units=NULL;


/* Convert udunits2 error code to a string.
   The udunits2 library allows us to define a global error handler,
   but that could conflict with other R packages (e.g. units, udunits2).
 */
static const char *
R_nc_uterror (ut_status errcode)
{
  switch (errcode) {
  case UT_BAD_ARG:
    return "Bad argument (udunits)";
  case UT_EXISTS:
    return "Unit, prefix, or identifier already exists (udunits)";
  case UT_NO_UNIT:
    return "No such unit exists (udunits)";
  case UT_OS:
    return "Operating-system error (udunits)";
  case UT_NOT_SAME_SYSTEM:
    return "Units belong to different unit-systems (udunits)";
  case UT_MEANINGLESS:
    return "Operation on the unit or units is meaningless (udunits)";
  case UT_NO_SECOND:
    return "Unit-system doesn't have a unit named 'second' (udunits)";
  case UT_VISIT_ERROR:
    return "Error occurred while visiting a unit (udunits)";
  case UT_CANT_FORMAT:
    return "Unit can't be formatted in the desired manner (udunits)";
  case UT_SYNTAX:
    return "String unit representation contains syntax error (udunits)";
  case UT_UNKNOWN:
    return "String unit representation contains unknown word (udunits)";
  case UT_OPEN_ARG:
    return "Can't open argument-specified unit database (udunits)";
  case UT_OPEN_ENV:
    return "Can't open environment-specified unit database (udunits)";
  case UT_OPEN_DEFAULT:
    return "Can't open installed, default, unit database (udunits)";
  case UT_PARSE:
    return "Error parsing unit database (udunits)";
  default:
    return "Unknown error (udunits)";
  }
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_calendar()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_calendar (SEXP unitstring, SEXP values)
{
  int year, month, day, hour, minute, isreal;
  double second, res, din, dencode, *dout;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  size_t ii, count;
  ut_unit *inunit=NULL, *refunit=NULL, *secunit=NULL;
  cv_converter *converter=NULL;
  ut_status status;
  SEXP result;

  /* Handle arguments and initialise outputs */
  cstring = R_nc_strarg (unitstring);
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values);

  result = PROTECT(allocMatrix (REALSXP, count, 6));
  dout = REAL (result);

  /* Parse unitstring */
  inunit = ut_parse (R_nc_units, cstring, UT_ASCII);
  if (!inunit) {
    goto cleanup;
  }

  /* Prepare for conversion to encoded time values used internally by udunits2 */
  secunit = ut_get_unit_by_name (R_nc_units, "second");
  if (!secunit) {
    goto cleanup;
  }
  refunit = ut_offset_by_time (secunit, 0.0);
  if (!refunit) {
    goto cleanup;
  }
  converter = ut_get_converter (inunit, refunit);
  if (!converter) {
    goto cleanup;
  }

  /*-- Convert values ---------------------------------------------------------*/
  for (ii = 0; ii < count; ii++) {
    if (isreal) {
      din = dvals[ii];
    } else {
      din = (ivals[ii] == NA_INTEGER) ? NA_REAL : ((double) ivals[ii]);
    } 
    if (R_FINITE (din)) {
      dencode = cv_convert_double (converter, din);
      ut_decode_time (dencode, &year, &month, &day, &hour, &minute, &second, &res);
      dout[ii] = year;
      dout[ii + count] = month;
      dout[ii + 2 * count] = day;
      dout[ii + 3 * count] = hour;
      dout[ii + 4 * count] = minute;
      dout[ii + 5 * count] = second;
    } else {
      dout[ii] = NA_REAL;
      dout[ii + count] = NA_REAL;
      dout[ii + 2 * count] = NA_REAL;
      dout[ii + 3 * count] = NA_REAL;
      dout[ii + 4 * count] = NA_REAL;
      dout[ii + 5 * count] = NA_REAL;
    }
  }

  /*-- Returning the array ----------------------------------------------------*/
cleanup:
  status = ut_get_status ();
  if (inunit) {
    ut_free (inunit);
  }
  if (refunit) {
    ut_free (refunit);
  }
  if (secunit) {
    ut_free (secunit);
  }
  if (converter) {
    cv_free (converter); 
  }

  if (status != UT_SUCCESS) {
    error (R_nc_uterror (status));
  }

  UNPROTECT(1);
  return result;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_utinit()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_utinit (SEXP path)
{
  const char *pathp;

  /* Free units if initialised previously */
  R_nc_utterm();

  /* Initialise a units system */
  pathp = R_nc_strarg (path);
  R_nc_units = ut_read_xml (pathp);

  if (!R_nc_units) {
    error (R_nc_uterror (ut_get_status ()));
  }
  return R_NilValue;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inv_calendar()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inv_calendar (SEXP unitstring, SEXP values)
{
  int itmp, isreal, isfinite;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  double datetime[6], dtmp, dencode, *dout;
  size_t ii, jj, count;
  ut_unit *outunit=NULL, *refunit=NULL, *secunit=NULL;
  cv_converter *converter=NULL;
  ut_status status;

  SEXP result;

  /* Handle arguments and initialise outputs */
  cstring = R_nc_strarg (unitstring);
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values) / 6;

  result = PROTECT(allocVector (REALSXP, count));
  dout = REAL (result);

  /* Parse unitstring */
  outunit = ut_parse (R_nc_units, cstring, UT_ASCII);
  if (!outunit) {
    goto cleanup;
  }

  /* Prepare for conversion to encoded time values used internally by udunits2 */
  secunit = ut_get_unit_by_name (R_nc_units, "second");
  if (!secunit) {
    goto cleanup;
  }
  refunit = ut_offset_by_time (secunit, 0.0);
  if (!refunit) {
    goto cleanup;
  }
  converter = ut_get_converter (refunit, outunit);
  if (!converter) {
    goto cleanup;
  }

  /*-- Convert values ---------------------------------------------------------*/
  for (ii = 0; ii < count; ii++) {
    isfinite = 1;
    if (isreal) {
      for (jj = 0; jj < 6; jj++) {
        dtmp = dvals[ii + jj*count];
        if (R_FINITE (dtmp)) {
          datetime[jj] = dtmp;
        } else {
          isfinite = 0;
          break;
        }
      }
    } else {
      for (jj = 0; jj < 6; jj++) {
        itmp = ivals[ii + jj*count];
        if (itmp == NA_INTEGER) {
          isfinite = 0;
          break;
        } else {
          datetime[jj] = itmp;
        }
      }
    }
    if (isfinite) {
      dencode = ut_encode_time (datetime[0], datetime[1], datetime[2],
                                datetime[3], datetime[4], datetime[5]);
      dout[ii] = cv_convert_double (converter, dencode);
    } else {
      dout[ii] = NA_REAL;
    }
  }

  /* Returning the array */
cleanup:
  status = ut_get_status ();
  if (outunit) {
    ut_free (outunit);
  }
  if (refunit) {
    ut_free (refunit);
  }
  if (secunit) {
    ut_free (secunit);
  }
  if (converter) {
    cv_free (converter); 
  }

  if (status != UT_SUCCESS) {
    error (R_nc_uterror (status));
  }

  UNPROTECT(1);
  return result;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_utterm()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_utterm ()
{
  if (R_nc_units) {
    ut_free_system (R_nc_units);
    R_nc_units = NULL;
  }
  return R_NilValue;
}

#endif /* Conditional compilation with UDUNITS2 */
