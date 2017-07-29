/*=============================================================================*\
 *									       *
 *  Name:       udunits.c						       *
 *									       *
 *  Version:    2.0-1							       *
 *									       *
 *  Purpose:    udunits functions for RNetCDF.				       *
 *									       *
 *  Author:     Pavel Michna (michna@giub.unibe.ch)			       *
 *              Milton Woods (m.woods@bom.gov.au)                              *
 *									       *
 *  Copyright:  (C) 2004-2017 Pavel Michna                                     *
 *									       *
 *=============================================================================*
 *									       *
 *  This program is free software; you can redistribute it and/or modify       *
 *  it under the terms of the GNU General Public License as published by       *
 *  the Free Software Foundation; either version 2 of the License, or	       *
 *  (at your option) any later version. 				       *
 *									       *
 *  This program is distributed in the hope that it will be useful,	       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of	       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	       *
 *  GNU General Public License for more details.			       *
 *									       *
 *  You should have received a copy of the GNU General Public License	       *
 *  along with this program; if not, write to the Free Software 	       *
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *									       *
 *=============================================================================*
 *  Implementation and Revisions					       *
 *-----------------------------------------------------------------------------*
 * $Header$ *
\*=============================================================================*/


/*=============================================================================*\
 *  Includes								       *
\*=============================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <float.h>

#include <netcdf.h>

#ifdef HAVE_UDUNITS2_UDUNITS_H
# include <udunits2/udunits.h>
#else
# include <udunits.h>
#endif

#include <R.h>
#include <Rinternals.h>

#include "common.h"
#include "udunits.h"


/* Convert udunits error code to a string */
static const char *
R_nc_uterror (int errcode)
{
  switch (errcode) {
  case UT_EOF:
    return "end-of-file encountered (udunits)";
  case UT_ENOFILE:
    return "no units-file (udunits)";
  case UT_ESYNTAX:
    return "syntax error (udunits)";
  case UT_EUNKNOWN:
    return "unknown specification (udunits)";
  case UT_EIO:
    return "I/O error (udunits)";
  case UT_EINVALID:
    return "invalid unit-structure (udunits)";
  case UT_ENOINIT:
    return "package not initialized (udunits)";
  case UT_ECONVERT:
    return "two units are not convertable (udunits)";
  case UT_EALLOC:
    return "memory allocation failure (udunits)";
  case UT_ENOROOM:
    return "insufficient room supplied (udunits)";
  case UT_ENOTTIME:
    return "not a unit of time (udunits)";
  default:
    return "unknown error (udunits)";
  }
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_calendar()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_calendar (SEXP unitstring, SEXP values)
{
  int year, month, day, hour, minute, status, isreal;
  float second;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  double dtmp, *dout;
  size_t ii, count;
  utUnit utunit;
  SEXP result;

  /* Handle arguments and initialise outputs */
  cstring = CHAR (STRING_ELT (unitstring, 0));
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values);

  result = R_nc_protect (allocMatrix (REALSXP, count, 6));
  dout = REAL (result);

  /*-- Scan unitstring --------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
  utIni (&utunit);
#endif

  status = utScan (cstring, &utunit);
  if (status != 0) {
    goto cleanup;
  }

  /*-- Check if unit is time and has origin -----------------------------------*/
  if (!utIsTime (&utunit)) {
    status = UT_ENOTTIME;
    goto cleanup;
  }

  if (!utHasOrigin (&utunit)) {
    status = UT_EINVALID;
    goto cleanup;
  }

  /*-- Convert values ---------------------------------------------------------*/
  for (ii = 0; ii < count; ii++) {
    if (isreal) {
      dtmp = dvals[ii];
    } else {
      dtmp = (ivals[ii] == NA_INTEGER) ? NA_REAL : ((double) ivals[ii]);
    } 
    if (R_FINITE (dtmp)) {
      status = utCalendar (dtmp, &utunit, &year, &month, &day,
                           &hour, &minute, &second);
      if (status != 0) {
	goto cleanup;
      }
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
#ifdef HAVE_LIBUDUNITS2
  utFree (&utunit);
#endif
  if (status != 0) {
    RERROR (R_nc_uterror (status));
  }
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_utinit()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_utinit (SEXP path)
{
  int status;

  /*-- Avoid "overriding default" messages from UDUNITS-2 (1/2) ---------------*/
#ifdef HAVE_LIBUDUNITS2
  ut_set_error_message_handler (ut_ignore);
#endif

  /*-- Initialize udunits library ---------------------------------------------*/
  status = utInit (R_ExpandFileName (CHAR (STRING_ELT (path, 0))));

  /*-- Avoid "overriding default" messages from UDUNITS-2 (2/2) ---------------*/
#ifdef HAVE_LIBUDUNITS2
  ut_set_error_message_handler (ut_write_to_stderr);
#endif

  /*-- Returning the list -----------------------------------------------------*/
  if (status != 0) {
    RERROR (R_nc_uterror (status));
  }
  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inv_calendar()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inv_calendar (SEXP unitstring, SEXP values)
{
  int status, itmp, isreal, isfinite;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  double datetime[6], *dout, dtmp;
  size_t ii, jj, count;
  utUnit utunit;
  SEXP result;

  /* Handle arguments and initialise outputs */
  cstring = CHAR (STRING_ELT (unitstring, 0));
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values) / 6;

  result = R_nc_protect (allocVector (REALSXP, count));
  dout = REAL (result);

  /*-- Scan unitstring --------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
  utIni (&utunit);
#endif

  status = utScan (cstring, &utunit);
  if (status != 0) {
    goto cleanup;
  }

  /*-- Check if unit is time and has origin -----------------------------------*/
  if (!utIsTime (&utunit)) {
    status = UT_ENOTTIME;
    goto cleanup;
  }

  if (!utHasOrigin (&utunit)) {
    status = UT_EINVALID;
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
      status = utInvCalendar (datetime[0], datetime[1], datetime[2],
                              datetime[3], datetime[4], datetime[5],
                              &utunit, &dout[ii]);
      if (status != 0) {
        goto cleanup;
      }
    } else {
      dout[ii] = NA_REAL;
    }
  }

  /*-- Returning the list -----------------------------------------------------*/
cleanup:
#ifdef HAVE_LIBUDUNITS2
  utFree (&utunit);
#endif
  if (status != 0) {
    RERROR (R_nc_uterror (status));
  }
  RRETURN(result);
}

