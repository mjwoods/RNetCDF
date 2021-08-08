/* NOTE: This code was generated from tools/convert.m4 */

/*=============================================================================*\
 *
 *  Name:       convert.c
 *
 *  Version:    2.5-1
 *
 *  Purpose:    Type conversions for RNetCDF
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright (C) 2004-2021 Pavel Michna and Milton Woods.
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

#include <R.h>
#include <Rinternals.h>

#include <netcdf.h>

#include "common.h"
#include "convert.h"

/*=============================================================================*\
 *  Local macros, constants and variables
\*=============================================================================*/

/* Definition of missing value used by bit64 package */
#define NA_INTEGER64 LLONG_MIN

/* Maximum length of R character string */
#define RNC_CHARSXP_MAXLEN 2147483647

/* Conversion from 64-bit integers to double may round upwards,
   so that the double cannot be converted back to the original type.
   The following limits can be safely converted both ways.
 */
static const double LLONG_MAX_DBL = \
  ((double) LLONG_MAX) * (1.0 - DBL_EPSILON);
static const double LLONG_MIN_DBL = \
  ((double) LLONG_MIN) * (1.0 - DBL_EPSILON);
static const double ULLONG_MAX_DBL = \
  ((double) ULLONG_MAX) * (1.0 - DBL_EPSILON);
static const double SIZE_MAX_DBL = \
  ((double) SIZE_MAX) * (1.0 - DBL_EPSILON);


/*=============================================================================*\
 *  Memory management.
\*=============================================================================*/

size_t
R_nc_length (int ndims, const size_t *count)
{
  int ii;
  size_t length;

  if (ndims < 0) {
    /* Vector of length count[0] */
    ndims = 1;
  }

  length = 1;
  for ( ii=0; ii<ndims; ii++ ) {
    length *= count[ii]; 
  }
  return (length);
}


size_t
R_nc_length_sexp (SEXP count)
{
  size_t length, ii, ndims;
  double *rcount;
  int *icount;

  ndims = xlength (count);

  // Assume scalar if count is empty
  length = 1;

  if (isReal (count)) {
    rcount = REAL (count);
    for ( ii=0; ii<ndims; ii++ ) {
      length *= rcount[ii]; 
    }
    if (!R_FINITE (length)) {
      error ("Non-finite length in R_nc_length_sexp");
    }
  } else if (isInteger (count)) {
    icount = INTEGER (count);
    for ( ii=0; ii<ndims; ii++ ) {
      if (icount[ii] != NA_INTEGER) {
        length *= icount[ii];
      } else {
        error ("Missing value in R_nc_length_sexp");
      }
    }
  } else if (!isNull (count)) {
    error ("Unsupported type in R_nc_length_sexp");
  }

  return (length);
}


SEXP
R_nc_allocArray (SEXPTYPE type, int ndims, const size_t *ccount) {
  SEXP result, rdim;
  int *intp, ii, jj;
  if (ndims > 0) {
    rdim = PROTECT( allocVector (INTSXP, ndims));
    intp = INTEGER (rdim);
    for ( ii=0, jj=ndims-1; ii<ndims; ii++, jj-- ) {
      if (ccount[jj] <= INT_MAX) {
        intp[ii] = ccount[jj];
      } else {
        error ("R array dimension cannot exceed range of type int");
      }
    }
    result = allocArray (type, rdim);
    UNPROTECT(1);
  } else if (ndims == 0) {
    /* R scalar with no dimensions */
    result = allocVector (type, 1);
  } else {
    /* R vector of length ccount[0] without a dimension attribute */
    result = allocVector (type, ccount[0]);
  }
  return result;
}


/*=============================================================================*\
 *  String conversions.
\*=============================================================================*/


static char *
R_nc_strsxp_char (SEXP rstr, int ndim, const size_t *xdim)
{
  size_t ii, strlen, cnt;
  char *carr, *thisstr;
  if (ndim > 0) {
    /* Omit fastest-varying dimension from R character array */
    strlen = xdim[ndim-1];
    cnt = R_nc_length (ndim-1, xdim);
  } else if (ndim == 0) {
    /* Scalar character */
    strlen = 1;
    cnt = 1;
  } else {
    /* Single string */
    strlen = xdim[0];
    cnt = 1;
  }
  if ((size_t) xlength (rstr) < cnt) {
    error (RNC_EDATALEN);
  }
  carr = R_alloc (cnt*strlen, sizeof (char));
  for (ii=0, thisstr=carr; ii<cnt; ii++, thisstr+=strlen) {
    strncpy(thisstr, CHAR( STRING_ELT (rstr, ii)), strlen);
  }
  return carr;
}


static SEXP
R_nc_char_strsxp_init (R_nc_buf *io)
{
  if (io->ndim > 0) {
    io->rxp = PROTECT(R_nc_allocArray (STRSXP, (io->ndim)-1, io->xdim));
  } else {
    /* Single character or string */
    io->rxp = PROTECT(R_nc_allocArray (STRSXP, 0, io->xdim));
  }
  if (!io->cbuf) {
    io->cbuf = R_alloc (R_nc_length (io->ndim, io->xdim), sizeof (char));
  }
  UNPROTECT(1);
  return io->rxp;
}


static void
R_nc_char_strsxp (R_nc_buf *io)
{
  size_t ii, cnt, clen, rlen;
  char *thisstr, *endstr;
  if (io->ndim > 0) {
    /* Omit fastest-varying dimension from R character array */
    clen = io->xdim[(io->ndim)-1];
  } else if (io->ndim == 0) {
    /* Scalar character */
    clen = 1;
  } else {
    /* Single string */
    clen = io->xdim[0];
  }
  rlen = (clen <= RNC_CHARSXP_MAXLEN) ? clen : RNC_CHARSXP_MAXLEN;
  cnt = xlength (io->rxp);
  for (ii=0, thisstr=io->cbuf; ii<cnt; ii++, thisstr+=clen) {
    /* Check if string is null-terminated */
    endstr = memchr (thisstr, 0, rlen);
    if (!endstr) {
      SET_STRING_ELT (io->rxp, ii, mkCharLen (thisstr, rlen));
    } else {
      SET_STRING_ELT (io->rxp, ii, mkChar (thisstr));
    }
  }
}


static const char *
R_nc_raw_char (SEXP rarr, int ndim, const size_t *xdim)
{
  size_t cnt;
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rarr) < cnt) {
    error (RNC_EDATALEN);
  }
  return (const char *) RAW (rarr);
}


static SEXP
R_nc_char_raw_init (R_nc_buf *io)
{
  io->rxp = PROTECT(R_nc_allocArray (RAWSXP, io->ndim, io->xdim));
  io->rbuf = RAW (io->rxp);
  if (!io->cbuf) {
    io->cbuf = io->rbuf;
  }
  UNPROTECT(1);
  return io->rxp;
}


static void
R_nc_char_raw (R_nc_buf *io)
{
  if (io->cbuf != io->rbuf) {
    memcpy(io->rbuf, io->cbuf, xlength(io->rxp) * sizeof(char));
  }
  return;
}


static const char **
R_nc_strsxp_str (SEXP rstr, int ndim, const size_t *xdim)
{
  size_t ii, cnt;
  const char **cstr;
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rstr) < cnt) {
    error (RNC_EDATALEN);
  }
  cstr = (const char **) R_alloc (cnt, sizeof(size_t));
  for (ii=0; ii<cnt; ii++) {
    cstr[ii] = CHAR( STRING_ELT (rstr, ii));
  }
  return cstr;
}


static SEXP
R_nc_str_strsxp_init (R_nc_buf *io)
{
  io->rxp = PROTECT(R_nc_allocArray (STRSXP, io->ndim, io->xdim));
  if (!io->cbuf) {
    io->cbuf = R_alloc (xlength (io->rxp), sizeof(size_t));
  }
  UNPROTECT(1);
  return io->rxp;
}


static void
R_nc_str_strsxp (R_nc_buf *io)
{
  size_t ii, nchar, cnt;
  char **cstr;
  cnt = xlength (io->rxp);
  cstr = (char **) io->cbuf;
  for (ii=0; ii<cnt; ii++) {
    nchar = strlen (cstr[ii]);
    if (nchar > RNC_CHARSXP_MAXLEN) {
      /* Truncate excessively long strings while reading into R */
      SET_STRING_ELT (io->rxp, ii, mkCharLen (cstr[ii], RNC_CHARSXP_MAXLEN));
    } else if (nchar > 0) {
      SET_STRING_ELT (io->rxp, ii, mkChar (cstr[ii]));
    }
  }
  /* Free pointers to strings created by netcdf */
  if (cnt > 0) {
    R_nc_check (nc_free_string (cnt, io->cbuf));
  }
}


/*=============================================================================*\
 *  Numeric type conversions
\*=============================================================================*/



/* Convert numeric values from R to C format.
   Memory for the result is allocated if necessary (and freed by R).
   In special cases, the output is a pointer to the input data,
   so the output data should not be modified.
   An error is raised if any input values are outside the range of the output type.
 */





static const signed char*
R_nc_r2c_int_schar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const signed char *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  signed char fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (signed char *) R_alloc (cnt, sizeof(signed char));
  if (hasfill) {
    if (fillsize != sizeof(signed char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) SCHAR_MIN <= in[ii]) && (in[ii] <= (int) SCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) SCHAR_MIN <= in[ii]) && (in[ii] <= (int) SCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned char*
R_nc_r2c_int_uchar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned char *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  unsigned char fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned char *) R_alloc (cnt, sizeof(unsigned char));
  if (hasfill) {
    if (fillsize != sizeof(unsigned char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) 0 <= in[ii]) && (in[ii] <= (int) UCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) 0 <= in[ii]) && (in[ii] <= (int) UCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const short*
R_nc_r2c_int_short (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const short *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  short fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (short *) R_alloc (cnt, sizeof(short));
  if (hasfill) {
    if (fillsize != sizeof(short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) SHRT_MIN <= in[ii]) && (in[ii] <= (int) SHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) SHRT_MIN <= in[ii]) && (in[ii] <= (int) SHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned short*
R_nc_r2c_int_ushort (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned short *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  unsigned short fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned short *) R_alloc (cnt, sizeof(unsigned short));
  if (hasfill) {
    if (fillsize != sizeof(unsigned short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) 0 <= in[ii]) && (in[ii] <= (int) USHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) 0 <= in[ii]) && (in[ii] <= (int) USHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const int*
R_nc_r2c_int_int (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const int *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  int fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    out = (int *) R_alloc (cnt, sizeof(int));
  } else {
    out = (int *) INTEGER (rv);
    return out;
  }
  if (hasfill) {
    if (fillsize != sizeof(int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

static const unsigned int*
R_nc_r2c_int_uint (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned int *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  unsigned int fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned int *) R_alloc (cnt, sizeof(unsigned int));
  if (hasfill) {
    if (fillsize != sizeof(unsigned int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) 0 <= in[ii])) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) 0 <= in[ii])) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const long long*
R_nc_r2c_int_ll (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const long long *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  long long fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (long long *) R_alloc (cnt, sizeof(long long));
  if (hasfill) {
    if (fillsize != sizeof(long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

static const unsigned long long*
R_nc_r2c_int_ull (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned long long *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  unsigned long long fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned long long *) R_alloc (cnt, sizeof(unsigned long long));
  if (hasfill) {
    if (fillsize != sizeof(unsigned long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) 0 <= in[ii])) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) 0 <= in[ii])) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const float*
R_nc_r2c_int_float (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const float *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  float fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (float *) R_alloc (cnt, sizeof(float));
  if (hasfill) {
    if (fillsize != sizeof(float)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

static const double*
R_nc_r2c_int_dbl (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const double *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  double fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (double *) R_alloc (cnt, sizeof(double));
  if (hasfill) {
    if (fillsize != sizeof(double)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

/* Only convert non-negative values to size_t */
#if SIZEOF_INT > SIZEOF_SIZE_T
static const size_t*
R_nc_r2c_int_size (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const size_t *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  size_t fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (size_t *) R_alloc (cnt, sizeof(size_t));
  if (hasfill) {
    if (fillsize != sizeof(size_t)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) 0 <= in[ii]) && (in[ii] <= (int) SIZE_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) 0 <= in[ii]) && (in[ii] <= (int) SIZE_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

#else
static const size_t*
R_nc_r2c_int_size (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const size_t *fill)
{
  size_t ii, cnt, hasfill;
  const int *in;
  size_t fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (size_t *) R_alloc (cnt, sizeof(size_t));
  if (hasfill) {
    if (fillsize != sizeof(size_t)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else if (((int) 0 <= in[ii])) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((int) 0 <= in[ii])) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

#endif

static const signed char*
R_nc_r2c_dbl_schar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const signed char *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  signed char fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (signed char *) R_alloc (cnt, sizeof(signed char));
  if (hasfill) {
    if (fillsize != sizeof(signed char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) SCHAR_MIN <= in[ii]) && (in[ii] <= (double) SCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) SCHAR_MIN <= in[ii]) && (in[ii] <= (double) SCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned char*
R_nc_r2c_dbl_uchar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned char *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  unsigned char fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned char *) R_alloc (cnt, sizeof(unsigned char));
  if (hasfill) {
    if (fillsize != sizeof(unsigned char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) 0 <= in[ii]) && (in[ii] <= (double) UCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) 0 <= in[ii]) && (in[ii] <= (double) UCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const short*
R_nc_r2c_dbl_short (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const short *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  short fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (short *) R_alloc (cnt, sizeof(short));
  if (hasfill) {
    if (fillsize != sizeof(short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) SHRT_MIN <= in[ii]) && (in[ii] <= (double) SHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) SHRT_MIN <= in[ii]) && (in[ii] <= (double) SHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned short*
R_nc_r2c_dbl_ushort (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned short *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  unsigned short fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned short *) R_alloc (cnt, sizeof(unsigned short));
  if (hasfill) {
    if (fillsize != sizeof(unsigned short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) 0 <= in[ii]) && (in[ii] <= (double) USHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) 0 <= in[ii]) && (in[ii] <= (double) USHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const int*
R_nc_r2c_dbl_int (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const int *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  int fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (int *) R_alloc (cnt, sizeof(int));
  if (hasfill) {
    if (fillsize != sizeof(int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) INT_MIN <= in[ii]) && (in[ii] <= (double) INT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) INT_MIN <= in[ii]) && (in[ii] <= (double) INT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned int*
R_nc_r2c_dbl_uint (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned int *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  unsigned int fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned int *) R_alloc (cnt, sizeof(unsigned int));
  if (hasfill) {
    if (fillsize != sizeof(unsigned int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) 0 <= in[ii]) && (in[ii] <= (double) UINT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) 0 <= in[ii]) && (in[ii] <= (double) UINT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const long long*
R_nc_r2c_dbl_ll (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const long long *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  long long fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (long long *) R_alloc (cnt, sizeof(long long));
  if (hasfill) {
    if (fillsize != sizeof(long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) LLONG_MIN_DBL <= in[ii]) && (in[ii] <= (double) LLONG_MAX_DBL)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) LLONG_MIN_DBL <= in[ii]) && (in[ii] <= (double) LLONG_MAX_DBL)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned long long*
R_nc_r2c_dbl_ull (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned long long *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  unsigned long long fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned long long *) R_alloc (cnt, sizeof(unsigned long long));
  if (hasfill) {
    if (fillsize != sizeof(unsigned long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) 0 <= in[ii]) && (in[ii] <= (double) ULLONG_MAX_DBL)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) 0 <= in[ii]) && (in[ii] <= (double) ULLONG_MAX_DBL)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const float*
R_nc_r2c_dbl_float (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const float *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  float fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (float *) R_alloc (cnt, sizeof(float));
  if (hasfill) {
    if (fillsize != sizeof(float)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if ((!R_FINITE(in[ii])) || (((double) -FLT_MAX <= in[ii]) && (in[ii] <= (double) FLT_MAX))) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if ((!R_FINITE(in[ii])) || (((double) -FLT_MAX <= in[ii]) && (in[ii] <= (double) FLT_MAX))) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const double*
R_nc_r2c_dbl_dbl (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const double *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  double fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    out = (double *) R_alloc (cnt, sizeof(double));
  } else {
    out = (double *) REAL (rv);
    return out;
  }
  if (hasfill) {
    if (fillsize != sizeof(double)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

/* Only convert non-negative values to size_t */
static const size_t*
R_nc_r2c_dbl_size (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const size_t *fill)
{
  size_t ii, cnt, hasfill;
  const double *in;
  size_t fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (size_t *) R_alloc (cnt, sizeof(size_t));
  if (hasfill) {
    if (fillsize != sizeof(size_t)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else if (((double) 0 <= in[ii]) && (in[ii] <= (double) SIZE_MAX_DBL)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((double) 0 <= in[ii]) && (in[ii] <= (double) SIZE_MAX_DBL)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}


/* bit64 is treated by R as signed long long,
   but we may need to store unsigned long long,
   with very large positive values wrapping to negative values in R.
   We allow wrapping in reverse for conversion of bit64 to unsigned long long.
 */
static const signed char*
R_nc_r2c_bit64_schar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const signed char *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  signed char fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (signed char *) R_alloc (cnt, sizeof(signed char));
  if (hasfill) {
    if (fillsize != sizeof(signed char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else if (((long long) SCHAR_MIN <= in[ii]) && (in[ii] <= (long long) SCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((long long) SCHAR_MIN <= in[ii]) && (in[ii] <= (long long) SCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned char*
R_nc_r2c_bit64_uchar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned char *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  unsigned char fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned char *) R_alloc (cnt, sizeof(unsigned char));
  if (hasfill) {
    if (fillsize != sizeof(unsigned char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) UCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) UCHAR_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const short*
R_nc_r2c_bit64_short (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const short *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  short fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (short *) R_alloc (cnt, sizeof(short));
  if (hasfill) {
    if (fillsize != sizeof(short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else if (((long long) SHRT_MIN <= in[ii]) && (in[ii] <= (long long) SHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((long long) SHRT_MIN <= in[ii]) && (in[ii] <= (long long) SHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned short*
R_nc_r2c_bit64_ushort (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned short *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  unsigned short fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned short *) R_alloc (cnt, sizeof(unsigned short));
  if (hasfill) {
    if (fillsize != sizeof(unsigned short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) USHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) USHRT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const int*
R_nc_r2c_bit64_int (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const int *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  int fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (int *) R_alloc (cnt, sizeof(int));
  if (hasfill) {
    if (fillsize != sizeof(int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else if (((long long) INT_MIN <= in[ii]) && (in[ii] <= (long long) INT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((long long) INT_MIN <= in[ii]) && (in[ii] <= (long long) INT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const unsigned int*
R_nc_r2c_bit64_uint (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned int *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  unsigned int fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned int *) R_alloc (cnt, sizeof(unsigned int));
  if (hasfill) {
    if (fillsize != sizeof(unsigned int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) UINT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) UINT_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

static const long long*
R_nc_r2c_bit64_ll (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const long long *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  long long fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    out = (long long *) R_alloc (cnt, sizeof(long long));
  } else {
    out = (long long *) REAL (rv);
    return out;
  }
  if (hasfill) {
    if (fillsize != sizeof(long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

static const unsigned long long*
R_nc_r2c_bit64_ull (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned long long *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  unsigned long long fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (unsigned long long *) R_alloc (cnt, sizeof(unsigned long long));
  if (hasfill) {
    if (fillsize != sizeof(unsigned long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

static const float*
R_nc_r2c_bit64_float (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const float *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  float fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (float *) R_alloc (cnt, sizeof(float));
  if (hasfill) {
    if (fillsize != sizeof(float)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

static const double*
R_nc_r2c_bit64_dbl (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const double *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  double fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (double *) R_alloc (cnt, sizeof(double));
  if (hasfill) {
    if (fillsize != sizeof(double)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

#if SIZEOF_LONG_LONG > SIZEOF_SIZE_T
static const size_t*
R_nc_r2c_bit64_size (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const size_t *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  size_t fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (size_t *) R_alloc (cnt, sizeof(size_t));
  if (hasfill) {
    if (fillsize != sizeof(size_t)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) SIZE_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      if (((long long) 0 <= in[ii]) && (in[ii] <= (long long) SIZE_MAX)) {
        out[ii] = in[ii];
      } else {
        error (nc_strerror (NC_ERANGE));
      }
    }
  }
  return out;
}

#else
static const size_t*
R_nc_r2c_bit64_size (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const size_t *fill)
{
  size_t ii, cnt, hasfill;
  const long long *in;
  size_t fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  hasfill = (fill != NULL);
  out = (size_t *) R_alloc (cnt, sizeof(size_t));
  if (hasfill) {
    if (fillsize != sizeof(size_t)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        out[ii] = in[ii];
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        out[ii] = in[ii];
      }
    }
  }
  return out;
}

#endif


/* Convert numeric values from R to C format with packing.
   Memory for the result is allocated and freed by R.
   An error is raised if any packed values are outside the range of the output type.
 */




/* Define functions similar to those for conversions without packing,
 * noting that range checks are used before conversions from double to the output type.
 */
static const signed char*
R_nc_r2c_pack_int_schar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const signed char *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  signed char fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (signed char *) R_alloc (cnt, sizeof(signed char));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(signed char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SCHAR_MIN <= dpack) && (dpack <= (double) SCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SCHAR_MIN <= dpack) && (dpack <= (double) SCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned char*
R_nc_r2c_pack_int_uchar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned char *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  unsigned char fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned char *) R_alloc (cnt, sizeof(unsigned char));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const short*
R_nc_r2c_pack_int_short (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const short *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  short fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (short *) R_alloc (cnt, sizeof(short));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SHRT_MIN <= dpack) && (dpack <= (double) SHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SHRT_MIN <= dpack) && (dpack <= (double) SHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned short*
R_nc_r2c_pack_int_ushort (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned short *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  unsigned short fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned short *) R_alloc (cnt, sizeof(unsigned short));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) USHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) USHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const int*
R_nc_r2c_pack_int_int (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const int *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  int fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (int *) R_alloc (cnt, sizeof(int));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) INT_MIN <= dpack) && (dpack <= (double) INT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) INT_MIN <= dpack) && (dpack <= (double) INT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned int*
R_nc_r2c_pack_int_uint (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned int *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  unsigned int fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned int *) R_alloc (cnt, sizeof(unsigned int));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UINT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UINT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const long long*
R_nc_r2c_pack_int_ll (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const long long *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  long long fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (long long *) R_alloc (cnt, sizeof(long long));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) LLONG_MIN_DBL  <= dpack) && (dpack <= (double) LLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) LLONG_MIN_DBL  <= dpack) && (dpack <= (double) LLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned long long*
R_nc_r2c_pack_int_ull (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned long long *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  unsigned long long fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned long long *) R_alloc (cnt, sizeof(unsigned long long));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) ULLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) ULLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const float*
R_nc_r2c_pack_int_float (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const float *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  float fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (float *) R_alloc (cnt, sizeof(float));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(float)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if ((!R_FINITE(dpack)) || (((double) -FLT_MAX <= dpack) && (dpack <= (double) FLT_MAX))) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if ((!R_FINITE(dpack)) || (((double) -FLT_MAX <= dpack) && (dpack <= (double) FLT_MAX))) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const double*
R_nc_r2c_pack_int_dbl (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const double *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const int *in;
  double fillval=0, *out;
  in = (int *) INTEGER (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (double *) R_alloc (cnt, sizeof(double));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(double)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        out[ii] = dpack;
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        out[ii] = dpack;
      }
    }
  }
  return out;
}

static const signed char*
R_nc_r2c_pack_dbl_schar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const signed char *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  signed char fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (signed char *) R_alloc (cnt, sizeof(signed char));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(signed char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SCHAR_MIN <= dpack) && (dpack <= (double) SCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SCHAR_MIN <= dpack) && (dpack <= (double) SCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned char*
R_nc_r2c_pack_dbl_uchar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned char *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  unsigned char fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned char *) R_alloc (cnt, sizeof(unsigned char));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const short*
R_nc_r2c_pack_dbl_short (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const short *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  short fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (short *) R_alloc (cnt, sizeof(short));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SHRT_MIN <= dpack) && (dpack <= (double) SHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SHRT_MIN <= dpack) && (dpack <= (double) SHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned short*
R_nc_r2c_pack_dbl_ushort (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned short *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  unsigned short fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned short *) R_alloc (cnt, sizeof(unsigned short));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) USHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) USHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const int*
R_nc_r2c_pack_dbl_int (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const int *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  int fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (int *) R_alloc (cnt, sizeof(int));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) INT_MIN <= dpack) && (dpack <= (double) INT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) INT_MIN <= dpack) && (dpack <= (double) INT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned int*
R_nc_r2c_pack_dbl_uint (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned int *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  unsigned int fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned int *) R_alloc (cnt, sizeof(unsigned int));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UINT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UINT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const long long*
R_nc_r2c_pack_dbl_ll (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const long long *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  long long fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (long long *) R_alloc (cnt, sizeof(long long));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) LLONG_MIN_DBL <= dpack) && (dpack <= (double) LLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) LLONG_MIN_DBL <= dpack) && (dpack <= (double) LLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned long long*
R_nc_r2c_pack_dbl_ull (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned long long *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  unsigned long long fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned long long *) R_alloc (cnt, sizeof(unsigned long long));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) ULLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) ULLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const float*
R_nc_r2c_pack_dbl_float (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const float *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  float fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (float *) R_alloc (cnt, sizeof(float));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(float)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if ((!R_FINITE(dpack)) || (((double) -FLT_MAX <= dpack) && (dpack <= (double) FLT_MAX))) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if ((!R_FINITE(dpack)) || (((double) -FLT_MAX <= dpack) && (dpack <= (double) FLT_MAX))) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const double*
R_nc_r2c_pack_dbl_dbl (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const double *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const double *in;
  double fillval=0, *out;
  in = (double *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (double *) R_alloc (cnt, sizeof(double));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(double)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((ISNA(in[ii]))) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        out[ii] = dpack;
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        out[ii] = dpack;
      }
    }
  }
  return out;
}


/* bit64 is treated by R as signed long long,
   but we may need to store unsigned long long,
   with very large positive values wrapping to negative values in R.
   We allow wrapping in reverse for conversion of bit64 to unsigned long long.
 */
static const signed char*
R_nc_r2c_pack_bit64_schar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const signed char *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  signed char fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (signed char *) R_alloc (cnt, sizeof(signed char));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(signed char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SCHAR_MIN <= dpack) && (dpack <= (double) SCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SCHAR_MIN <= dpack) && (dpack <= (double) SCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned char*
R_nc_r2c_pack_bit64_uchar (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned char *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  unsigned char fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned char *) R_alloc (cnt, sizeof(unsigned char));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned char)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UCHAR_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const short*
R_nc_r2c_pack_bit64_short (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const short *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  short fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (short *) R_alloc (cnt, sizeof(short));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SHRT_MIN <= dpack) && (dpack <= (double) SHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) SHRT_MIN <= dpack) && (dpack <= (double) SHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned short*
R_nc_r2c_pack_bit64_ushort (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned short *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  unsigned short fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned short *) R_alloc (cnt, sizeof(unsigned short));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned short)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) USHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) USHRT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const int*
R_nc_r2c_pack_bit64_int (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const int *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  int fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (int *) R_alloc (cnt, sizeof(int));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) INT_MIN <= dpack) && (dpack <= (double) INT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) INT_MIN <= dpack) && (dpack <= (double) INT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned int*
R_nc_r2c_pack_bit64_uint (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned int *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  unsigned int fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned int *) R_alloc (cnt, sizeof(unsigned int));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned int)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UINT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) UINT_MAX)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const long long*
R_nc_r2c_pack_bit64_ll (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const long long *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  long long fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (long long *) R_alloc (cnt, sizeof(long long));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) LLONG_MIN_DBL <= dpack) && (dpack <= (double) LLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) LLONG_MIN_DBL <= dpack) && (dpack <= (double) LLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const unsigned long long*
R_nc_r2c_pack_bit64_ull (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const unsigned long long *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  unsigned long long fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (unsigned long long *) R_alloc (cnt, sizeof(unsigned long long));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(unsigned long long)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) ULLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if (((double) 0 <= dpack) && (dpack <= (double) ULLONG_MAX_DBL)) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const float*
R_nc_r2c_pack_bit64_float (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const float *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  float fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (float *) R_alloc (cnt, sizeof(float));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(float)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        if ((!R_FINITE(dpack)) || (((double) -FLT_MAX <= dpack) && (dpack <= (double) FLT_MAX))) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        if ((!R_FINITE(dpack)) || (((double) -FLT_MAX <= dpack) && (dpack <= (double) FLT_MAX))) {
          out[ii] = dpack;
        } else {
          error (nc_strerror (NC_ERANGE));
        }
      }
    }
  }
  return out;
}

static const double*
R_nc_r2c_pack_bit64_dbl (SEXP rv, int ndim, const size_t *xdim,
     size_t fillsize, const double *fill,
     const double *scale, const double *add)
{
  size_t ii, cnt, hasfill;
  double factor=1.0, offset=0.0, dpack;
  const long long *in;
  double fillval=0, *out;
  in = (long long *) REAL (rv);
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }
  out = (double *) R_alloc (cnt, sizeof(double));
  if (scale) {
    factor = *scale;
  }
  if (add) {
    offset = *add;
  }
  hasfill = (fill != NULL);
  if (hasfill) {
    if (fillsize != sizeof(double)) {
      error ("Size of fill value does not match output type");
    }
    fillval = *fill;
  }
  if (hasfill) {
    for (ii=0; ii<cnt; ii++) {
      if ((in[ii]==NA_INTEGER64)) {
        out[ii] = fillval;
      } else {
        dpack = round((in[ii] - offset) / factor);
        out[ii] = dpack;
      }
    }
  } else {
    for (ii=0; ii<cnt; ii++) {
      {
        dpack = round((in[ii] - offset) / factor);
        out[ii] = dpack;
      }
    }
  }
  return out;
}



/* Allocate memory for reading a netcdf variable slice
   and converting the results to an R variable.
   On input, the R_nc_buf structure contains dimensions of the buffer (ndim, *xdim).
   On output, the R_nc_buf structure contains an allocated SEXP and a pointer to its data.
 */

static SEXP
R_nc_c2r_int_init (R_nc_buf *io)
{
  io->rxp = PROTECT(R_nc_allocArray (INTSXP, io->ndim, io->xdim));
  io->rbuf = INTEGER (io->rxp);
  if (!io->cbuf) {
    io->cbuf = io->rbuf;
  }
  UNPROTECT(1);
  return io->rxp;
}

static SEXP
R_nc_c2r_dbl_init (R_nc_buf *io)
{
  io->rxp = PROTECT(R_nc_allocArray (REALSXP, io->ndim, io->xdim));
  io->rbuf = REAL (io->rxp);
  if (!io->cbuf) {
    io->cbuf = io->rbuf;
  }
  UNPROTECT(1);
  return io->rxp;
}

static SEXP
R_nc_c2r_bit64_init (R_nc_buf *io)
{
  io->rxp = PROTECT(R_nc_allocArray (REALSXP, io->ndim, io->xdim));
  io->rbuf = REAL (io->rxp);
  if (!io->cbuf) {
    io->cbuf = io->rbuf;
  }
  UNPROTECT(1);
  return io->rxp;
}



/* Convert numeric values from C to R format.
   Parameters and buffers for the conversion are passed via the R_nc_buf struct.
   The same buffer may be used for input and output.
   Output type may be larger (not smaller) than input,
   so convert in reverse order to avoid overwriting input with output.
   Fill values and values outside the valid range are set to missing,
   but NA or NaN values in floating point data are transferred to the output
   (because all comparisons with NA or NaN are false).
 */


static void
R_nc_c2r_schar_int (R_nc_buf *io)
{
  size_t ii;
  signed char fillval=0, minval=0, maxval=0, *in;
  int *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (signed char *) io->cbuf;
  out = (int *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(signed char)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((signed char *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((signed char *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((signed char *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_uchar_int (R_nc_buf *io)
{
  size_t ii;
  unsigned char fillval=0, minval=0, maxval=0, *in;
  int *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned char *) io->cbuf;
  out = (int *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(unsigned char)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned char *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned char *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned char *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_short_int (R_nc_buf *io)
{
  size_t ii;
  short fillval=0, minval=0, maxval=0, *in;
  int *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (short *) io->cbuf;
  out = (int *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(short)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((short *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((short *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((short *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_ushort_int (R_nc_buf *io)
{
  size_t ii;
  unsigned short fillval=0, minval=0, maxval=0, *in;
  int *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned short *) io->cbuf;
  out = (int *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(unsigned short)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned short *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned short *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned short *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_int_int (R_nc_buf *io)
{
  size_t ii;
  int fillval=0, minval=0, maxval=0, *in;
  int *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (int *) io->cbuf;
  out = (int *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(int)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((int *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((int *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((int *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_INTEGER;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}


static void
R_nc_c2r_schar_dbl (R_nc_buf *io)
{
  size_t ii;
  signed char fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (signed char *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(signed char)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((signed char *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((signed char *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((signed char *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_uchar_dbl (R_nc_buf *io)
{
  size_t ii;
  unsigned char fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned char *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(unsigned char)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned char *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned char *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned char *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_short_dbl (R_nc_buf *io)
{
  size_t ii;
  short fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (short *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(short)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((short *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((short *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((short *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_ushort_dbl (R_nc_buf *io)
{
  size_t ii;
  unsigned short fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned short *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(unsigned short)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned short *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned short *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned short *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_int_dbl (R_nc_buf *io)
{
  size_t ii;
  int fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (int *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(int)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((int *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((int *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((int *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_uint_dbl (R_nc_buf *io)
{
  size_t ii;
  unsigned int fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned int *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(unsigned int)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned int *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned int *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned int *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_float_dbl (R_nc_buf *io)
{
  size_t ii;
  float fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (float *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(float)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((float *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((float *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((float *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_dbl_dbl (R_nc_buf *io)
{
  size_t ii;
  double fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (double *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(double)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((double *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((double *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((double *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_int64_dbl (R_nc_buf *io)
{
  size_t ii;
  long long fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (long long *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(long long)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((long long *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((long long *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((long long *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_uint64_dbl (R_nc_buf *io)
{
  size_t ii;
  unsigned long long fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned long long *) io->cbuf;
  out = (double *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(unsigned long long)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned long long *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned long long *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned long long *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}


/* bit64 is treated by R as signed long long,
   but we may need to store unsigned long long,
   with very large positive values wrapping to negative values in R.
 */
static void
R_nc_c2r_int64_bit64 (R_nc_buf *io)
{
  size_t ii;
  long long fillval=0, minval=0, maxval=0, *in;
  long long *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (long long *) io->cbuf;
  out = (long long *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(long long)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((long long *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((long long *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((long long *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}

static void
R_nc_c2r_uint64_bit64 (R_nc_buf *io)
{
  size_t ii;
  unsigned long long fillval=0, minval=0, maxval=0, *in;
  long long *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned long long *) io->cbuf;
  out = (long long *) io->rbuf;
  if ((io->fill || io->min || io->max ) && io->fillsize != sizeof(unsigned long long)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned long long *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned long long *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned long long *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_INTEGER64;
          } else {
            out[ii] = in[ii];
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii];
        }
      }
    }
  }
}



/* Convert numeric values from C to R format with unpacking.
   Parameters and buffers for the conversion are passed via the R_nc_buf struct.
   Output type is assumed not to be smaller than input type,
   so the same buffer may be used for input and output
   by converting in reverse order.
   Fill values and values outside the valid range are set to missing,
   but NA or NaN values in floating point data are transferred to the output
   (because all comparisons with NA or NaN are false).
 */




static void
R_nc_c2r_unpack_schar (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  signed char fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (signed char *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(signed char)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((signed char *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((signed char *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((signed char *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_uchar (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  unsigned char fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned char *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(unsigned char)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned char *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned char *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned char *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_short (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  short fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (short *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(short)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((short *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((short *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((short *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_ushort (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  unsigned short fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned short *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(unsigned short)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned short *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned short *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned short *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_int (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  int fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (int *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(int)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((int *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((int *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((int *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_uint (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  unsigned int fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned int *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(unsigned int)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned int *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned int *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned int *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_float (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  float fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (float *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(float)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((float *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((float *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((float *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_dbl (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  double fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (double *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(double)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((double *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((double *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((double *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_int64 (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  long long fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (long long *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(long long)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((long long *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((long long *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((long long *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}

static void
R_nc_c2r_unpack_uint64 (R_nc_buf *io)
{
  size_t ii;
  double factor=1.0, offset=0.0;
  unsigned long long fillval=0, minval=0, maxval=0, *in;
  double *out;
  int hasfill, hasmin, hasmax;
  ii = xlength (io->rxp);
  in = (unsigned long long *) io->cbuf;
  out = (double *) io->rbuf;
  if (io->scale) {
    factor = *(io->scale);
  }
  if (io->add) {
    offset = *(io->add);
  }
  if ((io->fill || io->min || io->max) && io->fillsize != sizeof(unsigned long long)) {
    error ("Size of fill value does not match input type");
  }
  hasfill = (io->fill != NULL);
  if (hasfill) {
    fillval = *((unsigned long long *) io->fill);
  }
  hasmin = (io->min != NULL);
  if (hasmin) {
    minval = *((unsigned long long *) io->min);
  }
  hasmax = (io->max != NULL);
  if (hasmax) {
    maxval = *((unsigned long long *) io->max);
  }
  if (hasfill) {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval || (in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if (in[ii] == fillval || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if (in[ii] == fillval) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    }
  } else {
    if (hasmin) {
      if (hasmax) {
        while (ii-- > 0) {
          if ((in[ii] < minval) || (maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          if ((in[ii] < minval)) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      }
    } else {
      if (hasmax) {
        while (ii-- > 0) {
          if ((maxval < in[ii])) {
            out[ii] = NA_REAL;
          } else {
            out[ii] = in[ii] * factor + offset;
          }
        }
      } else {
        while (ii-- > 0) {
          out[ii] = in[ii] * factor + offset;
        }
      }
    }
  }
}



/*=============================================================================*\
 *  User-defined type conversions
\*=============================================================================*/

/* -- VLEN class -- */

/* Convert list of vectors from R to nc_vlen_t format.
   Memory for the result is allocated if necessary (and freed by R).
   In special cases, the output may point to the input data,
   so the output data should not be modified.
   An error is raised if input values cannot be converted to the vlen base type.
 */
static nc_vlen_t *
R_nc_vecsxp_vlen (SEXP rv, int ncid, nc_type xtype, int ndim, const size_t *xdim)
{
  size_t ii, cnt, len, size;
  int baseclass;
  nc_type basetype;
  nc_vlen_t *vbuf;
  SEXP item;

  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < cnt) {
    error (RNC_EDATALEN);
  }

  R_nc_check (nc_inq_user_type (ncid, xtype, NULL, NULL, &basetype, NULL, NULL));
  if (basetype > NC_MAX_ATOMIC_TYPE) {
    R_nc_check (nc_inq_user_type (ncid, basetype, NULL, &size, NULL, NULL, &baseclass));
  } else {
    baseclass = NC_NAT;
    size = 0;
  }

  vbuf = (nc_vlen_t *) R_alloc (cnt, sizeof(nc_vlen_t));
  for (ii=0; ii<cnt; ii++) {
    item = VECTOR_ELT(rv, ii);
    if (basetype == NC_CHAR && TYPEOF (item) == STRSXP) {
      if (xlength (item) > 0) {
        len = strlen (CHAR (STRING_ELT (item, 0)));
      } else {
        len = 0;
      }
    } else if (baseclass == NC_OPAQUE && TYPEOF (item) == RAWSXP) {
      len = xlength(item) / size;
    } else {
      len = xlength(item);
    }
    vbuf[ii].len = len;
    if (len > 0) {
      vbuf[ii].p = (void *) R_nc_r2c (item, ncid, basetype,
                                      -1, &len, 0, NULL, NULL, NULL);
    } else {
      vbuf[ii].p = NULL;
    }
  }
  return vbuf;
}


/* Allocate memory for reading a slice of a netcdf vlen variable
   and converting the results to an R variable.
   On input, the R_nc_buf structure contains dimensions of the buffer (ndim, *xdim).
   On output, the R_nc_buf structure contains an allocated R list (with dim attribute),
     and the C buffer is an array of pointers which are allocated by netcdf
     when reading from the variable (and which must be freed later by netcdf).
 */
static SEXP
R_nc_vlen_vecsxp_init (R_nc_buf *io)
{
  io->rxp = PROTECT(R_nc_allocArray (VECSXP, io->ndim, io->xdim));
  if (!io->cbuf) {
    io->cbuf = R_alloc (xlength (io->rxp), sizeof(nc_vlen_t));
  }
  UNPROTECT(1);
  return io->rxp;
}


/* Convert netcdf vlen array from C to R format.
   Parameters and buffers for the conversion are passed via the R_nc_buf struct.
   On input, the C data is stored in io->cbuf.
   On output, the R data is copied to io->rxp, and memory used by netcdf is freed.
 */
static void
R_nc_vlen_vecsxp (R_nc_buf *io)
{
  size_t ii, cnt;
  nc_type basetype;
  nc_vlen_t *vbuf;
  R_nc_buf tmpio;
  SEXP tmprxp;

  vbuf = io->cbuf;
  cnt = xlength (io->rxp);
  R_nc_check (nc_inq_user_type (io->ncid, io->xtype, NULL, NULL, &basetype, NULL, NULL));

  for (ii=0; ii<cnt; ii++) {
    tmprxp = PROTECT(R_nc_c2r_init (&tmpio, &(vbuf[ii].p), io->ncid, basetype, -1,
                       &(vbuf[ii].len), io->rawchar, io->fitnum,
                       0, NULL, NULL, NULL, NULL, NULL));
    R_nc_c2r (&tmpio);
    SET_VECTOR_ELT (io->rxp, ii, tmprxp);
    nc_free_vlen(&(vbuf[ii]));
    UNPROTECT(1);
  }
}


/* -- Opaque class -- */


/* Convert raw array from R to netcdf opaque type.
   Memory for the result is allocated if necessary (and freed by R).
   In special cases, the output may point to the input data,
   so the output data should not be modified.
 */
static const char *
R_nc_raw_opaque (SEXP rv, int ncid, nc_type xtype, int ndim, const size_t *xdim)
{
  size_t cnt, size;
  R_nc_check (nc_inq_user_type (ncid, xtype, NULL, &size, NULL, NULL, NULL));
  cnt = R_nc_length (ndim, xdim);
  if ((size_t) xlength (rv) < (cnt * size)) {
    error (RNC_EDATALEN);
  }
  return (const char *) RAW (rv);
}


static SEXP
R_nc_opaque_raw_init (R_nc_buf *io)
{
  int ndim;
  size_t *xdim, size;

  /* Fastest varying dimension of R array contains bytes of opaque data */
  R_nc_check (nc_inq_user_type (io->ncid, io->xtype, NULL, &size, NULL, NULL, NULL));

  ndim = io->ndim;
  if (ndim < 0) {
    /* Special case for an R vector without dimension attribute,
       but dimensions are needed to select opaque elements of a vector
     */
    ndim = 1;
  }
  xdim = (size_t *) R_alloc (ndim + 1, sizeof(size_t));
  if (ndim != 0) {
    /* Scalar has no dimensions to copy */
    memcpy (xdim, io->xdim, ndim * sizeof(size_t));
  }
  xdim[ndim] = size;

  io->rxp = PROTECT(R_nc_allocArray (RAWSXP, ndim + 1, xdim));
  io->rbuf = RAW (io->rxp);
  if (!io->cbuf) {
    io->cbuf = io->rbuf;
  }
  UNPROTECT(1);
  return io->rxp;
}


static void
R_nc_opaque_raw (R_nc_buf *io)
{
  if (io->cbuf != io->rbuf) {
    memcpy(io->rbuf, io->cbuf, xlength(io->rxp) * sizeof(char));
  }
  return;
}


/* -- Enum class -- */


/* Convert factor array from R to netcdf enum type.
   Memory for the result is allocated if necessary (and freed by R).
 */
static void *
R_nc_factor_enum (SEXP rv, int ncid, nc_type xtype, int ndim, const size_t *xdim)
{
  SEXP levels;
  size_t size, imem, nmem, ilev, nlev, *ilev2mem, ifac, nfac, cnt;
  char *memnames, *memname, *memvals, *memval, *out;
  const char **levnames;
  int ismatch, *in, inval;

  /* Extract indices and level names of R factor */
  in = INTEGER (rv);

  levels = getAttrib (rv, R_LevelsSymbol);
  if (!isString (levels)) {
    error ("Expected character vector for levels of factor array");
  }

  nlev = xlength (levels);

  levnames = (const char **) R_alloc (nlev, sizeof(size_t));

  for (ilev=0; ilev<nlev; ilev++) {
    levnames[ilev] = CHAR( STRING_ELT (levels, ilev));
  }

  /* Read values and names of enum members */
  R_nc_check (nc_inq_enum(ncid, xtype, NULL, NULL, &size, &nmem));

  memnames = R_alloc (nmem, NC_MAX_NAME+1);
  memvals = R_alloc (nmem, size);

  for (imem=0, memname=memnames, memval=memvals; imem<nmem;
       imem++, memname+=(NC_MAX_NAME+1), memval+=size) {
    R_nc_check (nc_inq_enum_member (ncid, xtype, imem, memname, memval));
  }

  /* Find enum member for each R level */
  ilev2mem = (size_t *) R_alloc (nlev, sizeof(size_t));

  for (ilev=0; ilev<nlev; ilev++) {
    ismatch = 0;
    for (imem=0, memname=memnames; imem<nmem;
         imem++, memname+=(NC_MAX_NAME+1)) {
      if (strcmp(memname, levnames[ilev]) == 0) {
        ismatch = 1;
        ilev2mem[ilev] = imem;
        break;
      }
    }
    if (!ismatch) {
      error ("Level has no matching member in enum type");
    }
  }

  /* Convert factor indices to enum values */
  nfac = xlength (rv);
  cnt = R_nc_length (ndim, xdim);
  if (nfac < cnt) {
    error (RNC_EDATALEN);
  }
  out = R_alloc (nfac, size);

  for (ifac=0; ifac<nfac; ifac++) {
    inval = in[ifac];
    if (0 < inval && (size_t) inval <= nlev) {
      imem = ilev2mem[inval-1];
      memcpy(out + ifac*size, memvals + imem*size, size);
    } else {
      error ("Invalid index in factor");
    }
  }

  return out;
}


static SEXP
R_nc_enum_factor_init (R_nc_buf *io)
{
  size_t size;
  io->rxp = PROTECT(R_nc_allocArray (INTSXP, io->ndim, io->xdim));
  io->rbuf = INTEGER (io->rxp);
  if (!io->cbuf) {
    R_nc_check (nc_inq_type (io->ncid, io->xtype, NULL, &size));
    io->cbuf = R_alloc (xlength (io->rxp), size);
  }
  UNPROTECT(1);
  return io->rxp;
}


/* Convert specified number of bytes to an R symbol,
   as required to store and retrieve values from a hashed environment.
   The work array must have minimum size 2*size+2 bytes.
 */
static SEXP
R_nc_char_symbol (char *in, size_t size, char *work)
{
  size_t ii;
  work[0]='X';
  for (ii=0; ii<size; ii++) {
    sprintf(work+1+ii*2, "%02X", in[ii]);
  }
  work[2*size+1]='\0';
  return install(work);
}


/* Convert netcdf enum values in io->cbuf to R factor array in io->rbuf.
   Memory for the result must be pre-allocated by R_nc_enum_factor_init.
 */
static void
R_nc_enum_factor (R_nc_buf *io)
{
  SEXP levels, env, cmd, symbol, index;
  size_t size, nmem, ifac, nfac;
  char *memname, *memval, *work, *inval;
  int ncid, imem, imemmax, *out;
  nc_type xtype;

  /* Get size and number of enum members */
  ncid = io->ncid;
  xtype = io->xtype;
  R_nc_check (nc_inq_enum(ncid, xtype, NULL, NULL, &size, &nmem));

  /* Set attributes for R factor */
  levels = PROTECT(R_nc_allocArray (STRSXP, -1, &nmem));
  setAttrib(io->rxp, R_LevelsSymbol, levels);
  setAttrib(io->rxp, R_ClassSymbol, mkString("factor"));

  /* Create a hashed environment for value-index pairs.
     Members inherit PROTECTion from the env.
   */
  cmd = PROTECT(lang1 (install ("new.env")));
  env = PROTECT(eval (cmd, R_BaseEnv));

  /* Read values and names of netcdf enum members.
     Store names as R factor levels.
     Store values and their R indices (1-based) in hashed environment.
   */
  memname = R_alloc (nmem, NC_MAX_NAME+1);
  memval = R_alloc (1, size);
  work = R_alloc (2*size+2, 1);

  imemmax = nmem; // netcdf member index is int
  for (imem=0; imem<imemmax; imem++) {
    R_nc_check (nc_inq_enum_member (ncid, xtype, imem, memname, memval));
    SET_STRING_ELT (levels, imem, mkChar (memname));
    symbol = PROTECT (R_nc_char_symbol (memval, size, work));
    index = PROTECT (ScalarInteger (imem+1));
    defineVar (symbol, index, env);
    UNPROTECT(2);
  }

  /* Convert netcdf enum values to R indices.
     Use hashed environment prepared above for efficient lookups.
   */
  nfac = xlength (io->rxp);

  out = io->rbuf;
  for (ifac=0, inval=io->cbuf; ifac<nfac; ifac++, inval+=size) {
    symbol = PROTECT(R_nc_char_symbol (inval, size, work));
    index = findVarInFrame3 (env, symbol, TRUE);
    UNPROTECT(1);
    if (index == R_UnboundValue) {
      error ("Unknown enum value in variable");
    } else {
      out[ifac] = INTEGER (index)[0];
    }
  }

  /* Allow garbage collection of env and levels */
  UNPROTECT(3);
}


/* -- Compound class -- */

/* Convert list of arrays from R to netcdf compound type.
   Memory for the result is allocated (and freed by R).
 */
static void *
R_nc_vecsxp_compound (SEXP rv, int ncid, nc_type xtype, int ndim, const size_t *xdim)
{
  size_t cnt, size, nfld, offset, fldsize, fldcnt, fldlen,
         nlist, ilist, ielem, *dimsizefld;
  nc_type typefld;
  int ifldmax, ifld, idimfld, ndimfld, *dimlenfld, ismatch;
  char *bufout, namefld[NC_MAX_NAME+1];
  const char *buffld;
  void *highwater;
  SEXP namelist;

  /* Get size and number of fields in compound type */
  R_nc_check (nc_inq_compound(ncid, xtype, NULL, &size, &nfld));

  /* Check names attribute of R list */
  namelist = PROTECT(getAttrib (rv, R_NamesSymbol));
  if (!isString (namelist)) {
    error ("Named list required for conversion to compound type");
  }
  nlist = xlength (namelist);
  if (nlist < nfld) {
    error ("Not enough fields in list for conversion to compound type");
  }

  /* Allocate memory for compound array,
     filling with zeros so that valgrind does not complain about
     uninitialised values in gaps inserted for alignment */
  cnt = R_nc_length (ndim, xdim);
  bufout = R_alloc (cnt, size);
  memset(bufout, 0, cnt*size);

  /* Convert each field in turn */
  ifldmax = nfld;
  for (ifld=0; ifld<ifldmax; ifld++) {

    /* Save memory "highwater mark" to reclaim memory from R_alloc,
       which may consume large chunks of memory after R_nc_r2c.
     */
    highwater = vmaxget();

    /* Query the dataset for details of the field. */
    R_nc_check (nc_inq_compound_field (ncid, xtype, ifld, namefld,
                  &offset, &typefld, &ndimfld, NULL));
    dimlenfld = (int *) R_alloc (ndimfld, sizeof(int));
    R_nc_check (nc_inq_compound_fielddim_sizes(ncid, xtype, ifld, dimlenfld));
    R_nc_check (nc_inq_type (ncid, typefld, NULL, &fldsize));

    /* Find the field by name in the R input list */
    ismatch = 0;
    for (ilist=0; ilist<nlist; ilist++) {
      if (strcmp (CHAR (STRING_ELT (namelist, ilist)), namefld) == 0) {
        // ilist is the matching list index
        ismatch = 1;
        break;
      }
    }
    if (!ismatch) {
      error ("Name of compound field not found in input list");
    }

    /* Convert the field from R to C.
       Convert the dimension lengths from integer to size_t,
       adding an extra dimension (slowest varying) for the total number
       of elements in the compound array (cnt). */
    dimsizefld = (size_t *) R_alloc (ndimfld+1, sizeof(size_t));
    dimsizefld[0] = cnt;
    for (idimfld=0; idimfld<ndimfld; idimfld++) {
      dimsizefld[idimfld+1] = dimlenfld[idimfld];
    }
    buffld = R_nc_r2c (VECTOR_ELT (rv, ilist), ncid, typefld, ndimfld+1, dimsizefld,
                       0, NULL, NULL, NULL);

    /* Copy elements from the field array into the compound array */
    fldcnt = R_nc_length (ndimfld, dimsizefld+1);
    fldlen = fldsize * fldcnt;
    for (ielem=0; ielem<cnt; ielem++) {
      memcpy (bufout+ielem*size+offset, buffld+ielem*fldlen, fldlen);
    }

    /* Allow memory from R_alloc since vmaxget to be reclaimed */
    vmaxset (highwater);
  }

  UNPROTECT(1);
  return bufout;
}


static SEXP
R_nc_compound_vecsxp_init (R_nc_buf *io)
{
  size_t size, nfld, cnt;

  /* The in-memory layout of compound types can differ between writing and reading.
     When writing, an arbitrary layout is specified by the user.
     When reading, a "native" layout is returned by the netcdf library.
     If the layout has been defined since the dataset was opened,
     the type inquiry functions always return the layout used for writing.
     We can get the layout for reading from a read-only instance of the dataset.
   */
  if (R_nc_redef (io->ncid) == NC_NOERR) {
    /* Dataset must be writable because it is now in define mode */
    error ("Please read compound type from a read-only dataset");
  }

  /* Get number of fields in compound type */
  R_nc_check (nc_inq_compound(io->ncid, io->xtype, NULL, &size, &nfld));

  /* Allocate memory for output list */
  io->rxp = PROTECT(R_nc_allocArray (VECSXP, -1, &nfld));

  /* Allocate memory for compound array */
  if (!io->cbuf) {
    cnt = R_nc_length (io->ndim, io->xdim);
    io->cbuf = R_alloc (cnt, size);
  }

  UNPROTECT(1);
  return io->rxp;
}


/* Convert netcdf compound values in io->cbuf to R list of arrays in io->rxp.
   Data structures are prepared by a prior call to R_nc_compound_vecsxp_init.
 */
static void
R_nc_compound_vecsxp (R_nc_buf *io)
{
  int ncid, ifld, ifldmax, idim, ndim, idimfld, ndimfld, *dimlenfld, ndimslice;
  nc_type xtype, typefld;
  size_t size, nfld, cnt, offset, fldsize, *dimslice, fldcnt, fldlen, ielem;
  SEXP namelist, rxpfld;
  char namefld[NC_MAX_NAME+1], *buffld, *bufcmp;
  R_nc_buf iofld;
  void *highwater;

  /* Get size and number of fields in compound type */
  ncid = io->ncid;
  xtype = io->xtype;
  R_nc_check (nc_inq_compound(ncid, xtype, NULL, &size, &nfld));
  cnt = R_nc_length (io->ndim, io->xdim);

  /* Set names attribute of R list */
  namelist = PROTECT(R_nc_allocArray (STRSXP, -1, &nfld));
  setAttrib(io->rxp, R_NamesSymbol, namelist);

  /* Convert each field in turn */
  bufcmp = io->cbuf;
  ifldmax = nfld;
  for (ifld=0; ifld<ifldmax; ifld++) {

    /* Save memory "highwater mark" to reclaim memory from R_alloc,
       which may consume large chunks of memory after R_nc_r2c.
     */
    highwater = vmaxget();

    /* Query the dataset for details of the field. */
    R_nc_check (nc_inq_compound_field (ncid, xtype, ifld, namefld,
                  &offset, &typefld, &ndimfld, NULL));
    dimlenfld = (int *) R_alloc (ndimfld, sizeof(int));
    R_nc_check (nc_inq_compound_fielddim_sizes(ncid, xtype, ifld, dimlenfld));
    R_nc_check (nc_inq_type (ncid, typefld, NULL, &fldsize));

    /* Set the field name in the R list */
    SET_STRING_ELT (namelist, ifld, mkChar (namefld));

    /* Append field dimensions to the variable dimensions */
    ndim = io->ndim;
    if (ndim < 0) {
      /* Special case to drop dimensions of a vector,
         but dimensions are needed for a compound vector.
       */
      ndim = 1;
    }

    ndim = io->ndim < 0 ? 1 : io->ndim ;
    ndimslice = ndim + ndimfld;
    dimslice = (size_t *) R_alloc (ndimslice, sizeof(size_t));
    for (idim=0; idim<ndim; idim++) {
      dimslice[idim] = io->xdim[idim];
    }
    for (idimfld=0; idimfld<ndimfld; idimfld++) {
      dimslice[ndim+idimfld] = dimlenfld[idimfld];
    }
    fldcnt = R_nc_length (ndimfld, dimslice+ndim);

    /* Prepare to convert field data from C to R */
    buffld = NULL;
    rxpfld = PROTECT(R_nc_c2r_init (&iofld, (void **) &buffld, ncid, typefld,
               ndimslice, dimslice, io->rawchar, io->fitnum,
               0, NULL, NULL, NULL, NULL, NULL));

    /* Copy elements from the compound array into the field array */
    fldlen = fldsize * fldcnt;
    for (ielem=0; ielem<cnt; ielem++) {
      memcpy (buffld+ielem*fldlen, bufcmp+ielem*size+offset, fldlen);
    }

    /* Convert field data from C to R */
    R_nc_c2r (&iofld);

    /* Insert field data into R list */
    SET_VECTOR_ELT (io->rxp, ifld, rxpfld);

    /* Allow memory from R_alloc since vmaxget to be reclaimed */
    UNPROTECT(1);
    vmaxset (highwater);
  }
  UNPROTECT(1);
}


/*=============================================================================*\
 *  Generic type conversions
\*=============================================================================*/

const void *
R_nc_r2c (SEXP rv, int ncid, nc_type xtype, int ndim, const size_t *xdim,
          size_t fillsize, const void *fill,
          const double *scale, const double *add)
{
  int pack, class;

  pack = (scale || add);

  if (xtype > NC_MAX_ATOMIC_TYPE) {
    R_nc_check (nc_inq_user_type (ncid, xtype, NULL, NULL, NULL, NULL, &class));
  }

  switch (TYPEOF(rv)) {
  case INTSXP:
    if (pack) {
      switch (xtype) {
        case NC_BYTE:
          return R_nc_r2c_pack_int_schar (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UBYTE:
          return R_nc_r2c_pack_int_uchar (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_SHORT:
          return R_nc_r2c_pack_int_short (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_USHORT:
          return R_nc_r2c_pack_int_ushort (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_INT:
          return R_nc_r2c_pack_int_int (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UINT:
          return R_nc_r2c_pack_int_uint (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_INT64:
          return R_nc_r2c_pack_int_ll (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UINT64:
          return R_nc_r2c_pack_int_ull (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_FLOAT:
          return R_nc_r2c_pack_int_float (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_DOUBLE:
          return R_nc_r2c_pack_int_dbl (rv, ndim, xdim, fillsize, fill, scale, add);
      }
    } else {
      switch (xtype) {
      case NC_BYTE:
        return R_nc_r2c_int_schar (rv, ndim, xdim, fillsize, fill);
      case NC_UBYTE:
        return R_nc_r2c_int_uchar (rv, ndim, xdim, fillsize, fill);
      case NC_SHORT:
        return R_nc_r2c_int_short (rv, ndim, xdim, fillsize, fill);
      case NC_USHORT:
        return R_nc_r2c_int_ushort (rv, ndim, xdim, fillsize, fill);
      case NC_INT:
        return R_nc_r2c_int_int (rv, ndim, xdim, fillsize, fill);
      case NC_UINT:
        return R_nc_r2c_int_uint (rv, ndim, xdim, fillsize, fill);
      case NC_INT64:
        return R_nc_r2c_int_ll (rv, ndim, xdim, fillsize, fill);
      case NC_UINT64:
        return R_nc_r2c_int_ull (rv, ndim, xdim, fillsize, fill);
      case NC_FLOAT:
        return R_nc_r2c_int_float (rv, ndim, xdim, fillsize, fill);
      case NC_DOUBLE:
        return R_nc_r2c_int_dbl (rv, ndim, xdim, fillsize, fill);
      }
    }
    if (xtype > NC_MAX_ATOMIC_TYPE &&
        class == NC_ENUM &&
        R_nc_inherits (rv, "factor")) {
      return R_nc_factor_enum (rv, ncid, xtype, ndim, xdim);
    }
    break;
  case REALSXP:
    if (pack) {
      if (R_nc_inherits (rv, "integer64")) {
        switch (xtype) {
        case NC_BYTE:
          return R_nc_r2c_pack_bit64_schar (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UBYTE:
          return R_nc_r2c_pack_bit64_uchar (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_SHORT:
          return R_nc_r2c_pack_bit64_short (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_USHORT:
          return R_nc_r2c_pack_bit64_ushort (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_INT:
          return R_nc_r2c_pack_bit64_int (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UINT:
          return R_nc_r2c_pack_bit64_uint (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_INT64:
          return R_nc_r2c_pack_bit64_ll (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UINT64:
          return R_nc_r2c_pack_bit64_ull (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_FLOAT:
          return R_nc_r2c_pack_bit64_float (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_DOUBLE:
          return R_nc_r2c_pack_bit64_dbl (rv, ndim, xdim, fillsize, fill, scale, add);
        }
      } else {
        switch (xtype) {
        case NC_BYTE:
          return R_nc_r2c_pack_dbl_schar (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UBYTE:
          return R_nc_r2c_pack_dbl_uchar (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_SHORT:
          return R_nc_r2c_pack_dbl_short (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_USHORT:
          return R_nc_r2c_pack_dbl_ushort (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_INT:
          return R_nc_r2c_pack_dbl_int (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UINT:
          return R_nc_r2c_pack_dbl_uint (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_INT64:
          return R_nc_r2c_pack_dbl_ll (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_UINT64:
          return R_nc_r2c_pack_dbl_ull (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_FLOAT:
          return R_nc_r2c_pack_dbl_float (rv, ndim, xdim, fillsize, fill, scale, add);
        case NC_DOUBLE:
          return R_nc_r2c_pack_dbl_dbl (rv, ndim, xdim, fillsize, fill, scale, add);
        }
      }
    } else {
      if (R_nc_inherits (rv, "integer64")) {
        switch (xtype) {
        case NC_BYTE:
          return R_nc_r2c_bit64_schar (rv, ndim, xdim, fillsize, fill);
        case NC_UBYTE:
          return R_nc_r2c_bit64_uchar (rv, ndim, xdim, fillsize, fill);
        case NC_SHORT:
          return R_nc_r2c_bit64_short (rv, ndim, xdim, fillsize, fill);
        case NC_USHORT:
          return R_nc_r2c_bit64_ushort (rv, ndim, xdim, fillsize, fill);
        case NC_INT:
          return R_nc_r2c_bit64_int (rv, ndim, xdim, fillsize, fill);
        case NC_UINT:
          return R_nc_r2c_bit64_uint (rv, ndim, xdim, fillsize, fill);
        case NC_INT64:
          return R_nc_r2c_bit64_ll (rv, ndim, xdim, fillsize, fill);
        case NC_UINT64:
          return R_nc_r2c_bit64_ull (rv, ndim, xdim, fillsize, fill);
        case NC_FLOAT:
          return R_nc_r2c_bit64_float (rv, ndim, xdim, fillsize, fill);
        case NC_DOUBLE:
          return R_nc_r2c_bit64_dbl (rv, ndim, xdim, fillsize, fill);
        }
      } else {
        switch (xtype) {
        case NC_BYTE:
          return R_nc_r2c_dbl_schar (rv, ndim, xdim, fillsize, fill);
        case NC_UBYTE:
          return R_nc_r2c_dbl_uchar (rv, ndim, xdim, fillsize, fill);
        case NC_SHORT:
          return R_nc_r2c_dbl_short (rv, ndim, xdim, fillsize, fill);
        case NC_USHORT:
          return R_nc_r2c_dbl_ushort (rv, ndim, xdim, fillsize, fill);
        case NC_INT:
          return R_nc_r2c_dbl_int (rv, ndim, xdim, fillsize, fill);
        case NC_UINT:
          return R_nc_r2c_dbl_uint (rv, ndim, xdim, fillsize, fill);
        case NC_INT64:
          return R_nc_r2c_dbl_ll (rv, ndim, xdim, fillsize, fill);
        case NC_UINT64:
          return R_nc_r2c_dbl_ull (rv, ndim, xdim, fillsize, fill);
        case NC_FLOAT:
          return R_nc_r2c_dbl_float (rv, ndim, xdim, fillsize, fill);
        case NC_DOUBLE:
          return R_nc_r2c_dbl_dbl (rv, ndim, xdim, fillsize, fill);
        }
      }
    }
    break;
  case STRSXP:
    switch (xtype) {
    case NC_CHAR:
      return R_nc_strsxp_char (rv, ndim, xdim);
    case NC_STRING:
      return R_nc_strsxp_str (rv, ndim, xdim);
    }
    break;
  case RAWSXP:
    if (xtype == NC_CHAR) {
      return R_nc_raw_char (rv, ndim, xdim);
    } else if (xtype > NC_MAX_ATOMIC_TYPE && class == NC_OPAQUE) {
      return R_nc_raw_opaque (rv, ncid, xtype, ndim, xdim);
    }
    break;
  case VECSXP:
    if (xtype > NC_MAX_ATOMIC_TYPE) {
      switch (class) {
      case NC_VLEN:
        return R_nc_vecsxp_vlen (rv, ncid, xtype, ndim, xdim);
      case NC_COMPOUND:
        return R_nc_vecsxp_compound (rv, ncid, xtype, ndim, xdim);
      }
    }
    break;
  }
  error (RNC_EDATATYPE);
}

SEXP \
R_nc_c2r_init (R_nc_buf *io, void **cbuf,
               int ncid, nc_type xtype, int ndim, const size_t *xdim,
               int rawchar, int fitnum, size_t fillsize,
               const void *fill, const void *min, const void *max,
               const double *scale, const double *add)
{
  int class;

  if (!io) {
    error ("Pointer to R_nc_buf must not be NULL in R_nc_c2r_init");
  }

  /* Initialise the R_nc_buf, making copies of pointer arguments */
  io->rxp = NULL;
  io->cbuf = NULL;
  io->rbuf = NULL;
  io->xtype = xtype;
  io->ncid = ncid;
  io->ndim = ndim;
  io->rawchar = rawchar;
  io->fitnum = fitnum;
  io->xdim = NULL;
  io->fillsize = fillsize;
  io->fill = NULL;
  io->min = NULL;
  io->max = NULL;
  io->scale = NULL;
  io->add = NULL;

  if (cbuf) {
    io->cbuf = *cbuf;
  }

  if (xdim) {
    if (ndim > 0) {
      io->xdim = (size_t *) R_alloc (ndim, sizeof(size_t));
      memcpy (io->xdim, xdim, ndim*sizeof(size_t));
    } else if (ndim < 0) {
      /* Special case for vector without dim attribute */
      io->xdim = (size_t *) R_alloc (1, sizeof(size_t));
      memcpy (io->xdim, xdim, sizeof(size_t));
    }
    /* Scalar has no dimensions */
  }

  if (fill) {
    io->fill = R_alloc (1, fillsize);
    memcpy (io->fill, fill, fillsize);
  }

  if (min) {
    io->min = R_alloc (1, fillsize);
    memcpy (io->min, min, fillsize);
  }

  if (max) {
    io->max = R_alloc (1, fillsize);
    memcpy (io->max, max, fillsize);
  }

  if (scale) {
    io->scale = (double *) R_alloc (1, sizeof(double));
    *(io->scale) = *scale;
  }

  if (add) {
    io->add = (double *) R_alloc (1, sizeof(double));
    *(io->add) = *add;
  }

  /* Prepare buffers */ 
  switch (xtype) {
    case NC_BYTE:
    case NC_UBYTE:
    case NC_SHORT:
    case NC_USHORT:
    case NC_INT:
      if (fitnum && !scale && !add) {
        PROTECT(R_nc_c2r_int_init (io));
        break;
      }
    case NC_INT64:
    case NC_UINT64:
      if (fitnum && !scale && !add) {
        PROTECT(R_nc_c2r_bit64_init (io));
        classgets(io->rxp, mkString("integer64"));
        break;
      }
    case NC_UINT:
    case NC_FLOAT:
    case NC_DOUBLE:
      PROTECT(R_nc_c2r_dbl_init (io));
      break;
    case NC_CHAR:
      if (rawchar) {
        PROTECT(R_nc_char_raw_init (io));
      } else {
        PROTECT(R_nc_char_strsxp_init (io));
      }
      break;
    case NC_STRING:
      PROTECT(R_nc_str_strsxp_init (io));
      break;
    default:
      if (xtype > NC_MAX_ATOMIC_TYPE) {
        R_nc_check (nc_inq_user_type (ncid, xtype, NULL, NULL, NULL, NULL, &class));
        switch (class) {
        case NC_COMPOUND:
          PROTECT(R_nc_compound_vecsxp_init (io));
          break;
        case NC_ENUM:
          PROTECT(R_nc_enum_factor_init (io));
          break;
        case NC_VLEN:
          PROTECT(R_nc_vlen_vecsxp_init (io));
          break;
        case NC_OPAQUE:
          PROTECT(R_nc_opaque_raw_init (io));
          break;
        default:
          error (RNC_ETYPEDROP);
        }
      } else {
        error (RNC_ETYPEDROP);
      }
  }

  if (cbuf) {
    *cbuf = io->cbuf;
  }
  UNPROTECT(1);
  return io->rxp;
}


void
R_nc_c2r (R_nc_buf *io)
{
  int unpack, class;

  unpack = (io->scale || io->add);

  /* Type conversions */
  switch (io->xtype) {
    case NC_BYTE:
      if (unpack) {
        R_nc_c2r_unpack_schar (io);
      } else if (io->fitnum) {
        R_nc_c2r_schar_int (io);
      } else {
        R_nc_c2r_schar_dbl (io);
      }
      break;
    case NC_UBYTE:
      if (unpack) {
        R_nc_c2r_unpack_uchar (io);
      } else if (io->fitnum) {
        R_nc_c2r_uchar_int (io);
      } else {
        R_nc_c2r_uchar_dbl (io);
      }
      break;
    case NC_SHORT:
      if (unpack) {
        R_nc_c2r_unpack_short (io);
      } else if (io->fitnum) {
        R_nc_c2r_short_int (io);
      } else {
        R_nc_c2r_short_dbl (io);
      }
      break;
    case NC_USHORT:
      if (unpack) {
        R_nc_c2r_unpack_ushort (io);
      } else if (io->fitnum) {
        R_nc_c2r_ushort_int (io);
      } else {
        R_nc_c2r_ushort_dbl (io);
      }
      break;
    case NC_INT:
      if (unpack) {
        R_nc_c2r_unpack_int (io);
      } else if (io->fitnum) {
        R_nc_c2r_int_int (io);
      } else {
        R_nc_c2r_int_dbl (io);
      }
      break;
    case NC_UINT:
      if (unpack) {
        R_nc_c2r_unpack_uint (io);
      } else {
        R_nc_c2r_uint_dbl (io);
      }
      break;
    case NC_FLOAT:
      if (unpack) {
        R_nc_c2r_unpack_float (io);
      } else {
        R_nc_c2r_float_dbl (io);
      }
      break;
    case NC_DOUBLE:
      if (unpack) {
        R_nc_c2r_unpack_dbl (io);
      } else {
        R_nc_c2r_dbl_dbl (io);
      }
      break;
    case NC_INT64:
      if (unpack) {
        R_nc_c2r_unpack_int64 (io);
      } else if (io->fitnum) {
        R_nc_c2r_int64_bit64 (io);
      } else {
        R_nc_c2r_int64_dbl (io);
      }
      break;
    case NC_UINT64:
      if (unpack) {
        R_nc_c2r_unpack_uint64 (io);
      } else if (io->fitnum) {
        R_nc_c2r_uint64_bit64 (io);
      } else {
        R_nc_c2r_uint64_dbl (io);
      }
      break;
    case NC_CHAR:
      if (io->rawchar) {
        R_nc_char_raw (io);
      } else {
        R_nc_char_strsxp (io);
      }
      break;
    case NC_STRING:
      R_nc_str_strsxp (io);
      break;
    default:
      if (io->xtype > NC_MAX_ATOMIC_TYPE) {
        R_nc_check (nc_inq_user_type (
          io->ncid, io->xtype, NULL, NULL, NULL, NULL, &class));
        switch (class) {
        case NC_COMPOUND:
          R_nc_compound_vecsxp (io);
          break;
        case NC_ENUM:
          R_nc_enum_factor (io);
          break;
        case NC_VLEN:
          R_nc_vlen_vecsxp (io);
          break;
        case NC_OPAQUE:
          R_nc_opaque_raw (io);
          break;
        default:
          error (RNC_ETYPEDROP);
        }
      } else {
        error (RNC_ETYPEDROP);
      }
  }
}


/*=============================================================================*\
 *  Dimension conversions
\*=============================================================================*/

/* Reverse a vector in-place.
   Example: R_nc_rev_int (cv, cnt);
 */

void
R_nc_rev_int (int *data, size_t cnt)
{
  size_t ii, jj;
  int tmp;
  if (cnt<=0) return;
  for (ii=0, jj=cnt-1; ii<jj; ii++, jj--) {
    tmp = data[ii];
    data[ii] = data[jj];
    data[jj] = tmp;
  }
}

void
R_nc_rev_size (size_t *data, size_t cnt)
{
  size_t ii, jj;
  size_t tmp;
  if (cnt<=0) return;
  for (ii=0, jj=cnt-1; ii<jj; ii++, jj--) {
    tmp = data[ii];
    data[ii] = data[jj];
    data[jj] = tmp;
  }
}

/* Define R_nc_rev for other types as needed */


/* Copy the leading N elements of R vector rv into a new C vector of type TYPE,
   reversing from Fortran to C storage order.
   Elements beyond the length of rv and non-finite values are stored as fillval.
 */

int *
R_nc_dim_r2c_int (SEXP rv, size_t N, int fillval)
{
  int *cv;
  const void *voidbuf;
  size_t nr, ii;

  /* Allocate new C vector (freed by R) */
  cv = (int *) R_alloc (N, sizeof (int));

  /* Number of elements to copy must not exceed N */
  nr = xlength (rv);
  nr = (nr < N) ? nr : N;

  /* Copy R elements to cv */
  if (isReal (rv)) {
    if (R_nc_inherits (rv, "integer64")) {
      voidbuf = R_nc_r2c_bit64_int (rv, 1, &nr, sizeof(int), &fillval);
    } else {
      voidbuf = R_nc_r2c_dbl_int (rv, 1, &nr, sizeof(int), &fillval);
    }
  } else if (isInteger (rv)) {
    voidbuf = R_nc_r2c_int_int (rv, 1, &nr, sizeof(int), &fillval);
  } else {
    error ("Unsupported R type in R_nc_dim_r2c_int");
  }
  memcpy (cv, voidbuf, nr*sizeof (int));

  /* Reverse from Fortran to C order */
  R_nc_rev_int (cv, nr);

  /* Fill any remaining elements beyond length of rv */
  for ( ii=nr; ii<N; ii++ ) {
    cv[ii] = fillval;
  }

  return cv;
}

size_t *
R_nc_dim_r2c_size (SEXP rv, size_t N, size_t fillval)
{
  size_t *cv;
  const void *voidbuf;
  size_t nr, ii;

  /* Allocate new C vector (freed by R) */
  cv = (size_t *) R_alloc (N, sizeof (size_t));

  /* Number of elements to copy must not exceed N */
  nr = xlength (rv);
  nr = (nr < N) ? nr : N;

  /* Copy R elements to cv */
  if (isReal (rv)) {
    if (R_nc_inherits (rv, "integer64")) {
      voidbuf = R_nc_r2c_bit64_size (rv, 1, &nr, sizeof(size_t), &fillval);
    } else {
      voidbuf = R_nc_r2c_dbl_size (rv, 1, &nr, sizeof(size_t), &fillval);
    }
  } else if (isInteger (rv)) {
    voidbuf = R_nc_r2c_int_size (rv, 1, &nr, sizeof(size_t), &fillval);
  } else {
    error ("Unsupported R type in R_nc_dim_r2c_size");
  }
  memcpy (cv, voidbuf, nr*sizeof (size_t));

  /* Reverse from Fortran to C order */
  R_nc_rev_size (cv, nr);

  /* Fill any remaining elements beyond length of rv */
  for ( ii=nr; ii<N; ii++ ) {
    cv[ii] = fillval;
  }

  return cv;
}



/* Wrap R_nc_dim_r2c_size for scalar arguments
 */
size_t
R_nc_sizearg (SEXP size)
{
  size_t *result;
  if (xlength (size) < 1) {
    error ("Size argument must contain at least one numeric value");
  }
  result = R_nc_dim_r2c_size (size, 1, 0);
  return *result;
}
