/*=============================================================================*\
 *
 *  Name:       convert.c
 *
 *  Version:    2.0-1
 *
 *  Purpose:    Type conversions for RNetCDF
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
 *  Implementation and Revisions
 *-----------------------------------------------------------------------------*
 *  Author   Date       Description
 *  ------   ----       -----------
 *  mw       26/03/17   Split RNetCDF.c into multiple files
 *
\*=============================================================================*/


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

#define RNC_CHARSXP_MAXLEN 2147483647

#ifdef __WIN32__
# define RNC_FMT_LL "%I64d"
#else
# define RNC_FMT_LL "%lld"
#endif

#ifdef __WIN32__
# define RNC_FMT_ULL "%I64u"
#else
# define RNC_FMT_ULL "%llu"
#endif

#define RNC_DBL_DIG 24

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

/* Definitions for integer64 as provided by bit64 package */
int isInt64(SEXP rv) {
  int status=-1, ii;
  SEXP class;
  class = getAttrib(rv, R_ClassSymbol);
  if (isString(class)) {
    for (ii=0; ii<length(class); ii++) {
      status = strcmp(CHAR(STRING_ELT(class, ii)), "integer64");
      if (status == 0) break;
    }
  }
  return (status == 0);
}

/*=============================================================================*\
 *  String conversions.
\*=============================================================================*/


char *
R_nc_strsxp_char (SEXP rstr, int ndim, size_t *xdim)
{
  size_t ii, strlen, cnt;
  char *carr, *thisstr;
  if (ndim > 0) {
    /* Omit fastest-varying dimension from R character array */
    strlen = xdim[ndim-1];
    cnt = R_nc_length (ndim-1, xdim);
  } else {
    /* Scalar character */
    strlen = 1;
    cnt = 1;
  }
  if (xlength (rstr) < cnt) {
    RERROR (RNC_EDATALEN);
  }
  carr = R_alloc (cnt*strlen, sizeof (char));
  for (ii=0, thisstr=carr; ii<cnt; ii++, thisstr+=strlen) {
    strncpy(thisstr, CHAR( STRING_ELT (rstr, ii)), strlen);
  }
  return carr;
}


SEXP
R_nc_char_strsxp (char *carr, int ndim, size_t *xdim)
{
  size_t ii, cnt, clen, rlen;
  char *thisstr, *endstr, endchar;
  SEXP rstr;
  if (ndim > 0) {
    /* Omit fastest-varying dimension from R character array */
    clen = xdim[ndim-1];
  } else {
    /* Scalar character */
    clen = 1;
  }
  rlen = (clen <= RNC_CHARSXP_MAXLEN) ? clen : RNC_CHARSXP_MAXLEN;
  rstr = R_nc_allocArray (STRSXP, ndim-1, xdim);
  cnt = xlength (rstr);
  for (ii=0, thisstr=carr; ii<cnt; ii++, thisstr+=clen) {
    /* Temporarily null-terminate each string before passing to R */
    endstr = thisstr + rlen;
    endchar = *endstr;
    *endstr = '\0';
    SET_STRING_ELT (rstr, ii, mkChar(thisstr));
    *endstr = endchar;
  }
  return rstr;
}


const char **
R_nc_strsxp_str (SEXP rstr, int ndim, size_t *xdim)
{
  size_t ii, cnt;
  cnt = R_nc_length (ndim, xdim);
  cstr = (char **) R_alloc (cnt, sizeof(size_t));
  for (ii=0; ii<cnt; ii++) {
    cstr[ii] = CHAR( STRING_ELT (rstr, ii));
  }
  return cstr;
}


SEXP
R_nc_str_strsxp (char **cstr, int ndim, size_t *xdim)
{
  size_t ii, nchar, cnt;
  char *endstr, endchar;
  SEXP rstr;
  rstr = R_nc_allocArray (STRSXP, ndim, xdim);
  cnt = xlength (rstr);
  for (ii=0; ii<cnt; ii++) {
    nchar = strlen (cstr[ii]);
    if (nchar > RNC_CHARSXP_MAXLEN) {
      /* Temporarily truncate excessively long strings before passing to R */
      endstr = cstr[ii]+RNC_CHARSXP_MAXLEN+1;
      endchar = *endstr;
      *endstr = '\0';
      SET_STRING_ELT (rstr, ii, mkChar (cstr[ii]));
      *endstr = endchar;
    } else if (nchar > 0) {
      SET_STRING_ELT (rstr, ii, mkChar (cstr[ii]));
    }
  }
  return rstr;
}


/*=============================================================================*\
 *  Numeric type conversions
\*=============================================================================*/

#define R_NC_ISNA_INT(value) (value==NA_INTEGER)
#define R_NC_ISNA_REAL(value) (ISNAN(value))
#define R_NC_ISNA_NONE(value) (0)

#define R_NC_RANGE_MIN(VAL,LIM,TYPE) ((TYPE) LIM <= (TYPE) VAL)
#define R_NC_RANGE_MAX(VAL,LIM,TYPE) ((TYPE) VAL <= (TYPE) LIM)
#define R_NC_RANGE_NONE(VAL,LIM,TYPE) (1)

#define R_NC_R2C_NUM(FUN, ITYPE, IFUN, OTYPE, NATEST, MINTEST, MINVAL, MAXTEST, MAXVAL) \
static OTYPE* \
FUN (SEXP rv, int ndim, size_t *xdim, \
     OTYPE *fill, double *scale, double *add) \
{ \
  size_t ii, cnt; \
  int erange=0; \
  double factor, offset; \
  const ITYPE* in; \
  OTYPE fillval; \
  OTYPE restrict *out; \
  in = (ITYPE *) IFUN (rv); \
  cnt = R_nc_length (ndim, xsize); \
  if (xlength (rv) < cnt) { \
    RERROR (RNC_EDATALEN); \
  } \
  out = (OTYPE *) R_alloc (cnt, sizeof(OTYPE)); \
  if (scale) { \
    factor = *scale; \
  } else { \
    factor = 1.0; \
  } \
  if (add) { \
    offset = *add; \
  } else { \
    offset = 0.0; \
  } \
  if (fill) { \
    fillval = *fill; \
    if (scale || add) { \
      for (ii=0; ii<cnt; ii++) { \
	if (NATEST(in[ii])) { \
	  out[ii] = fillval; \
	} else if (MINTEST(in[ii],MINVAL,ITYPE) && MAXTEST(in[ii],MAXVAL,ITYPE)) { \
	  out[ii] = round((in[ii] - offset) / factor); \
	} else { \
	  erange = 1; \
	  break; \
	} \
      } \
    } else { \
      for (ii=0; ii<cnt; ii++) { \
	if (NATEST(in[ii])) { \
	  out[ii] = fillval; \
	} else if (MINTEST(in[ii],MINVAL,ITYPE) && MAXTEST(in[ii],MAXVAL,ITYPE)) { \
	  out[ii] = in[ii]; \
	} else { \
	  erange = 1; \
	  break; \
	} \
      } \
    } \
  } else { \
    if (scale || add) { \
      for (ii=0; ii<cnt; ii++) { \
	if (MINTEST(in[ii],MINVAL,ITYPE) && MAXTEST(in[ii],MAXVAL,ITYPE)) { \
	  out[ii] = round((in[ii] - offset) / factor); \
	} else { \
	  erange = 1; \
	  break; \
	} \
      } \
    } else { \
      for (ii=0; ii<cnt; ii++) { \
	if (MINTEST(in[ii],MINVAL,ITYPE) && MAXTEST(in[ii],MAXVAL,ITYPE)) { \
	  out[ii] = in[ii]; \
	} else { \
	  erange = 1; \
	  break; \
	} \
      } \
    } \
  } \
  if ( erange ) { \
    R_nc_error (nc_strerror (NC_ERANGE)); \
  } \
  return out; \
}

R_NC_R2C_NUM(R_nc_r2c_raw_uchar, unsigned char, RAW, unsigned char, \
  R_NC_ISNA_NONE, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );

R_NC_R2C_NUM(R_nc_r2c_int_schar, int, INTEGER, signed char, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_uchar, int, INTEGER, unsigned char, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_short, int, INTEGER, short, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_ushort, int, INTEGER, unsigned short, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_int, INTEGER, int, int, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_uint, INTEGER, int, unsigned int, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_ll, int, INTEGER, long long, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_ull, int, INTEGER, unsigned long long, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_size, int, INTEGER, size_t, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_float, int, INTEGER, float, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_dbl, int, INTEGER, double, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );

R_NC_R2C_NUM(R_nc_r2c_dbl_schar, double, REAL, signed char, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uchar, double, REAL, unsigned char, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_short, double, REAL, short, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ushort, double, REAL, unsigned short, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_int, double, REAL, int, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, INT_MIN, R_NC_RANGE_MAX, INT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uint, double, REAL, unsigned int, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UINT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ll, double, REAL, long long, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, LLONG_MIN_DBL, R_NC_RANGE_MAX, LLONG_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_ull, double, REAL, unsigned long long, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, ULLONG_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_size, double, REAL, size_t, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, SIZE_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_float, double, REAL, float, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, -FLT_MAX, R_NC_RANGE_MAX, FLT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_dbl, double, REAL, double, \
  R_NC_ISNA_REAL, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );

R_NC_R2C_NUM(R_nc_r2c_bit64_schar, long long, REAL, signed char, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_uchar, long long, REAL, unsigned char, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_short, long long, REAL, short, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_ushort, long long, REAL, unsigned short, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_int, long long, REAL, int, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, INT_MIN, R_NC_RANGE_MAX, INT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_uint, long long, REAL, unsigned int, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UINT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_ll, long long, REAL, long long, \
  R_NC_ISNA_NONE, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
/* Treat bit64 as unsigned when converting to unsigned long long */
R_NC_R2C_NUM(R_nc_r2c_bit64_ull, unsigned long long, REAL, unsigned long long, \
  R_NC_ISNA_NONE, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_bit64_size, long long, REAL, size_t, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, SIZE_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_float, long long, REAL, float, \
  R_NC_ISNA_NONE, R_NC_RANGE_MIN, -FLT_MAX, R_NC_RANGE_MAX, FLT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_dbl, long long, REAL, double, \
  R_NC_ISNA_NONE, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );


#define R_NC_C2R_NUM(FUN, ITYPE, SEXPTYPE, OFUN, OTYPE, MISSVAL) \
static SEXP \
FUN (const ITYPE* restrict in, int ndim, size_t *xdim, \
     ITYPE *fill, double *scale, double *add) \
{ \
  size_t ii, cnt; \
  double factor, offset; \
  SEXP rv; \
  ITYPE fillval; \
  OTYPE restrict *out; \
  rv = R_nc_allocArray (SEXPTYPE, ndims, xdim); \
  cnt = xlength (rv); \
  out = (OTYPE) OFUN (rv); \
  if (scale) { \
    factor = *scale; \
  } else { \
    factor = 1.0; \
  } \
  if (add) { \
    offset = *add; \
  } else { \
    offset = 0.0; \
  } \
  if (fill) { \
    fillval = *fill; \
    if (fillval != fillval) \
      if (scale || add) { \
	for (ii=0; ii<cnt; ii++) { \
	  if (in[ii] != in[ii]) { \
            out[ii] = MISSVAL; \
          } else { \
	    out[ii] = in[ii] * factor + offset; \
	  } \
	} \
      } else { \
	for (ii=0; ii<cnt; ii++) { \
	  if (in[ii] != in[ii]) { \
	    out[ii] = MISSVAL; \
	  } else { \
	    out[ii] = in[ii]; \
	  } \
	} \
      } \
    } else { \
      if (scale || add) { \
	for (ii=0; ii<cnt; ii++) { \
	  if (in[ii] == fillval) { \
            out[ii] = MISSVAL; \
          } else { \
	    out[ii] = in[ii] * factor + offset; \
	  } \
	} \
      } else { \
	for (ii=0; ii<cnt; ii++) { \
	  if (in[ii] == fillval) { \
	    out[ii] = MISSVAL; \
	  } else { \
	    out[ii] = in[ii]; \
	  } \
	} \
      } \
    } \
  } else { \
    if (scale || add) { \
      for (ii=0; ii<cnt; ii++) { \
	out[ii] = in[ii] * factor + offset; \
      } \
    } else { \
      for (ii=0; ii<cnt; ii++) { \
	out[ii] = in[ii]; \
      } \
    } \
  } \
  return rv; \
}

R_NC_C2R_NUM(R_nc_c2r_uchar_raw, char, RAWSXP, RAW, char, '\0');

R_NC_C2R_NUM(R_nc_c2r_schar_int, signed char, INTSXP, INTEGER, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_uchar_int, unsigned char, INTSXP, INTEGER, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_short_int, short, INTSXP, INTEGER, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_ushort_int, unsigned short, INTSXP, INTEGER, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_int_int, int, INSTSXP, INTEGER, int, NA_INTEGER);

R_NC_C2R_NUM(R_nc_c2r_schar_dbl, signed char, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uchar_dbl, unsigned char, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_short_dbl, short, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_ushort_dbl, unsigned short, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_int_dbl, int, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uint_dbl, unsigned int, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_float_dbl, float, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_dbl_dbl, double, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_int64_dbl, long long, REALSXP, REAL, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uint64_dbl, unsigned long long, REALSXP, REAL, double, NA_REAL);

R_NC_C2R_NUM(R_nc_c2r_schar_bit64, signed char, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_uchar_bit64, unsigned char, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_short_bit64, short, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_ushort_bit64, unsigned short, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_int_bit64, int, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_uint_bit64, unsigned int, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_float_bit64, float, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_dbl_bit64, double, REALSXP, REAL, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_int64_bit64, long long, REALSXP, REAL, long long, NA_INTEGER64);
/* Treat bit64 as unsigned when converting from unsigned long long */
R_NC_C2R_NUM(R_nc_c2r_uint64_bit64, unsigned long long, REALSXP, REAL, unsigned long long, NA_INTEGER64);


/*=============================================================================*\
 *  Generic type conversions
\*=============================================================================*/

void *
R_nc_r2c (SEXP rv, int ncid, nc_type xtype, int ndim, size_t *xdim,
          void *fill, double *scale, double *add)
{
  void *cv=NULL;

  switch (TYPEOF(rv)) {
  case INTSXP:
    switch (xtype) {
    case NC_BYTE:
      cv = R_nc_r2c_int_schar (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_UBYTE:
      cv = R_nc_r2c_int_uchar (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_SHORT:
      cv = R_nc_r2c_int_short (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_USHORT:
      cv = R_nc_r2c_int_ushort (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_INT:
      cv = R_nc_r2c_int_int (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_UINT:
      cv = R_nc_r2c_int_uint (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_INT64:
      cv = R_nc_r2c_int_ll (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_UINT64:
      cv = R_nc_r2c_int_ull (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_FLOAT:
      cv = R_nc_r2c_int_float (rv, ndim, xdim, fill, scale, add);
      break;
    case NC_DOUBLE:
      cv = R_nc_r2c_int_dbl (rv, ndim, xdim, fill, scale, add);
      break;
    default:
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  case REALSXP:  
    if (isInt64(rv)) {
      switch (xtype) {
      case NC_BYTE:
	cv = R_nc_r2c_bit64_schar (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_UBYTE:
	cv = R_nc_r2c_bit64_uchar (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_SHORT:
	cv = R_nc_r2c_bit64_short (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_USHORT:
	cv = R_nc_r2c_bit64_ushort (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_INT:
	cv = R_nc_r2c_bit64_int (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_UINT:
	cv = R_nc_r2c_bit64_uint (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_INT64:
	cv = R_nc_r2c_bit64_ll (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_UINT64:
	cv = R_nc_r2c_bit64_ull (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_FLOAT:
	cv = R_nc_r2c_bit64_float (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_DOUBLE:
	cv = R_nc_r2c_bit64_dbl (rv, ndim, xdim, fill, scale, add);
	break;
      default:
	R_nc_error (RNC_EDATATYPE);
      }
    } else {
      switch (xtype) {
      case NC_BYTE:
	cv = R_nc_r2c_dbl_schar (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_UBYTE:
	cv = R_nc_r2c_dbl_uchar (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_SHORT:
	cv = R_nc_r2c_dbl_short (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_USHORT:
	cv = R_nc_r2c_dbl_ushort (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_INT:
	cv = R_nc_r2c_dbl_int (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_UINT:
	cv = R_nc_r2c_dbl_uint (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_INT64:
	cv = R_nc_r2c_dbl_ll (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_UINT64:
	cv = R_nc_r2c_dbl_ull (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_FLOAT:
	cv = R_nc_r2c_dbl_float (rv, ndim, xdim, fill, scale, add);
	break;
      case NC_DOUBLE:
	cv = R_nc_r2c_dbl_dbl (rv, ndim, xdim, fill, scale, add);
	break;
      default:
	R_nc_error (RNC_EDATATYPE);
      }
    }
    break;
  case STRSXP:
    switch (xtype) {
    case NC_CHAR:
      cv = R_nc_strsxp_char (rv, ndim, xdim);
      break;
    case NC_STRING:
      cv = R_nc_strsxp_str (rv, ndim, xdim);
      break;
    default:
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  case RAWSXP:
    if (xtype == NC_CHAR) {
      cv = R_nc_r2c_raw_uchar (rv, ndim, xdim, NULL, NULL, NULL);
    } else {
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  default:
    R_nc_error (RNC_EDATATYPE);
  }
  return cv;
}


SEXP
R_nc_c2r (void *cv, int ncid, nc_type xtype, int ndim, size_t *xdim,
          int rawchar, int fitnum,
          void *fill, double *scale, double *add)
{
  SEXP rv=NULL;
  int asInt;

  asInt = ((fitnum == TRUE) && (scale == NULL) && (add == NULL));

  /* Type conversions */
  switch (xtype) {
    case NC_BYTE:
      if (asInt) {
        rv = R_nc_c2r_schar_int (cv, ndim, xdim, fill, NULL, NULL);
      } else {
        rv = R_nc_c2r_schar_dbl (cv, ndim, xdim, fill, scale, add);
      }
      break;
    case NC_UBYTE:
      if (asInt) {
        rv = R_nc_c2r_uchar_int (cv, ndim, xdim, fill, NULL, NULL);
      } else {
        rv = R_nc_c2r_uchar_dbl (cv, ndim, xdim, fill, scale, add);
      }
      break;
    case NC_SHORT:
      if (asInt) {
        rv = R_nc_c2r_short_int (cv, ndim, xdim, fill, NULL, NULL);
      } else {
        rv = R_nc_c2r_short_dbl (cv, ndim, xdim, fill, scale, add);
      }
      break;
    case NC_USHORT:
      if (asInt) {
        rv = R_nc_c2r_ushort_int (cv, ndim, xdim, fill, NULL, NULL);
      } else {
        rv = R_nc_c2r_ushort_dbl (cv, ndim, xdim, fill, scale, add);
      }
      break;
    case NC_INT:
      if (asInt) {
        rv = R_nc_c2r_int_int (cv, ndim, xdim, fill, NULL, NULL);
      } else {
        rv = R_nc_c2r_int_dbl (cv, ndim, xdim, fill, scale, add);
      }
      break;
    case NC_UINT:
      rv = R_nc_c2r_uint_dbl (cv, ndim, xdim, fill, scale, add);
      break;
    case NC_FLOAT:
      rv = R_nc_c2r_float_dbl (cv, ndim, xdim, fill, scale, add);
      break;
    case NC_DOUBLE:
      rv = R_nc_c2r_dbl_dbl (cv, ndim, xdim, fill, scale, add);
      break;
    case NC_INT64:
      if (asInt) {
        rv = R_nc_c2r_int64_bit64 (cv, ndim, xdim, fill, NULL, NULL);
      } else {
        rv = R_nc_c2r_int64_dbl (cv, ndim, xdim, fill, scale, add);
      }
      break;
    case NC_UINT64:
      if (asInt) {
        rv = R_nc_c2r_uint64_bit64 (cv, ndim, xdim, fill, NULL, NULL);
      } else {
        rv = R_nc_c2r_uint64_dbl (cv, ndim, xdim, fill, scale, add);
      }
      break;
    case NC_CHAR:
      if (rawchar) {
        rv = R_nc_c2r_char_raw (cv, ndim, xdim, NULL, NULL, NULL);
      } else {
        rv = R_nc_char_strsxp (cv, ndim, xdim);
      }
      break;
    case NC_STRING:
      rv = R_nc_str_strsxp (cv, ndim, xdim);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
  }
  return rv;
}


/*=============================================================================*\
 *  Dimension conversions
\*=============================================================================*/

/* Reverse a vector in-place.
   Example: R_nc_rev_int (cv, cnt);
 */
#define R_NC_REVERSE(FUN, TYPE) \
void \
FUN (TYPE *data, size_t cnt) \
{ \
  size_t ii, jj; \
  TYPE tmp; \
  if (cnt<=0) return; \
  for (ii=0, jj=cnt-1; ii<jj; ii++, jj--) { \
    tmp = data[ii]; \
    data[ii] = data[jj]; \
    data[jj] = tmp; \
  } \
}

R_NC_REVERSE(R_nc_rev_int, int);
R_NC_REVERSE(R_nc_rev_size, size_t);
/* Define R_nc_rev for other types as needed */


/* Copy the leading nr elements of R vector rv to C vector cv,
   converting type to TYPE and reversing from Fortran to C storage order.
   Elements beyond the length of rv and non-finite values are stored as fillval.
 */
#define R_NC_DIM_R2C(FUN, TYPENAME, TYPE) \
void \
FUN (SEXP rv, size_t nr, TYPE fillval, TYPE *cv) \
{ \
  double *realp; \
  int *intp; \
  size_t nc, ii; \
\
  /* Number of elements to copy must not exceed length of rv */ \
  nc = xlength (rv); \
  nc = (nr < nc) ? nr : nc; \
\
  /* Copy elements */ \
  if (isReal (rv)) { \
    realp = REAL (rv); \
    R_nc_r2c_dbl_##TYPENAME (realp, cv, nc, &fillval, NULL, NULL); \
  } else if (isInteger (rv)) { \
    intp = INTEGER (rv); \
    R_nc_r2c_int_##TYPENAME (intp, cv, nc, &fillval, NULL, NULL); \
  } else { \
    nc = 0; \
  } \
\
  /* Fill any remaining elements beyond length of rv */ \
  for ( ii=nc; ii<nr; ii++ ) { \
    cv[ii] = fillval; \
  } \
\
  /* Reverse from Fortran to C order */ \
  R_nc_rev_##TYPENAME (cv, nr); \
}

R_NC_DIM_R2C (R_nc_dim_r2c_int, int, int);
R_NC_DIM_R2C (R_nc_dim_r2c_size, size, size_t);


