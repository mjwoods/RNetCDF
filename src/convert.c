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
 *  Memory management.
\*=============================================================================*/

SEXP
R_nc_allocArray (SEXPTYPE type, int ndims, const size_t *ccount) {
  SEXP result, rdim;
  int *intp, ii, jj;
  if (ndims > 0) {
    rdim = R_nc_protect( allocVector (INTSXP, ndims));
    intp = INTEGER (rdim);
    for ( ii=0, jj=ndims-1; ii<ndims; ii++, jj-- ) {
      if (ccount[jj] <= INT_MAX) {
        intp[ii] = ccount[jj];
      } else {
        RERROR ("R array dimension cannot exceed range of type int");
      }
    }
    result = R_nc_protect (allocArray (type, rdim));
  } else {
    /* R scalar or vector with no dimensions */
    result = R_nc_protect (allocVector (type, 1));
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


static void
R_nc_char_strsxp_init (R_nc_buf *io)
{
  size_t cnt;
  cnt = R_nc_length (io->ndim, io->xdim);
  io->rxp = R_nc_allocArray (STRSXP, (io->ndim)-1, io->xdim);
  io->buf = R_alloc (cnt, sizeof (char));
}


static void
R_nc_char_strsxp (R_nc_buf *io)
{
  size_t ii, cnt, clen, rlen;
  char *thisstr, *endstr, endchar;
  if (io->ndim > 0) {
    /* Omit fastest-varying dimension from R character array */
    clen = io->xdim[(io->ndim)-1];
  } else {
    /* Scalar character */
    clen = 1;
  }
  rlen = (clen <= RNC_CHARSXP_MAXLEN) ? clen : RNC_CHARSXP_MAXLEN;
  cnt = xlength (io->rxp);
  for (ii=0, thisstr=io->buf; ii<cnt; ii++, thisstr+=clen) {
    /* Temporarily null-terminate each string before passing to R */
    endstr = thisstr + rlen;
    endchar = *endstr;
    *endstr = '\0';
    SET_STRING_ELT (io->rxp, ii, mkChar(thisstr));
    *endstr = endchar;
  }
}


static char *
R_nc_raw_char (SEXP rarr, int ndim, const size_t *xdim)
{
  size_t cnt;
  char *carr;
  cnt = R_nc_length (ndim, xdim);
  if (xlength (rarr) < cnt) {
    RERROR (RNC_EDATALEN);
  }
  carr = R_alloc (cnt, sizeof (char));
  memcpy (carr, RAW (rarr), cnt);
  return carr;
}


static void
R_nc_char_raw_init (R_nc_buf *io)
{
  io->rxp = R_nc_allocArray (RAWSXP, io->ndim, io->xdim);
  io->buf = RAW (io->rxp);
}


static void
R_nc_char_raw (R_nc_buf *io)
{
  // Nothing to do!
  return;
}


static const char **
R_nc_strsxp_str (SEXP rstr, int ndim, const size_t *xdim)
{
  size_t ii, cnt;
  cnt = R_nc_length (ndim, xdim);
  cstr = (char **) R_alloc (cnt, sizeof(size_t));
  for (ii=0; ii<cnt; ii++) {
    cstr[ii] = CHAR( STRING_ELT (rstr, ii));
  }
  return cstr;
}


static void
R_nc_str_strsxp_init (R_nc_buf *io)
{
  size_t cnt;
  cnt = R_nc_length (io->ndim, io->xdim);
  io->rxp = R_nc_allocArray (STRSXP, io->ndim, io->xdim);
  io->buf = R_alloc (cnt, sizeof(size_t));
}


static void
R_nc_str_strsxp (R_nc_buf *io)
{
  size_t ii, nchar, cnt;
  char **cstr, *endstr, endchar;
  cnt = xlength (io->rxp);
  cstr = (char **) io->buf;
  for (ii=0; ii<cnt; ii++) {
    nchar = strlen (cstr[ii]);
    if (nchar > RNC_CHARSXP_MAXLEN) {
      /* Temporarily truncate excessively long strings before passing to R */
      endstr = cstr[ii]+RNC_CHARSXP_MAXLEN+1;
      endchar = *endstr;
      *endstr = '\0';
      SET_STRING_ELT (io->rxp, ii, mkChar (cstr[ii]));
      *endstr = endchar;
    } else if (nchar > 0) {
      SET_STRING_ELT (io->rxp, ii, mkChar (cstr[ii]));
    }
  }
  /* Free pointers to strings created by netcdf */
  if (cnt > 0) {
    R_nc_check (nc_free_string (cnt, io->buf));
  }
}


/*=============================================================================*\
 *  Numeric type conversions
\*=============================================================================*/

#define R_NC_ISNA_INT(value) (value==NA_INTEGER)
#define R_NC_ISNA_REAL(value) (ISNAN(value))
#define R_NC_ISNA_BIT64(value) (value==NA_INTEGER64)

#define R_NC_RANGE_MIN(VAL,LIM,TYPE) ((TYPE) LIM <= (TYPE) VAL)
#define R_NC_RANGE_MAX(VAL,LIM,TYPE) ((TYPE) VAL <= (TYPE) LIM)
#define R_NC_RANGE_NONE(VAL,LIM,TYPE) (1)


/* Convert numeric values from R to C format.
   Memory for the result is allocated if necessary (and freed by R).
   In special cases, the output is a pointer to the input data,
   so the output data should not be modified.
   An error is raised if any input values are outside the range of the output type.
 */
#define R_NC_R2C_NUM(FUN, \
  NCITYPE, ITYPE, IFUN, NCOTYPE, OTYPE, \
  NATEST, MINTEST, MINVAL, MAXTEST, MAXVAL) \
static const OTYPE* \
FUN (SEXP rv, int ndim, const size_t *xdim, \
     const OTYPE *fill, const double *scale, const double *add) \
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
  if (fill || scale || add || (NCITYPE != NCOTYPE)) { \
    out = (OTYPE *) R_alloc (cnt, sizeof(OTYPE)); \
  } else { \
    out = (ITYPE *) IFUN (rv); \
  } \
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
      if (NCITYPE != NCOTYPE) { \
	for (ii=0; ii<cnt; ii++) { \
	  if (MINTEST(in[ii],MINVAL,ITYPE) && MAXTEST(in[ii],MAXVAL,ITYPE)) { \
	    out[ii] = in[ii]; \
	  } else { \
	    erange = 1; \
	    break; \
	  } \
        } \
      } else { \
	for (ii=0; ii<cnt; ii++) { \
	  if (!(MINTEST(in[ii],MINVAL,ITYPE) && MAXTEST(in[ii],MAXVAL,ITYPE))) { \
	    erange = 1; \
	    break; \
	  } \
	} \
      } \
    } \
  } \
  if ( erange ) { \
    R_nc_error (nc_strerror (NC_ERANGE)); \
  } \
  return out; \
}

R_NC_R2C_NUM(R_nc_r2c_int_schar, NC_INT, int, INTEGER, NC_BYTE, signed char, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_uchar, NC_INT, int, INTEGER, NC_UBYTE, unsigned char, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_short, NC_INT, int, INTEGER, NC_SHORT, short, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_ushort, NC_INT, int, INTEGER, NC_USHORT, unsigned short, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_int, NC_INT, int, INTEGER, NC_INT, int, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_uint, NC_INT, int, INTEGER, NC_UINT, unsigned int, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_ll, NC_INT, int, INTEGER, NC_INT64, long long, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_ull, NC_INT, int, INTEGER, NC_UINT64, unsigned long long, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
/* Assume int and size_t are different types */
R_NC_R2C_NUM(R_nc_r2c_int_size, NC_INT, int, INTEGER, NC_UINT64, size_t, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_float, NC_INT, int, INTEGER, NC_FLOAT, float, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_dbl, NC_INT, int, INTEGER, NC_DOUBLE, double, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );

R_NC_R2C_NUM(R_nc_r2c_dbl_schar, NC_DOUBLE, double, REAL, NC_BYTE, signed char, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uchar, NC_DOUBLE, double, REAL, NC_UBYTE, unsigned char, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_short, NC_DOUBLE, double, REAL, NC_SHORT, short, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ushort, NC_DOUBLE, double, REAL, NC_USHORT, unsigned short, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_int, NC_DOUBLE, double, REAL, NC_INT, int, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, INT_MIN, R_NC_RANGE_MAX, INT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uint, NC_DOUBLE, double, REAL, NC_UINT, unsigned int, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UINT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ll, NC_DOUBLE, double, REAL, NC_INT64, long long, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, LLONG_MIN_DBL, R_NC_RANGE_MAX, LLONG_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_ull, NC_DOUBLE, double, REAL, NC_UINT64, unsigned long long, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, ULLONG_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_size, NC_DOUBLE, double, REAL, NC_UINT64, size_t, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, SIZE_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_float, NC_DOUBLE, double, REAL, NC_FLOAT, float, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, -FLT_MAX, R_NC_RANGE_MAX, FLT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_dbl, NC_DOUBLE, double, REAL, NC_DOUBLE, double, \
  R_NC_ISNA_REAL, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );

R_NC_R2C_NUM(R_nc_r2c_bit64_schar, NC_INT64, long long, REAL, NC_BYTE, signed char, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_uchar, NC_INT64, long long, REAL, NC_UBYTE, unsigned char, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_short, NC_INT64, long long, REAL, NC_SHORT, short, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_ushort, NC_INT64, long long, REAL, NC_USHORT, unsigned short, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_int, NC_INT64, long long, REAL, NC_INT, int, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, INT_MIN, R_NC_RANGE_MAX, INT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_uint, NC_INT64, long long, REAL, NC_UINT, unsigned int, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UINT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_ll, NC_INT64, long long, REAL, NC_INT64, long long, \
  R_NC_ISNA_BIT64, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
/* Treat bit64 as unsigned when converting to unsigned long long */
R_NC_R2C_NUM(R_nc_r2c_bit64_ull, NC_UINT64, unsigned long long, REAL, NC_UINT64, unsigned long long, \
  R_NC_ISNA_BIT64, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
/* Assume bit64 and size_t are different types */
R_NC_R2C_NUM(R_nc_r2c_bit64_size, NC_INT64, long long, REAL, NC_UINT64, size_t, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, SIZE_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_float, NC_INT64, long long, REAL, NC_FLOAT, float, \
  R_NC_ISNA_BIT64, R_NC_RANGE_MIN, -FLT_MAX, R_NC_RANGE_MAX, FLT_MAX);
R_NC_R2C_NUM(R_nc_r2c_bit64_dbl, NC_INT64, long long, REAL, NC_DOUBLE, double, \
  R_NC_ISNA_BIT64, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );


/* Allocate memory for reading a netcdf variable slice
   and converting the results to an R variable.
   On input, the R_nc_buf structure contains dimensions of the buffer (ndim, *xdim).
   On output, the R_nc_buf structure contains an allocated SEXP and a pointer to its data.
 */
#define R_NC_C2R_NUM_INIT(FUN, SEXPTYPE, OFUN) \
static void \
FUN (R_nc_buf *io) \
{ \
  io->rxp = R_nc_allocArray (SEXPTYPE, io->ndim, io->xdim); \
  io->buf = OFUN (io->rxp); \
}

R_NC_C2R_NUM_INIT(R_nc_c2r_int_init, INTSXP, INTEGER);
R_NC_C2R_NUM_INIT(R_nc_c2r_dbl_init, REALSXP, REAL);
R_NC_C2R_NUM_INIT(R_nc_c2r_bit64_init, REALSXP, REAL);


/* Convert numeric values from C to R format.
   Parameters and buffers for the conversion are passed via the R_nc_buf struct.
   The same buffer is used for input and output.
   Output type may be larger (not smaller) than input,
   so convert in reverse order to avoid overwriting input with output.
   If input and output are same type, no copying is needed.
 */
#define R_NC_C2R_NUM(FUN, NCITYPE, ITYPE, NCOTYPE, OTYPE, MISSVAL) \
static void \
FUN (R_nc_buf *io) \
{ \
  size_t ii, cnt; \
  ITYPE fillval, *in; \
  OTYPE *out; \
  cnt = xlength (io->rxp); \
  in = (ITYPE *) io->buf; \
  out = (OTYPE *) io->buf; \
  if (NCITYPE == NCOTYPE) { \
    if (io->fill) { \
      fillval = *(io->fill); \
      if (fillval != fillval) { \
	for (ii=cnt-1; ii>=0; ii--) { \
	  if (in[ii] != in[ii]) { \
	    out[ii] = MISSVAL; \
	  } \
	} \
      } else { \
	for (ii=cnt-1; ii>=0; ii--) { \
	  if (in[ii] == fillval) { \
	    out[ii] = MISSVAL; \
	  } \
        } \
      } \
    } \
  } else { \
    if (io->fill) { \
      fillval = *(io->fill); \
      if (fillval != fillval) { \
	for (ii=cnt-1; ii>=0; ii--) { \
	  if (in[ii] != in[ii]) { \
	    out[ii] = MISSVAL; \
	  } else { \
	    out[ii] = in[ii]; \
	  } \
	} \
      } else { \
	for (ii=cnt-1; ii>=0; ii--) { \
	  if (in[ii] == fillval) { \
	    out[ii] = MISSVAL; \
	  } else { \
	    out[ii] = in[ii]; \
	  } \
	} \
      } \
    } else { \
      for (ii=cnt-1; ii>=0; ii--) { \
	out[ii] = in[ii]; \
      } \
    } \
  } \
}

R_NC_C2R_NUM(R_nc_c2r_schar_int, NC_BYTE, signed char, NC_INT, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_uchar_int, NC_UBYTE, unsigned char, NC_INT, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_short_int, NC_SHORT, short, NC_INT, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_ushort_int, NC_USHORT, unsigned short, NC_INT, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_int_int, NC_INT, int, NC_INT, int, NA_INTEGER);

R_NC_C2R_NUM(R_nc_c2r_schar_dbl, NC_BYTE, signed char, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uchar_dbl, NC_UBYTE, unsigned char, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_short_dbl, NC_SHORT, short, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_ushort_dbl, NC_USHORT, unsigned short, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_int_dbl, NC_INT, int, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uint_dbl, NC_UINT, unsigned int, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_float_dbl, NC_FLOAT, float, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_dbl_dbl, NC_DOUBLE, double, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_int64_dbl, NC_INT64, long long, NC_DOUBLE, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uint64_dbl, NC_UINT64, unsigned long long, NC_DOUBLE, double, NA_REAL);

R_NC_C2R_NUM(R_nc_c2r_schar_bit64, NC_BYTE, signed char, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_uchar_bit64, NC_UBYTE, unsigned char, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_short_bit64, NC_SHORT, short, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_ushort_bit64, NC_USHORT, unsigned short, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_int_bit64, NC_INT, int, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_uint_bit64, NC_UINT, unsigned int, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_float_bit64, NC_FLOAT, float, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_dbl_bit64, NC_DOUBLE, double, NC_INT64, long long, NA_INTEGER64);
R_NC_C2R_NUM(R_nc_c2r_int64_bit64, NC_INT64, long long, NC_INT64, long long, NA_INTEGER64);
/* Treat bit64 as unsigned when converting from unsigned long long */
R_NC_C2R_NUM(R_nc_c2r_uint64_bit64, NC_UINT64, unsigned long long, NC_UINT64, unsigned long long, NA_INTEGER64);


/* Convert numeric values from C to R format with unpacking.
   Parameters and buffers for the conversion are passed via the R_nc_buf struct.
   Output type is assumed not to be smaller than input type,
   so the same buffer is used for input and output
   by converting in reverse order.
 */

#define R_NC_C2R_NUM_UNPACK(FUN, ITYPE) \
static void \
FUN (R_nc_buf *io) \
{ \
  size_t ii, cnt; \
  double factor, offset; \
  ITYPE fillval, *in; \
  double *out; \
  cnt = xlength (io->rxp); \
  in = (ITYPE *) io->buf; \
  out = (double *) io->buf; \
  if (io->scale) { \
    factor = *(io->scale); \
  } else { \
    factor = 1.0; \
  } \
  if (io->add) { \
    offset = *(io->add); \
  } else { \
    offset = 0.0; \
  } \
  if (io->fill) { \
    fillval = *(io->fill); \
    if (fillval != fillval) { \
      for (ii=cnt-1; ii>=0; ii--) { \
	if (in[ii] != in[ii]) { \
	  out[ii] = NA_REAL; \
	} else { \
	  out[ii] = in[ii] * factor + offset; \
	} \
      } \
    } else { \
      for (ii=cnt-1; ii>=0; ii--) { \
	if (in[ii] == fillval) { \
	  out[ii] = NA_REAL; \
	} else { \
	  out[ii] = in[ii] * factor + offset; \
	} \
      } \
    } \
  } else { \
    for (ii=cnt-1; ii>=0; ii--) { \
      out[ii] = in[ii] * factor + offset; \
    } \
  } \
}

R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_schar, signed char);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_uchar, unsigned char);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_short, short);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_ushort, unsigned short);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_int, int);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_uint, unsigned int);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_float, float);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_dbl, double);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_int64, long long);
R_NC_C2R_NUM_UNPACK(R_nc_c2r_unpack_uint64, unsigned long long);


/*=============================================================================*\
 *  Generic type conversions
\*=============================================================================*/

const void *
R_nc_r2c (SEXP rv, int ncid, nc_type xtype, int ndim, const size_t *xdim,
          const void *fill, const double *scale, const double *add)
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
      cv = R_nc_raw_char (rv, ndim, xdim);
    } else {
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  default:
    R_nc_error (RNC_EDATATYPE);
  }
  return cv;
}


void * \
R_nc_c2r_init (R_nc_buf *io, int ncid, nc_type xtype, int ndim, const size_t *xdim,
               int rawchar, int fitnum,
               const void *fill, const double *scale, const double *add)
{
  /* Initialise the R_nc_buf */
  io->rxp = NULL;
  io->buf = NULL;
  io->xtype = xtype;
  io->ncid = ncid;
  io->ndim = ndim;
  io->rawchar = rawchar;
  io->fitnum = fitnum;
  io->xdim = xdim;
  io->fill = fill;
  io->scale = scale;
  io->add = add;

  switch (xtype) {
    case NC_BYTE:
    case NC_UBYTE:
    case NC_SHORT:
    case NC_USHORT:
    case NC_INT:
      if (fitnum && !scale && !add) {
        R_nc_c2r_int_init (io);
        break;
      }
    case NC_INT64:
    case NC_UINT64:
      if (fitnum && !scale && !add) {
        R_nc_c2r_bit64_init (io);
        break;
      }
    case NC_UINT:
    case NC_FLOAT:
    case NC_DOUBLE:
      io = R_nc_c2r_dbl_init (io);
      break;
    case NC_CHAR:
      if (rawchar) {
        io = R_nc_char_raw_init (io);
      } else {
        io = R_nc_char_strsxp_init (io);
      }
      break;
    case NC_STRING:
      io = R_nc_str_strsxp_init (io);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
  }
  return io->buf;
}


SEXP
R_nc_c2r (R_nc_buf *io)
{
  int unpack;

  unpack = (io->scale || io->add);

  /* Type conversions */
  switch (io->xtype) {
    case NC_BYTE:
      if (unpack) {
        R_nc_c2r_unpack_schar (io);
      } else if (fitnum) {
        R_nc_c2r_schar_int (io);
      } else {
        R_nc_c2r_schar_dbl (io);
      }
      break;
    case NC_UBYTE:
      if (unpack) {
        R_nc_c2r_unpack_uchar (io);
      } else if (fitnum) {
        R_nc_c2r_uchar_int (io);
      } else {
        R_nc_c2r_uchar_dbl (io);
      }
      break;
    case NC_SHORT:
      if (unpack) {
        R_nc_c2r_unpack_short (io);
      } else if (fitnum) {
        R_nc_c2r_short_int (io);
      } else {
        R_nc_c2r_short_dbl (io);
      }
      break;
    case NC_USHORT:
      if (unpack) {
        R_nc_c2r_unpack_ushort (io);
      } else if (fitnum) {
        R_nc_c2r_ushort_int (io);
      } else {
        R_nc_c2r_ushort_dbl (io);
      }
      break;
    case NC_INT:
      if (unpack) {
        R_nc_c2r_unpack_int (io);
      } else if (fitnum) {
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
      } else if (fitnum) {
        R_nc_c2r_int64_bit64 (io);
      } else {
        R_nc_c2r_int64_dbl (io);
      }
      break;
    case NC_UINT64:
      if (unpack) {
        R_nc_c2r_unpack_uint64 (io);
      } else if (fitnum) {
        R_nc_c2r_uint64_bit64 (io);
      } else {
        R_nc_c2r_uint64_dbl (io);
      }
      break;
    case NC_CHAR:
      if (rawchar) {
        R_nc_char_raw (io);
      } else {
        R_nc_char_strsxp (io);
      }
      break;
    case NC_STRING:
      R_nc_str_strsxp (io);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
  }
  return io->rxp;
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


