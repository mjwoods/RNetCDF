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


/*=============================================================================*\
 *  String conversions and other operations.
\*=============================================================================*/


void
R_nc_strsxp_char (SEXP rstr, char *carr, size_t imin, size_t cnt,
                  size_t strlen)
{
  size_t ii;
  char *thisstr;
  for (ii=imin, thisstr=carr; ii<(imin+cnt); ii++, thisstr+=strlen) {
    strncpy(thisstr, CHAR( STRING_ELT (rstr, ii)), strlen);
  }
}


void
R_nc_char_strsxp (char *carr, SEXP rstr,
                  size_t len, size_t imin, size_t cnt)
{
  size_t ii;
  char *thisstr, *endstr, endchar;
  size_t rlen;
  rlen = (len <= RNC_CHARSXP_MAXLEN) ? len : RNC_CHARSXP_MAXLEN;
  for (ii=imin, thisstr=carr; ii<(imin+cnt); ii++, thisstr+=len) {
    /* Temporarily null-terminate each string before passing to R */
    endstr = thisstr + rlen;
    endchar = *endstr;
    *endstr = '\0';
    SET_STRING_ELT (rstr, ii, mkChar(thisstr));
    *endstr = endchar;
  }
}


void
R_nc_strsxp_str (SEXP rstr, const char **cstr, size_t imin, size_t cnt)
{
  size_t ii, jj;
  for (ii=0, jj=imin; ii<cnt; ii++, jj++) {
    cstr[ii] = CHAR( STRING_ELT (rstr, jj));
  }
}


void
R_nc_str_strsxp (char **cstr, SEXP rstr, size_t imin, size_t cnt)
{
  size_t ii, jj;
  size_t nchar;
  char *endstr, endchar;
  for (ii=0, jj=imin; ii<cnt; ii++, jj++) {
    nchar = strlen (cstr[ii]);
    if (nchar > RNC_CHARSXP_MAXLEN) {
      /* Temporarily truncate excessively long strings before passing to R */
      endstr = cstr[ii]+RNC_CHARSXP_MAXLEN+1;
      endchar = *endstr;
      *endstr = '\0';
      SET_STRING_ELT (rstr, jj, mkChar (cstr[ii]));
      *endstr = endchar;
    } else if (nchar > 0) {
      SET_STRING_ELT (rstr, jj, mkChar (cstr[ii]));
    }
  }
}


#define R_NC_R2C_STR_NUM(FUN, OTYPE, STRTONUM) \
static void \
FUN (SEXP rstr, OTYPE *out, size_t imin, size_t cnt, OTYPE *fill) \
{ \
  size_t ii, jj; \
  const char *charptr; \
  char *endptr; \
  SEXP charsxp; \
  for (ii=0, jj=imin; ii<cnt; ii++, jj++) { \
    charsxp = STRING_ELT (rstr, jj); \
    if (fill && charsxp == NA_STRING) { \
      out[ii] = *fill; \
    } else { \
      charptr = CHAR (charsxp); \
      errno = 0; \
      out[ii] = STRTONUM (charptr, &endptr, 10); \
      if (endptr == charptr || *endptr != '\0' || errno != 0) { \
        R_nc_error (nc_strerror (NC_ERANGE)); \
      } \
    } \
  } \
}

R_NC_R2C_STR_NUM(R_nc_strsxp_int64, long long, strtoll);
R_NC_R2C_STR_NUM(R_nc_strsxp_uint64, unsigned long long, strtoull);


#define R_NC_C2R_NUM_STR(FUN, ITYPE, STRFMT) \
static void \
FUN (ITYPE *in, SEXP rstr, size_t imin, size_t cnt, \
     ITYPE *fill, ITYPE *min, ITYPE *max) \
{ \
  size_t ii, jj; \
  char chartmp[RNC_DBL_DIG]; \
  for (ii=0, jj=imin; ii<cnt; ii++, jj++) { \
    if ((in[ii] != in[ii]) || \
        (fill && *fill == in[ii]) || \
        (min && *min > in[ii]) || \
        (max && *max < in[ii])) { \
      SET_STRING_ELT (rstr, jj, NA_STRING); \
    } else if (sprintf (chartmp, STRFMT, in[ii]) > 0) { \
      SET_STRING_ELT (rstr, jj, mkChar (chartmp)); \
    } else { \
      SET_STRING_ELT (rstr, jj, NA_STRING); \
    } \
  } \
}

R_NC_C2R_NUM_STR(R_nc_int64_strsxp, long long, RNC_FMT_LL);
R_NC_C2R_NUM_STR(R_nc_uint64_strsxp, unsigned long long, RNC_FMT_ULL);


/*=============================================================================*\
 *  Numeric type conversions
\*=============================================================================*/

#define R_NC_ISNA_INT(value) (value==NA_INTEGER)
#define R_NC_ISNA_REAL(value) (ISNAN(value))
#define R_NC_RANGE_MIN(VAL,LIM,TYPE) ((TYPE) LIM <= (TYPE) VAL)
#define R_NC_RANGE_MAX(VAL,LIM,TYPE) ((TYPE) VAL <= (TYPE) LIM)
#define R_NC_RANGE_NONE(VAL,LIM,TYPE) (1)

#define R_NC_R2C_NUM(FUN, ITYPE, OTYPE, NATEST, MINTEST, MINVAL, MAXTEST, MAXVAL) \
static void \
FUN (const ITYPE* restrict in, OTYPE* restrict out, size_t cnt, \
     OTYPE *fill, double *scale, double *add) \
{ \
  size_t ii, erange=0; \
  double factor, offset; \
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
  for (ii=0; ii<cnt; ii++) { \
    if (fill && NATEST(in[ii])) { \
      out[ii] = *fill; \
    } else if (MINTEST(in[ii],MINVAL,ITYPE) && MAXTEST(in[ii],MAXVAL,ITYPE)) { \
      if (scale || add) { \
	out[ii] = round((in[ii] - offset) / factor); \
      } else { \
	out[ii] = in[ii]; \
      } \
    } else { \
      erange = 1; \
    } \
  } \
  if ( erange ) { \
    R_nc_error (nc_strerror (NC_ERANGE)); \
  } \
}

R_NC_R2C_NUM(R_nc_r2c_int_schar, int, signed char, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_uchar, int, unsigned char, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_short, int, short, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_ushort, int, unsigned short, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_int, int, int, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_uint, int, unsigned int, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_ll, int, long long, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_ull, int, unsigned long long, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_size, int, size_t, \
  R_NC_ISNA_INT, R_NC_RANGE_MIN, 0, R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_float, int, float, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );
R_NC_R2C_NUM(R_nc_r2c_int_dbl, int, double, \
  R_NC_ISNA_INT, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );

R_NC_R2C_NUM(R_nc_r2c_dbl_schar, double, signed char, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, SCHAR_MIN, R_NC_RANGE_MAX, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uchar, double, unsigned char, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_short, double, short, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, SHRT_MIN, R_NC_RANGE_MAX, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ushort, double, unsigned short, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_int, double, int, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, INT_MIN, R_NC_RANGE_MAX, INT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uint, double, unsigned int, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, UINT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ll, double, long long, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, LLONG_MIN_DBL, R_NC_RANGE_MAX, LLONG_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_ull, double, unsigned long long, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, ULLONG_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_size, double, size_t, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, 0, R_NC_RANGE_MAX, SIZE_MAX_DBL);
R_NC_R2C_NUM(R_nc_r2c_dbl_float, double, float, \
  R_NC_ISNA_REAL, R_NC_RANGE_MIN, -FLT_MAX, R_NC_RANGE_MAX, FLT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_dbl, double, double, \
  R_NC_ISNA_REAL, R_NC_RANGE_NONE, , R_NC_RANGE_NONE, );


#define R_NC_C2R_NUM(FUN, ITYPE, OTYPE, MISSVAL) \
static void \
FUN (const ITYPE* restrict in, OTYPE* restrict out, size_t cnt, \
     ITYPE *fill, ITYPE *min, ITYPE *max, double *scale, double *add) \
{ \
  size_t ii; \
  double factor, offset; \
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
  for (ii=0; ii<cnt; ii++) { \
    if ((in[ii] != in[ii]) || \
        (fill && *fill == in[ii]) || \
        (min && *min > in[ii]) || \
        (max && *max < in[ii])) { \
      out[ii] = MISSVAL; \
    } else if (scale || add) { \
      out[ii] = in[ii] * factor + offset; \
    } else { \
      out[ii] = in[ii]; \
    } \
  } \
}

R_NC_C2R_NUM(R_nc_c2r_schar_int, signed char, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_uchar_int, unsigned char, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_short_int, short, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_ushort_int, unsigned short, int, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_int_int, int, int, NA_INTEGER);

R_NC_C2R_NUM(R_nc_c2r_schar_dbl, signed char, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uchar_dbl, unsigned char, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_short_dbl, short, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_ushort_dbl, unsigned short, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_int_dbl, int, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uint_dbl, unsigned int, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_float_dbl, float, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_dbl_dbl, double, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_int64_dbl, long long, double, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_uint64_dbl, unsigned long long, double, NA_REAL);


void *
R_nc_r2c (SEXP rv, void *cv, size_t imin, size_t cnt, nc_type xtype,
          void *fill, double *scale, double *add)
{
  int *intp=NULL;
  double *realp=NULL;

  /* Allocate a C vector if cv is NULL */
  if (cv == NULL) {
    switch (xtype) {
      case NC_BYTE:
	cv = R_alloc(cnt, sizeof(signed char));
	break;
      case NC_UBYTE:
	cv = R_alloc(cnt, sizeof(unsigned char));
	break;
      case NC_SHORT:
	cv = R_alloc(cnt, sizeof(short));
	break;
      case NC_USHORT:
	cv = R_alloc(cnt, sizeof(unsigned short));
	break;
      case NC_INT:
	cv = R_alloc(cnt, sizeof(int));
	break;
      case NC_UINT:
	cv = R_alloc(cnt, sizeof(unsigned int));
	break;
      case NC_FLOAT:
	cv = R_alloc(cnt, sizeof(float));
	break;
      case NC_DOUBLE:
	cv = R_alloc(cnt, sizeof(double));
	break;
      case NC_INT64:
	cv = R_alloc(cnt, sizeof(long long));
	break;
      case NC_UINT64:
	cv = R_alloc(cnt, sizeof(unsigned long long));
	break;
      default:
	R_nc_error (RNC_ETYPEDROP);
    }
  }

  if (isInteger(rv)) {
    intp = &(INTEGER(rv)[imin]);
    switch (xtype) {
    case NC_BYTE:
      R_nc_r2c_int_schar (intp, cv, cnt, fill, scale, add);
      break;
    case NC_UBYTE:
      R_nc_r2c_int_uchar (intp, cv, cnt, fill, scale, add);
      break;
    case NC_SHORT:
      R_nc_r2c_int_short (intp, cv, cnt, fill, scale, add);
      break;
    case NC_USHORT:
      R_nc_r2c_int_ushort (intp, cv, cnt, fill, scale, add);
      break;
    case NC_INT:
      R_nc_r2c_int_int (intp, cv, cnt, fill, scale, add);
      break;
    case NC_UINT:
      R_nc_r2c_int_uint (intp, cv, cnt, fill, scale, add);
      break;
    case NC_INT64:
      R_nc_r2c_int_ll (intp, cv, cnt, fill, scale, add);
      break;
    case NC_UINT64:
      R_nc_r2c_int_ull (intp, cv, cnt, fill, scale, add);
      break;
    case NC_FLOAT:
      R_nc_r2c_int_float (intp, cv, cnt, fill, scale, add);
      break;
    case NC_DOUBLE:
      R_nc_r2c_int_dbl (intp, cv, cnt, fill, scale, add);
      break;
    default:
      R_nc_error (RNC_EDATATYPE);
    }
  } else if (isReal(rv)) {
    realp = &(REAL(rv)[imin]);
    switch (xtype) {
    case NC_BYTE:
      R_nc_r2c_dbl_schar (realp, cv, cnt, fill, scale, add);
      break;
    case NC_UBYTE:
      R_nc_r2c_dbl_uchar (realp, cv, cnt, fill, scale, add);
      break;
    case NC_SHORT:
      R_nc_r2c_dbl_short (realp, cv, cnt, fill, scale, add);
      break;
    case NC_USHORT:
      R_nc_r2c_dbl_ushort (realp, cv, cnt, fill, scale, add);
      break;
    case NC_INT:
      R_nc_r2c_dbl_int (realp, cv, cnt, fill, scale, add);
      break;
    case NC_UINT:
      R_nc_r2c_dbl_uint (realp, cv, cnt, fill, scale, add);
      break;
    case NC_INT64:
      R_nc_r2c_dbl_ll (realp, cv, cnt, fill, scale, add);
      break;
    case NC_UINT64:
      R_nc_r2c_dbl_ull (realp, cv, cnt, fill, scale, add);
      break;
    case NC_FLOAT:
      R_nc_r2c_dbl_float (realp, cv, cnt, fill, scale, add);
      break;
    case NC_DOUBLE:
      R_nc_r2c_dbl_dbl (realp, cv, cnt, fill, scale, add);
      break;
    default:
      R_nc_error (RNC_EDATATYPE);
    }
  } else if (isString(rv)) {
    /* Note that packing is not currently supported for string conversions */
    switch (xtype) {
    case NC_INT64:
      R_nc_strsxp_int64 (rv, cv, imin, cnt, fill);
      break;
    case NC_UINT64:
      R_nc_strsxp_uint64 (rv, cv, imin, cnt, fill);
      break;
    default:
      R_nc_error (RNC_EDATATYPE);
    }
  }
  return cv;
}


SEXP
R_nc_c2r (void *cv, size_t imin, size_t cnt, nc_type xtype, int fitnum,
          void *fill, void *min, void *max, double *scale, double *add)
{
  int *intp=NULL;
  double *realp=NULL;
  SEXP rv=NULL;

  /* Allocate an R vector, get pointer to start of data */
  switch (xtype) {
    case NC_BYTE:
    case NC_UBYTE:
    case NC_SHORT:
    case NC_USHORT:
    case NC_INT:
      if (fitnum == TRUE) {
        rv = R_nc_protect (allocVector (INTSXP, cnt));
        intp = &(INTEGER(rv)[imin]);
        break;
      }
    case NC_INT64:
    case NC_UINT64:
      if (fitnum == TRUE) {
        rv = R_nc_protect (allocVector (STRSXP, cnt));
        break;
      }
    case NC_UINT:
    case NC_FLOAT:
    case NC_DOUBLE:
      rv = R_nc_protect (allocVector (REALSXP, cnt));
      realp = &(REAL(rv)[imin]);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
  }

  /* Type conversions */
  switch (xtype) {
    case NC_BYTE:
      if (fitnum == TRUE) {
        R_nc_c2r_schar_int (cv, intp, cnt, fill, min, max, scale, add);
      } else {
        R_nc_c2r_schar_dbl (cv, realp, cnt, fill, min, max, scale, add);
      }
      break;
    case NC_UBYTE:
      if (fitnum == TRUE) {
        R_nc_c2r_uchar_int (cv, intp, cnt, fill, min, max, scale, add);
      } else {
        R_nc_c2r_uchar_dbl (cv, realp, cnt, fill, min, max, scale, add);
      }
      break;
    case NC_SHORT:
      if (fitnum == TRUE) {
        R_nc_c2r_short_int (cv, intp, cnt, fill, min, max, scale, add);
      } else {
        R_nc_c2r_short_dbl (cv, realp, cnt, fill, min, max, scale, add);
      }
      break;
    case NC_USHORT:
      if (fitnum == TRUE) {
        R_nc_c2r_ushort_int (cv, intp, cnt, fill, min, max, scale, add);
      } else {
        R_nc_c2r_ushort_dbl (cv, realp, cnt, fill, min, max, scale, add);
      }
      break;
    case NC_INT:
      if (fitnum == TRUE) {
        R_nc_c2r_int_int (cv, intp, cnt, fill, min, max, scale, add);
      } else {
        R_nc_c2r_int_dbl (cv, realp, cnt, fill, min, max, scale, add);
      }
      break;
    case NC_UINT:
      R_nc_c2r_uint_dbl (cv, realp, cnt, fill, min, max, scale, add);
      break;
    case NC_FLOAT:
      R_nc_c2r_float_dbl (cv, realp, cnt, fill, min, max, scale, add);
      break;
    case NC_DOUBLE:
      R_nc_c2r_dbl_dbl (cv, realp, cnt, fill, min, max, scale, add);
      break;
    case NC_INT64:
      if (fitnum == TRUE) {
        R_nc_int64_strsxp (cv, rv, imin, cnt, fill, min, max);
      } else {
        R_nc_c2r_int64_dbl (cv, realp, cnt, fill, min, max, scale, add);
      }
      break;
    case NC_UINT64:
      if (fitnum == TRUE) {
        R_nc_uint64_strsxp (cv, rv, imin, cnt, fill, min, max);
      } else {
        R_nc_c2r_uint64_dbl (cv, realp, cnt, fill, min, max, scale, add);
      }
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
  }
  return rv;
}


/* Reverse a vector in-place.
   Example: R_nc_rev_int (cv, cnt);
 */
#define R_NC_REVERSE(FUN, TYPE) \
void \
FUN (TYPE *data, size_t cnt) \
{ \
  size_t ii, jj; \
  TYPE tmp; \
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


