/*=============================================================================*\
 *									       *
 *  Name:       RNetCDF.c						       *
 *									       *
 *  Version:    2.0-1							       *
 *									       *
 *  Purpose:    NetCDF interface for R.					       *
 *									       *
 *  Author:     Pavel Michna (michna@giub.unibe.ch)			       *
 *              Milton Woods (m.woods@bom.gov.au)                              *
 *									       *
 *  Copyright:  (C) 2004-2016 Pavel Michna                                     *
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
 *  Author   Date       Description					       *
 *  ------   ----       -----------					       *
 *  pm       12/06/04   First implementation				       *
 *  pm       09/07/04   Support scalar variables   		               *
 *  pm       21/07/04   Changed error handling	                               *
 *  pm       03/01/05   Corrected minor bugs	                               *
 *  pm       25/07/06   Changed SET_STRING_ELT to SET_VECTOR_ELT               *
 *  mw       14/04/08   Changed nc_redef and nc_enddef usage                   *
 *                      to avoid unnecessary data movement within a file       *
 *  mw       14/04/08   Added new modes (large, prefill, share) to             *
 *                      functions nc_open and nc_create                        *
 *  pm       24/11/10   Restored nc_redef and nc_enddef usage and added        *
 *                      enddef option for having the same effect               *
 *  pm       01/12/10   Removed argument SEXP enddef, checking for NC_DEFINE   *
 *  pm       03/12/10   Minor bug corrections at possible memory leaks         *
 *  pm       15/12/10   Minor bug corrections                                  *
 *  pm       25/12/10   Added UDUNITS-2 message override handling (R_ut_init)  *
 *  pm       04/01/11   Corrected string handling in R_nc_get_vara_text        *
 *  pm       05/01/11   Removed extra zeroing after Calloc                     *
 *  pm       26/05/14   Corrected memory leak issue (lines 1338 and 1593)      *
 *  mw       05/09/14   Support reading and writing raw character arrays,      *
 *                      avoid temporary arrays when reading/writing variables  *
 *  mw       08/09/14   Handle reading and writing of zero-sized arrays        *
 *  mw       01/02/15   Remove redundant ut_read_xml from R_ut_init            *
 *  mw       24/04/15   Initialise and free utunit when using udunits2,        *
 *                      to fix memory errors reported by valgrind.             *
 *                      Allow udunits2 headers to be in udunits2 directory.    *
 *  mw       26/01/16   Fix memory leak from abnormal exit of calendar funcs.  *
 *  mw       24/02/16   Support creation of files in netcdf4 (hdf5) format.    *
 *  mw       21/05/16   Add functions for netcdf4 groups.                      *
 *									       *
\*=============================================================================*/


/*=============================================================================*\
 *  Includes								       *
\*=============================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <float.h>

#include <netcdf.h>

#ifdef HAVE_UDUNITS2_UDUNITS_H
#include <udunits2/udunits.h>
#else
#include <udunits.h>
#endif

#include <R.h>
#include <Rinternals.h>

/*=============================================================================*\
 *  Local macros, constants and variables
\*=============================================================================*/

#define NA_SIZE ((size_t) -1)

#define RNC_CHARSXP_MAXLEN 2147483647

#define RRETURN(object) { R_nc_unprotect (); return (object); }

#define RERROR(msg) { R_nc_error (msg); return R_NilValue; }

static int R_nc_protect_count = 0;

static const char RNC_EDATALEN[]="Not enough data", \
  RNC_EDATATYPE[]="Incompatible data for external type", \
  RNC_ERANGE[]="Value out of range for type conversion", \
  RNC_ETYPEDROP[]="Unsupported external type";

/*=============================================================================*\
 *  R house-keeping functions
\*=============================================================================*/

/* Protect an object from garbage collection by R */
static SEXP
R_nc_protect (SEXP obj)
{
  PROTECT(obj);
  R_nc_protect_count++;
  return obj;
}


/* Unprotect all objects from garbage collection by R */
static void
R_nc_unprotect (void) {
  if (R_nc_protect_count > 0) {
    UNPROTECT (R_nc_protect_count);
    R_nc_protect_count = 0;
  }
}


/* Raise an error in R */
static void
R_nc_error(const char *msg)
{
  R_nc_unprotect ();
  error ("%s", msg);
}


/*=============================================================================*\
 *  String conversions and other operations.
\*=============================================================================*/


/* Convert R strings to char array.
   Argument rstr is an R string vector with cnt indices from imin.
   Argument carr provides space for at least cnt*strlen bytes.
   Strings are copied from rstr to carr,
   trimming or padding each string with null characters to length strlen.
 */
static SEXP
R_nc_strsxp_char (SEXP rstr, char *carr, R_xlen_t imin, R_xlen_t cnt,
                  size_t strlen)
{
  R_xlen_t ii;
  char *thisstr;
  for (ii=imin, thisstr=carr; ii<(imin+cnt); ii++, thisstr+=strlen) {
    strncpy(thisstr, CHAR( STRING_ELT (rstr, ii)), strlen);
  }
  return R_NilValue;
}

/* Convert a char array to R strings.
   Argument carr is assumed to contain cnt strings of maximum length len,
   its allocated size must be at least len*cnt+1 bytes,
   and its contents are modified during execution but restored on return.
   Argument rstr is an R string vector with length cnt from index imin.
 */
static SEXP
R_nc_char_strsxp (char *carr, SEXP rstr,
                  R_xlen_t len, R_xlen_t imin, R_xlen_t cnt)
{
  R_xlen_t ii, rlen;
  char *thisstr, *endstr, endchar;
  rlen = (len <= RNC_CHARSXP_MAXLEN) ? len : RNC_CHARSXP_MAXLEN;
  for (ii=imin, thisstr=carr; ii<(imin+cnt); ii++, thisstr+=len) {
    /* Temporarily null-terminate each string before passing to R */
    endstr = thisstr + rlen;
    endchar = *endstr;
    *endstr = '\0';
    SET_STRING_ELT (rstr, ii, mkChar(thisstr));
    *endstr = endchar;
  }
  return R_NilValue;
}


/* Convert R strings to char ragged array.
   Argument rstr is an R string vector with cnt indices from imin.
   Argument cstr provides space for at least cnt pointers,
   which will be set to the address of each R string on return.
 */
static SEXP
R_nc_strsxp_str (SEXP rstr, const char **cstr, R_xlen_t imin, R_xlen_t cnt)
{
  R_xlen_t ii;
  for (ii=imin; ii<(imin+cnt); ii++) {
    cstr[ii] = CHAR( STRING_ELT (rstr, ii));
  }
  return R_NilValue;
}


/* Convert a char ragged array to R strings.
   Argument cstr is assumed to contain cnt pointers to null-terminated strings.
   Argument rstr is an R string vector with length cnt from index imin.
 */
static SEXP
R_nc_str_strsxp (char **cstr, SEXP rstr, R_xlen_t imin, R_xlen_t cnt)
{
  R_xlen_t ii, nchar;
  char *endstr, endchar;
  for (ii=imin; ii<(imin+cnt); ii++) {
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
  return R_NilValue;
}


/* Convert R strings to int64 array.
   Argument rstr is an R string vector with cnt indices from imin.
   Argument cint64 contains cnt values of type long long on return.
   Any element of cint64 that cannot be converted from rstr is set to *fill,
   or NC_FILL_INT64 if fill is NULL.
 */
static SEXP
R_nc_strsxp_int64 (SEXP rstr, long long *cint64,
                   R_xlen_t imin, R_xlen_t cnt, long long *fill)
{
  R_xlen_t ii;
  const char *charptr;
  char *endptr;
  long long fillval;
  if (fill == NULL) {
    fillval = NC_FILL_INT64;
  } else {
    fillval = *fill;
  }
  errno = 0;
  for (ii=imin; ii<(imin+cnt); ii++) {
    charptr = CHAR (STRING_ELT (rstr, ii));
    cint64[ii] = strtoll (charptr, &endptr, 10);
    if (endptr == charptr || *endptr != '\0' || errno != 0) {
      cint64[ii] = fillval;
    }
  }
  return R_NilValue;
}


/* Convert an int64 array to R strings.
   Argument cint64 contains cnt values of type long long.
   Argument rstr is an R string vector with length cnt from index imin.
   Any element of cint64 with value *fill (or NC_FILL_INT64 if fill is NULL)
   will be converted to the string "NA".
 */
static SEXP
R_nc_int64_strsxp (long long *cint64, SEXP rstr,
                   R_xlen_t imin, R_xlen_t cnt, long long *fill)
{
  R_xlen_t ii;
  char chartmp[24];
  long long fillval;
  if (fill == NULL) {
    fillval = NC_FILL_INT64;
  } else {
    fillval = *fill;
  }
  for (ii=imin; ii<(imin+cnt); ii++) {
    if ((cint64[ii] != fillval) &&
        (sprintf (chartmp, "%lli", cint64[ii]) > 0)) {
      SET_STRING_ELT (rstr, ii, mkChar (chartmp));
    } else {
      SET_STRING_ELT (rstr, ii, NA_STRING);
    }
  }
  return R_NilValue;
}


/* Convert R strings to uint64 array.
   Argument rstr is an R string vector with cnt indices from imin.
   Argument cuint64 contains cnt values of type unsigned long long on return.
   Any element of cuint64 that cannot be converted from rstr is set to *fill,
   or NC_FILL_UINT64 if fill is NULL.
 */
static SEXP
R_nc_strsxp_uint64 (SEXP rstr, unsigned long long *cuint64,
                    R_xlen_t imin, R_xlen_t cnt, unsigned long long *fill)
{
  R_xlen_t ii;
  const char *charptr;
  char *endptr;
  unsigned long long fillval;
  errno = 0;
  if (fill == NULL) {
    fillval = NC_FILL_UINT64;
  } else {
    fillval = *fill;
  }
  for (ii=imin; ii<(imin+cnt); ii++) {
    charptr = CHAR (STRING_ELT (rstr, ii));
    cuint64[ii] = strtoull (charptr, &endptr, 10);
    if (endptr == charptr || *endptr != '\0' || errno != 0) {
      cuint64[ii] = fillval;
    }
  }
  return R_NilValue;
}


/* Convert a uint64 array to R strings.
   Argument cuint64 contains cnt values of type unsigned long long.
   Argument rstr is an R string vector with length cnt.
   Any element of cuint64 with value *fill (or NC_FILL_UINT64 if fill is NULL)
   will be converted to the string "NA".
 */
static SEXP
R_nc_uint64_strsxp (unsigned long long *cuint64, SEXP rstr,
                    R_xlen_t imin, R_xlen_t cnt, unsigned long long *fill)
{
  size_t ii;
  char chartmp[24];
  unsigned long long fillval;
  if (fill == NULL) {
    fillval = NC_FILL_UINT64;
  } else {
    fillval = *fill;
  }
  for (ii=imin; ii<(imin+cnt); ii++) {
    if ((cuint64[ii] != fillval) &&
        (sprintf (chartmp, "%llu", cuint64[ii]) > 0)) {
      SET_STRING_ELT (rstr, ii, mkChar (chartmp));
    } else {
      SET_STRING_ELT (rstr, ii, NA_STRING);
    }
  }
  return R_NilValue;
}


/* Determine if a C string matches the first element of an R variable.
   Result is a logical value. */
static int
R_nc_strcmp (SEXP var, const char *str)
{
  return (isString(var) &&
          xlength(var) >= 1 &&
          strcmp(CHAR (STRING_ELT (var, 0)), str) == 0);
}


/*=============================================================================*\
 *  Numeric conversion functions.
 *  These are used where conversions are not provided by NetCDF.
\*=============================================================================*/

/* Convert a vector of R numeric values to another type.
   Values that are out-of-range for the output type cause an R error.
   Missing values are replaced by the value of *fill,
   or if fill is NULL, by the default netcdf fill value.
   (We assume the optimising compiler can eliminate comparisons of constants).
   Example: R_nc_r2c_int_short (rv, cv, cnt, &fill);
 */
#define R_NC_R2C_NAINT (in[ii]==NA_INTEGER)
#define R_NC_R2C_NAREAL (!R_FINITE(in[ii]))
#define R_NC_R2C_NONE -1
#define R_NC_R2C_NUM(FUN, ITYPE, OTYPE, NATEST, FILLVAL, MINVAL, MAXVAL) \
static void \
FUN (ITYPE *in, OTYPE *out, size_t cnt, OTYPE *fill) \
{ \
  size_t ii; \
  OTYPE fillval; \
  if (fill == NULL) { \
    fillval = FILLVAL; \
  } else { \
    fillval = *fill; \
  } \
  for (ii=0; ii<cnt; ii++) { \
    if (NATEST) { \
      out[ii] = fillval; \
    } else if ((MINVAL != R_NC_R2C_NONE) && (in[ii] < MINVAL)) { \
      R_nc_error (RNC_ERANGE); \
    } else if ((MAXVAL != R_NC_R2C_NONE) && (in[ii] > MAXVAL)) { \
      R_nc_error (RNC_ERANGE); \
    } else { \
      out[ii] = in[ii]; \
    } \
  } \
}

R_NC_R2C_NUM(R_nc_r2c_int_schar, int, signed char, \
  R_NC_R2C_NAINT, NC_FILL_BYTE, SCHAR_MIN, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_uchar, int, unsigned char, \
  R_NC_R2C_NAINT, NC_FILL_UBYTE, 0, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_short, int, short, \
  R_NC_R2C_NAINT, NC_FILL_SHORT, SHRT_MIN, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_ushort, int, unsigned short, \
  R_NC_R2C_NAINT, NC_FILL_USHORT, 0, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_int, int, int, \
  R_NC_R2C_NAINT, NC_FILL_INT, R_NC_R2C_NONE, R_NC_R2C_NONE);
R_NC_R2C_NUM(R_nc_r2c_int_uint, int, unsigned int, \
  R_NC_R2C_NAINT, NC_FILL_UINT, 0, R_NC_R2C_NONE);
R_NC_R2C_NUM(R_nc_r2c_int_ll, int, long long, \
  R_NC_R2C_NAINT, NC_FILL_INT64, R_NC_R2C_NONE, R_NC_R2C_NONE);
R_NC_R2C_NUM(R_nc_r2c_int_ull, int, unsigned long long, \
  R_NC_R2C_NAINT, NC_FILL_UINT64, 0, R_NC_R2C_NONE);
R_NC_R2C_NUM(R_nc_r2c_int_size, int, size_t, \
  R_NC_R2C_NAINT, SIZE_MAX, 0, SIZE_MAX);
R_NC_R2C_NUM(R_nc_r2c_int_float, int, float, \
  R_NC_R2C_NAINT, NC_FILL_FLOAT, R_NC_R2C_NONE, R_NC_R2C_NONE);
R_NC_R2C_NUM(R_nc_r2c_int_dbl, int, double, \
  R_NC_R2C_NAINT, NC_FILL_DOUBLE, R_NC_R2C_NONE, R_NC_R2C_NONE);

R_NC_R2C_NUM(R_nc_r2c_dbl_schar, double, signed char, \
  R_NC_R2C_NAREAL, NC_FILL_BYTE, SCHAR_MIN, SCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uchar, double, unsigned char, \
  R_NC_R2C_NAREAL, NC_FILL_UBYTE, 0, UCHAR_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_short, double, short, \
  R_NC_R2C_NAREAL, NC_FILL_SHORT, SHRT_MIN, SHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ushort, double, unsigned short, \
  R_NC_R2C_NAREAL, NC_FILL_USHORT, 0, USHRT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_int, double, int, \
  R_NC_R2C_NAREAL, NC_FILL_INT, INT_MIN, INT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_uint, double, unsigned int, \
  R_NC_R2C_NAREAL, NC_FILL_UINT, 0, UINT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ll, double, long long, \
  R_NC_R2C_NAREAL, NC_FILL_INT64, LLONG_MIN, LLONG_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_ull, double, unsigned long long, \
  R_NC_R2C_NAREAL, NC_FILL_UINT64, 0, ULLONG_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_size, double, size_t, \
  R_NC_R2C_NAREAL, SIZE_MAX, 0, SIZE_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_float, double, float, \
  R_NC_R2C_NAREAL, NC_FILL_FLOAT, -FLT_MAX, FLT_MAX);
R_NC_R2C_NUM(R_nc_r2c_dbl_dbl, double, double, \
  R_NC_R2C_NAREAL, NC_FILL_DOUBLE, R_NC_R2C_NONE, R_NC_R2C_NONE);


/* Convert a vector of R numeric values to a netcdf external type.
   Values that are out-of-range for the output type cause an R error.
   Missing values are replaced by a fill value, which is either *fill
   or the default netcdf fill value if fill is NULL.
   Example: R_nc_r2c (rv, cv, cnt, xtype, &fill);
 */
static void
R_nc_r2c (SEXP rv, void *cv, size_t cnt, nc_type xtype, void *fill)
{
  int *intp;
  double *realp;
  if (isInteger(rv)) {
    intp = INTEGER (rv);
    switch (xtype) {
    case NC_BYTE:
      R_nc_r2c_int_schar (intp, cv, cnt, fill);
      break;
    case NC_UBYTE:
      R_nc_r2c_int_uchar (intp, cv, cnt, fill);
      break;
    case NC_SHORT:
      R_nc_r2c_int_short (intp, cv, cnt, fill);
      break;
    case NC_USHORT:
      R_nc_r2c_int_ushort (intp, cv, cnt, fill);
      break;
    case NC_INT:
      R_nc_r2c_int_int (intp, cv, cnt, fill);
      break;
    case NC_UINT:
      R_nc_r2c_int_uint (intp, cv, cnt, fill);
      break;
    case NC_INT64:
      R_nc_r2c_int_ll (intp, cv, cnt, fill);
      break;
    case NC_UINT64:
      R_nc_r2c_int_ull (intp, cv, cnt, fill);
      break;
    case NC_FLOAT:
      R_nc_r2c_int_float (intp, cv, cnt, fill);
      break;
    case NC_DOUBLE:
      R_nc_r2c_int_dbl (intp, cv, cnt, fill);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
    }
  } else if (isReal(rv)) {
    realp = REAL (rv);
    switch (xtype) {
    case NC_BYTE:
      R_nc_r2c_dbl_schar (realp, cv, cnt, fill);
      break;
    case NC_UBYTE:
      R_nc_r2c_dbl_uchar (realp, cv, cnt, fill);
      break;
    case NC_SHORT:
      R_nc_r2c_dbl_short (realp, cv, cnt, fill);
      break;
    case NC_USHORT:
      R_nc_r2c_dbl_ushort (realp, cv, cnt, fill);
      break;
    case NC_INT:
      R_nc_r2c_dbl_int (realp, cv, cnt, fill);
      break;
    case NC_UINT:
      R_nc_r2c_dbl_uint (realp, cv, cnt, fill);
      break;
    case NC_INT64:
      R_nc_r2c_dbl_ll (realp, cv, cnt, fill);
      break;
    case NC_UINT64:
      R_nc_r2c_dbl_ull (realp, cv, cnt, fill);
      break;
    case NC_FLOAT:
      R_nc_r2c_dbl_float (realp, cv, cnt, fill);
      break;
    case NC_DOUBLE:
      R_nc_r2c_dbl_dbl (realp, cv, cnt, fill);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
    }
  } else if (isString(rv)) {
    switch (xtype) {
      case NC_INT64:
        R_nc_strsxp_int64 (rv, cv, 0, cnt, fill);
      case NC_UINT64:
        R_nc_strsxp_uint64 (rv, cv, 0, cnt, fill);
      default:
        R_nc_error (RNC_ETYPEDROP);
    }
  } else {
    R_nc_error (RNC_EDATATYPE);
  }
}


/* Convert a vector of values to an R type.
   No range checks are performed, because we only convert to R types
   that can hold the full range of input values.
   A fill value is specified by *fill or the default netcdf fill value
   if fill is NULL, and this value is replaced by an R missing value.
   Example: R_nc_c2r_short_int (cv, rv, cnt, &fill);
 */
#define R_NC_C2R_NUM(FUN, ITYPE, OTYPE, FILLVAL, MISSVAL) \
static void \
FUN (ITYPE *in, OTYPE *out, size_t cnt, ITYPE *fill) \
{ \
  size_t ii; \
  ITYPE fillval; \
  if (fill == NULL) { \
    fillval = FILLVAL; \
  } else { \
    fillval = *fill; \
  } \
  for (ii=0; ii<cnt; ii++) { \
    if (in[ii] == fillval) { \
      out[ii] = MISSVAL; \
    } else { \
      out[ii] = in[ii]; \
    } \
  } \
}

R_NC_C2R_NUM(R_nc_c2r_schar_int, signed char, int, NC_FILL_BYTE, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_uchar_int, unsigned char, int, NC_FILL_UBYTE, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_short_int, short, int, NC_FILL_SHORT, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_ushort_int, unsigned short, int, NC_FILL_USHORT, NA_INTEGER);
R_NC_C2R_NUM(R_nc_c2r_int_int, int, int, NC_FILL_INT, NA_INTEGER);

R_NC_C2R_NUM(R_nc_c2r_uint_dbl, unsigned int, double, NC_FILL_UINT, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_float_dbl, float, double, NC_FILL_FLOAT, NA_REAL);
R_NC_C2R_NUM(R_nc_c2r_dbl_dbl, double, double, NC_FILL_DOUBLE, NA_REAL);


/* Convert a vector of netcdf external type to R numeric values.
   The output R vector is allocated inside the function,
   using the smallest R type that covers the range of the input type.
   A fill value is specified by *fill or the default netcdf fill value
   if fill is NULL, and this value is replaced by an R missing value.
   Example: R_nc_c2r (cv, rv, cnt, xtype, &fill);
 */
static void
R_nc_c2r (void *cv, SEXP rv, size_t cnt, nc_type xtype, void *fill)
{
  int *intp;
  double *realp;

  /* Allocate an R vector of the smallest type that can hold xtype */
  switch (xtype) {
    case NC_BYTE:
    case NC_UBYTE:
    case NC_SHORT:
    case NC_USHORT:
    case NC_INT:
      rv = R_nc_protect (allocVector (INTSXP, cnt));
      break;
    case NC_UINT:
    case NC_FLOAT:
    case NC_DOUBLE:
      rv = R_nc_protect (allocVector (REALSXP, cnt));
      break;
    case NC_INT64:
    case NC_UINT64:
      rv = R_nc_protect (allocVector (STRSXP, cnt));
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
  }

  if (isInteger(rv)) {
    intp = INTEGER (rv);
    switch (xtype) {
    case NC_BYTE:
      R_nc_c2r_schar_int (cv, intp, cnt, fill);
      break;
    case NC_UBYTE:
      R_nc_c2r_uchar_int (cv, intp, cnt, fill);
      break;
    case NC_SHORT:
      R_nc_c2r_short_int (cv, intp, cnt, fill);
      break;
    case NC_USHORT:
      R_nc_c2r_ushort_int (cv, intp, cnt, fill);
      break;
    case NC_INT:
      R_nc_c2r_int_int (cv, intp, cnt, fill);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
    }
  } else if (isReal(rv)) {
    realp = REAL (rv);
    switch (xtype) {
    case NC_UINT:
      R_nc_c2r_uint_dbl (cv, realp, cnt, fill);
      break;
    case NC_FLOAT:
      R_nc_c2r_float_dbl (cv, realp, cnt, fill);
      break;
    case NC_DOUBLE:
      R_nc_c2r_dbl_dbl (cv, realp, cnt, fill);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
    }
  } else if (isString(rv)) {
    switch (xtype) {
    case NC_INT64:
      R_nc_int64_strsxp (cv, rv, 0, cnt, fill);
      break;
    case NC_UINT64:
      R_nc_uint64_strsxp (cv, rv, 0, cnt, fill);
      break;
    default:
      R_nc_error (RNC_ETYPEDROP);
    }
  } else {
    R_nc_error (RNC_EDATATYPE);
  }
}


/*=============================================================================*\
 *  Private functions for netcdf interface
\*=============================================================================*/


/* Enter netcdf define mode if possible.
   Returns netcdf error code if an unhandled error occurs.
 */
static int
R_nc_redef (int ncid)
{
  int status;
  status = nc_redef(ncid);
  if (status == NC_EINDEFINE) {
    status = NC_NOERR;
  }
  return status;
}


/* Enter netcdf data mode if possible.
   Returns netcdf error code if an unhandled error occurs.
 */
static int
R_nc_enddef (int ncid)
{
  int status;
  status = nc_enddef(ncid);
  if (status == NC_ENOTINDEFINE) {
    status = NC_NOERR;
  }
  return status;
}


/* Convert netcdf type code to string label.
 */
static const char *
R_nc_type2str (int ncid, nc_type xtype)
{
  char *str;

  switch (xtype) {
  case NC_BYTE:
    return "NC_BYTE";
  case NC_UBYTE:
    return "NC_UBYTE";
  case NC_CHAR:
    return "NC_CHAR";
  case NC_SHORT:
    return "NC_SHORT";
  case NC_USHORT:
    return "NC_USHORT";
  case NC_INT:
    return "NC_INT";
  case NC_UINT:
    return "NC_UINT";
  case NC_INT64:
    return "NC_INT64";
  case NC_UINT64:
    return "NC_UINT64";
  case NC_FLOAT:
    return "NC_FLOAT";
  case NC_DOUBLE:
    return "NC_DOUBLE";
  case NC_STRING:
    return "NC_STRING";
  default:
    /* Try to get name of a user defined type */
    str = R_alloc (NC_MAX_NAME + 1, sizeof (char));
    if (nc_inq_user_type (ncid, xtype, str, NULL, NULL, NULL, NULL) ==
        NC_NOERR) {
      return str;
    } else {
      return "UNKNOWN";
    };
  }
}


/* Convert netcdf string label to type code.
   Return NC_NOERR if ok, netcdf error code otherwise.
 */
static int
R_nc_str2type (int ncid, const char *str, nc_type * xtype)
{
  size_t typelen;
  typelen = strlen (str);
  *xtype = NC_NAT;
  if (typelen >= 6) {
    switch (str[3]) {
    case 'B':
      if (strcmp (str, "NC_BYTE") == 0) {
        *xtype = NC_BYTE;
      }
      break;
    case 'C':
      if (strcmp (str, "NC_CHAR") == 0) {
        *xtype = NC_CHAR;
      }
      break;
    case 'D':
      if (strcmp (str, "NC_DOUBLE") == 0) {
        *xtype = NC_DOUBLE;
      }
      break;
    case 'F':
      if (strcmp (str, "NC_FLOAT") == 0) {
        *xtype = NC_FLOAT;
      }
      break;
    case 'I':
      switch (str[6]) {
      case '\0':
        if (strcmp (str, "NC_INT") == 0) {
          *xtype = NC_INT;
        }
        break;
      case '6':
        if (strcmp (str, "NC_INT64") == 0) {
          *xtype = NC_INT64;
        }
        break;
      }
      break;
    case 'L':
      if (strcmp (str, "NC_LONG") == 0) {
        *xtype = NC_LONG;
      }
      break;
    case 'S':
      switch (str[4]) {
      case 'H':
        if (strcmp (str, "NC_SHORT") == 0) {
          *xtype = NC_SHORT;
        }
        break;
      case 'T':
        if (strcmp (str, "NC_STRING") == 0) {
          *xtype = NC_STRING;
        }
        break;
      }
      break;
    case 'U':
      if (typelen >= 7) {
        switch (str[7]) {
        case '\0':
          if (strcmp (str, "NC_UINT") == 0) {
            *xtype = NC_UINT;
          }
          break;
        case '6':
          if (strcmp (str, "NC_UINT64") == 0) {
            *xtype = NC_UINT64;
          }
          break;
        case 'E':
          if (strcmp (str, "NC_UBYTE") == 0) {
            *xtype = NC_UBYTE;
          }
          break;
        case 'R':
          if (strcmp (str, "NC_USHORT") == 0) {
            *xtype = NC_USHORT;
          }
          break;
        }
      }
      break;
    }
  }

  if (*xtype == NC_NAT) {
    /* Try to get id of a user defined type */
    return nc_inq_typeid (ncid, str, xtype);
  } else {
    return NC_NOERR;
  }
}


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


/* Convert attribute identifier from R string or number to a C string.
   Argument attname must have space for NC_MAX_NAME+1 characters.
   Result is a netcdf status value.
 */
static int
R_nc_att_name (SEXP att, int ncid, int varid, char *attname)
{
  if (isNumeric (att)) {
    return nc_inq_attname (ncid, varid, asInteger (att), attname);
  } else if (isString (att)) {
    strcpy (attname, CHAR (STRING_ELT (att, 0)));
    return NC_NOERR;
  } else {
    return NC_EINVAL;
  }
}


/* Convert dimension identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
static int
R_nc_dim_id (SEXP dim, int ncid, int *dimid, int idx)
{
  if (isInteger (dim)) {
    *dimid = INTEGER (dim)[idx];
    return NC_NOERR;
  } else if (isReal (dim)) {
    *dimid = REAL (dim)[idx];
    return NC_NOERR;
  } else if (isString (dim)) {
    return nc_inq_dimid (ncid, CHAR (STRING_ELT (dim, idx)), dimid);
  } else {
    return NC_EINVAL;
  }
}


/* Convert variable identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
static int
R_nc_var_id (SEXP var, int ncid, int *varid)
{
  if (isNumeric (var)) {
    *varid = asInteger (var);
    return NC_NOERR;
  } else if (isString (var)) {
    return nc_inq_varid (ncid, CHAR (STRING_ELT (var, 0)), varid);
  } else {
    return NC_EINVAL;
  }
}


/* If status is a netcdf error, raise an R error with a suitable message,
   otherwise return to caller. */
static int
R_nc_check(int status)
{
  if (status != NC_NOERR) {
    R_nc_error (nc_strerror (status));
  }
  return status;
}


/* Copy the leading nr elements of R vector rv to C vector cv,
   converting type to size_t and reversing from Fortran to C storage order.
   Elements beyond the length of rv and non-finite values are stored as fillval.
 */
static void
R_nc_size_r2c(SEXP rv, size_t nr, size_t fillval, size_t *cv)
{
  double *realp;
  int *intp;
  size_t nc, ii;

  /* Number of elements to copy must not exceed length of rv */
  nc = xlength (rv);
  nc = (nr < nc) ? nr : nc;

  /* Copy elements */
  if (isReal (rv)) {
    realp = REAL (rv);
    for ( ii=0; ii<nc; ii++ ) {
      if (R_FINITE (realp[ii])) {
        cv[nr-1-ii] = realp[ii];
      } else {
        cv[nr-1-ii] = fillval;
      }
    }
  } else if (isInteger (rv)) {
    intp = INTEGER (rv);
    for ( ii=0; ii<nc; ii++ ) {
      if (intp[ii] == NA_INTEGER) {
        cv[nr-1-ii] = fillval;
      } else {
        cv[nr-1-ii] = intp[ii];
      }
    }
  } else {
    nc = 0;
  }

  /* Fill any remaining elements beyond length of rv */
  for ( ii=nc; ii<nr; ii++ ) {
    cv[nr-1-ii] = fillval;
  }

}


/* Handle NA values in user-specified variable slices.
   Store slice ranges in cstart and ccount vectors with C dimension order.
   The number of dimensions is returned in ndims,
   and both C vectors are allocated (via R_alloc) to length ndims.
   Result is a netcdf status value.
 */
static int
R_nc_slice (SEXP data, SEXP start, SEXP count, int ncid, int varid,
            int *ndims, size_t **cstart, size_t **ccount)
{
  int ii, status, *dimids, nr;
  nc_type xtype;
  size_t clen;
  SEXP datadim;

  /* Get type and dimension identifiers of the variable */
  status = nc_inq_var (ncid, varid, NULL, &xtype, ndims, NULL, NULL);
  if (status != NC_NOERR) {
    return(status);
  }

  if (*ndims <= 0) {
    /* Shortcut for scalar variables */
    return NC_NOERR;
  }

  dimids = (void *) R_alloc (*ndims, sizeof (int));

  status = nc_inq_vardimid (ncid, varid, dimids);
  if (status != NC_NOERR) {
    return(status);
  }

  /* Copy start indices from start to cstart,
     converting Fortran indices (1-based) to C (0-based)
     and reversing dimension order from Fortran to C.
     Default value for any missing dimension is 0,
     including the special case of start being NULL.
   */
  *cstart = (void *) R_alloc (*ndims, sizeof (size_t));
  R_nc_size_r2c (start, *ndims, 1, *cstart);
  for (ii=0; ii<*ndims; ii++) {
    (*cstart)[ii] -= 1;
  }
 
  /* Copy edge lengths from count to ccount,
     reversing dimension order from Fortran to C.
     In the special case of count being NULL,
     use dimensions of data and set any slower dimensions to 1,
     appending the fastest dimension for NC_CHAR variables if needed.
     Default for missing dimensions is to calculate edge length
     from start index to defined dimension length.
   */
  *ccount = (void *) R_alloc (*ndims, sizeof (size_t));
  for (ii=0; ii<*ndims; ii++) {
    (*ccount)[ii] = NA_SIZE;
  }

  if (isNull (count)) {
    if (!isNull (data)) {
      if (xtype == NC_CHAR) {
        nr = *ndims-1;
      } else {
        nr = *ndims;
      }
      datadim = getAttrib (data, R_DimSymbol);
      if (!isNull (datadim)) {
        R_nc_size_r2c (datadim, nr, 1, *ccount);
      } else {
        for (ii=0; ii<nr-1; ii++) {
          (*ccount)[ii] = 1;
        }
        (*ccount)[nr-1] = xlength (data);
      }
    }
  } else {
    R_nc_size_r2c (count, *ndims, NA_SIZE, *ccount);
  }

  /* Convert NA_SIZE in ccount so that corresponding dimensions are
     read/written from specified start index to the highest index.
   */
  for ( ii=0; ii<*ndims; ii++ ) {
    if ((*ccount)[ii] == NA_SIZE) {
      status = nc_inq_dimlen (ncid, dimids[ii], &clen);
      if (status != NC_NOERR) {
        return(status);
      }
      (*ccount)[ii] = clen - (*cstart)[ii];
    }
  }

  return(NC_NOERR);
}


/* Find total number of elements in an array from dimension lengths.
   Result is 1 for a scalar or product of dimensions for an array. */
static size_t
R_nc_length (int ndims, const size_t *count)
{
  int ii;
  size_t length;

  length = 1;
  for ( ii=0; ii<ndims; ii++ ) {
    length *= count[ii]; 
  }
  return (length);
}


/*=============================================================================*\
 *  Private functions for UDUNITS interface
\*=============================================================================*/


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


/*=============================================================================*\
 *  Public functions called by R for netcdf interface
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_nc_copy_att()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_copy_att (SEXP nc_in, SEXP var_in, SEXP att, SEXP nc_out, SEXP var_out)
{
  int ncid_in, ncid_out, varid_in, varid_out;
  char attname[NC_MAX_NAME+1];

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid_in = asInteger (nc_in);
  ncid_out = asInteger (nc_out);

  if (R_nc_strcmp(var_in, "NC_GLOBAL")) {
    varid_in = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var_in, ncid_in, &varid_in));
  }

  if (R_nc_strcmp(var_out, "NC_GLOBAL")) {
    varid_out = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var_out, ncid_out, &varid_out));
  }

  R_nc_check (R_nc_att_name (att, ncid_in, varid_in, attname));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid_out));

  /*-- Copy the attribute -----------------------------------------------------*/
  R_nc_check (nc_copy_att (ncid_in, varid_in, attname,
                           ncid_out, varid_out));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_delete_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_delete_att (SEXP nc, SEXP var, SEXP att)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  R_nc_check (R_nc_att_name (att, ncid, varid, attname));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Delete the attribute ---------------------------------------------------*/
  R_nc_check (nc_del_att (ncid, varid, attname));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  Private functions used by R_nc_get_att()                                   *
\*-----------------------------------------------------------------------------*/


/* Read NC_CHAR attribute as R raw */
static SEXP
R_nc_get_att_raw (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  result = R_nc_protect (allocVector (RAWSXP, cnt));
  if (cnt > 0) {
    R_nc_check (nc_get_att_text (ncid, varid, attname,
                                 (char *) RAW (result)));
  }
  return result;
}

/* Read NC_CHAR attribute as a single R string */
static SEXP
R_nc_get_att_char (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  char *charbuf;
  result = R_nc_protect (allocVector (STRSXP, 1));
  if (cnt > 0) {
    charbuf = R_alloc (cnt + 1, sizeof (char));
    R_nc_check (nc_get_att_text (ncid, varid, attname, charbuf));
    R_nc_char_strsxp (charbuf, result, cnt, 0, 1);
  }
  return result;
}

/* Read NC_STRING attribute as a vector of R strings */
static SEXP
R_nc_get_att_string (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  char **strbuf;
  result = R_nc_protect (allocVector (STRSXP, cnt));
  if (cnt > 0) {
    strbuf = (void *) R_alloc (cnt, sizeof(char *));
    R_nc_check (nc_get_att_string (ncid, varid, attname, strbuf));
    R_nc_str_strsxp (strbuf, result, 0, cnt);
    R_nc_check (nc_free_string (cnt, strbuf));
  }
  return result;
}

/* Read numeric attribute as R integers */
static SEXP
R_nc_get_att_int (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  result = R_nc_protect (allocVector (INTSXP, cnt));
  if (cnt > 0) {
    R_nc_check (nc_get_att_int (ncid, varid, attname, INTEGER (result)));
  }
  return result;
}

/* Read NC_INT64 attribute as R strings */
static SEXP
R_nc_get_att_int64 (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  long long *int64buf;
  result = R_nc_protect (allocVector (STRSXP, cnt));
  if (cnt > 0) {
    int64buf = (void *) R_alloc (cnt, sizeof (long long));
    R_nc_check (nc_get_att_longlong (ncid, varid, attname, int64buf));
    R_nc_int64_strsxp (int64buf, result, 0, cnt, NULL);
  }
  return result;
}

/* Read NC_UINT64 attribute as R strings */
static SEXP
R_nc_get_att_uint64 (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  unsigned long long *uint64buf;
  result = R_nc_protect (allocVector (STRSXP, cnt));
  if (cnt > 0) {
    uint64buf = (void *) R_alloc (cnt, sizeof (unsigned long long));
    R_nc_check (nc_get_att_ulonglong (ncid, varid, attname, uint64buf));
    R_nc_uint64_strsxp (uint64buf, result, 0, cnt, NULL);
  }
  return result;
}

/* Read numeric attribute as R double precision */
static SEXP
R_nc_get_att_double (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  result = R_nc_protect (allocVector (REALSXP, cnt));
  if (cnt > 0) {
    R_nc_check (nc_get_att_double (ncid, varid, attname, REAL (result)));
  }
  return result;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_att (SEXP nc, SEXP var, SEXP att, SEXP rawchar, SEXP fitnum)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];
  size_t cnt;
  nc_type xtype;
  SEXP result=R_NilValue;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  R_nc_check (R_nc_att_name (att, ncid, varid, attname));

  /*-- Get the attribute's type and size --------------------------------------*/
  R_nc_check(nc_inq_att (ncid, varid, attname, &xtype, &cnt));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Allocate memory and read attribute from file ---------------------------*/
  switch (xtype) {
  case NC_CHAR:
    if (asLogical (rawchar) == TRUE) {
      result = R_nc_get_att_raw (ncid, varid, attname, cnt);
    } else {
      result = R_nc_get_att_char (ncid, varid, attname, cnt);
    }
    break;
  case NC_STRING:
    result = R_nc_get_att_string (ncid, varid, attname, cnt);
    break;
  case NC_BYTE:
  case NC_UBYTE:
  case NC_SHORT:
  case NC_USHORT:
  case NC_INT:
  case NC_UINT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_INT64:
  case NC_UINT64:
    if (asLogical (fitnum) == TRUE) {
      switch (xtype) {
      case NC_BYTE:
      case NC_UBYTE:
      case NC_SHORT:
      case NC_USHORT:
      case NC_INT:
	result = R_nc_get_att_int (ncid, varid, attname, cnt);
	break;
      case NC_INT64:
	result = R_nc_get_att_int64 (ncid, varid, attname, cnt);
	break;
      case NC_UINT64:
	result = R_nc_get_att_uint64 (ncid, varid, attname, cnt);
	break;
      }
    }
    if (result == R_NilValue) {
      result = R_nc_get_att_double (ncid, varid, attname, cnt);
    }
    break;
  }

  if (result == R_NilValue) {
    RERROR (RNC_ETYPEDROP);
  } else {
    RRETURN (result);
  }
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_att (SEXP nc, SEXP var, SEXP att)
{
  int ncid, varid, attid;
  char attname[NC_MAX_NAME+1];
  const char *atttype;
  nc_type type;
  size_t cnt;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  R_nc_check (R_nc_att_name (att, ncid, varid, attname));

  /*-- Inquire about the attribute --------------------------------------------*/
  R_nc_check (nc_inq_attid (ncid, varid, attname, &attid));

  R_nc_check (nc_inq_att (ncid, varid, attname, &type, &cnt));

  /*-- Convert nc_type to char ------------------------------------------------*/
  atttype = R_nc_type2str (ncid, type);

  /*-- Returning the list -----------------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 4));
  SET_VECTOR_ELT (result, 0, ScalarInteger (attid));
  SET_VECTOR_ELT (result, 1, mkString (attname));
  SET_VECTOR_ELT (result, 2, mkString (atttype));
  /* cnt may not fit in integer, so return as double */
  SET_VECTOR_ELT (result, 3, ScalarReal (cnt));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_att (SEXP nc, SEXP var, SEXP att, SEXP type, SEXP data)
{
  int ncid, varid;
  size_t cnt;
  nc_type xtype;
  const char *attname, *charbuf, **strbuf;
  long long *int64buf;
  unsigned long long *uint64buf;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  attname = CHAR (STRING_ELT (att, 0));

  /*-- Convert char to nc_type ------------------------------------------------*/
  R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &xtype));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Write attribute to file ------------------------------------------------*/
  switch (TYPEOF (data)) {
  case RAWSXP:
    if (xtype == NC_CHAR) {
      charbuf = (char *) RAW (data);
      cnt = xlength (data);
      R_nc_check (nc_put_att_text (ncid, varid, attname, cnt, charbuf));
      RRETURN (R_NilValue);
    }
    break;
  case STRSXP:
    switch (xtype) {
    case NC_CHAR:
      /* Only write a single string */
      charbuf = CHAR (STRING_ELT (data, 0));
      cnt = strlen (charbuf);
      R_nc_check (nc_put_att_text (ncid, varid, attname, cnt, charbuf));
      RRETURN (R_NilValue);
    case NC_STRING:
      cnt = xlength (data);
      strbuf = (void *) R_alloc (cnt, sizeof(char *));
      R_nc_strsxp_str (data, strbuf, 0, cnt);
      R_nc_check (nc_put_att_string (ncid, varid, attname, cnt, strbuf));
      RRETURN (R_NilValue);
    case NC_INT64:
      cnt = xlength (data);
      int64buf = (void *) R_alloc (cnt, sizeof (long long));
      R_nc_strsxp_int64 (data, int64buf, 0, cnt, NULL);
      R_nc_check (nc_put_att_longlong (ncid, varid, attname,
                                       xtype, cnt, int64buf));
      RRETURN (R_NilValue);
    case NC_UINT64:
      cnt = xlength (data);
      uint64buf = (void *) R_alloc (cnt, sizeof (unsigned long long));
      R_nc_strsxp_uint64 (data, uint64buf, 0, cnt, NULL);
      R_nc_check (nc_put_att_ulonglong (ncid, varid, attname,
                                        xtype, cnt, uint64buf));
      RRETURN (R_NilValue);
    }
    break;
  }

  switch (xtype) {
  case NC_BYTE:
  case NC_UBYTE:
  case NC_SHORT:
  case NC_USHORT:
  case NC_INT:
  case NC_UINT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_INT64:
  case NC_UINT64:
    switch (TYPEOF (data)) {
    case INTSXP:
    case LGLSXP:
      cnt = xlength (data);
      R_nc_check (nc_put_att_int (ncid, varid, attname, xtype, cnt, INTEGER (data)));
      RRETURN (R_NilValue);
    case REALSXP:
      cnt = xlength (data);
      R_nc_check (nc_put_att_double (ncid, varid, attname, xtype, cnt, REAL (data)));
      RRETURN (R_NilValue);
    }
    break;
  }

  /* If this point is reached, input and external types were not compatible */
  RERROR (RNC_EDATATYPE);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_att (SEXP nc, SEXP var, SEXP att, SEXP newname)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];
  const char *newnamep;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  R_nc_check (R_nc_att_name (att, ncid, varid, attname));

  newnamep = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the attribute ---------------------------------------------------*/
  R_nc_check (nc_rename_att (ncid, varid, attname, newnamep));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_close()                                                               *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_close (SEXP ptr)
{
  int *fileid;

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
 *  R_nc_create()                                                              *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_create (SEXP filename, SEXP clobber, SEXP share, SEXP prefill,
             SEXP format)
{
  int cmode, fillmode, old_fillmode, ncid, *fileid;
  SEXP Rptr, result;

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
  R_nc_check (nc_create (R_ExpandFileName (CHAR (STRING_ELT (filename, 0))),
                       cmode, &ncid));
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
 *  R_nc_def_dim()                                                             *
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

  dimnamep = CHAR (STRING_ELT (dimname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Create the dimension ---------------------------------------------------*/
  if (asLogical(unlim) == TRUE) {
    nccnt = NC_UNLIMITED;
  } else {
    /* Allow size to be a double, which can be larger than integer */
    if (isInteger(size)) {
      nccnt = asInteger(size);
    } else {
      nccnt = asReal(size);
    }
  }

  R_nc_check (nc_def_dim (ncid, dimnamep, nccnt, &dimid));

  result = R_nc_protect (ScalarInteger (dimid));
  RRETURN(result);
}


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


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dim (SEXP nc, SEXP dim)
{
  int ncid, nunlim, *unlimids, isunlim, dimid, ii;
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
 *  R_nc_rename_dim()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_dim (SEXP nc, SEXP dim, SEXP newname)
{
  int ncid, dimid;
  const char *newnamep;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_dim_id (dim, ncid, &dimid, 0));

  newnamep = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the dimension ---------------------------------------------------*/
  R_nc_check (nc_rename_dim (ncid, dimid, newnamep));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()                                                            *
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
 *  R_nc_open()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill)
{
  int ncid, omode, fillmode, old_fillmode, *fileid;
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
  R_nc_check (nc_open (R_ExpandFileName (CHAR (STRING_ELT (filename, 0))),
                     omode, &ncid));
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
 *  R_nc_sync()                                                                *
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


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_var (SEXP nc, SEXP varname, SEXP type, SEXP dims)
{
  int ncid, ii, *dimids, ndims, varid;
  nc_type xtype;
  const char *varnamep;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  varnamep = CHAR (STRING_ELT (varname, 0));

  R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &xtype));

  ndims = length(dims);
  dimids = (void *) R_alloc (ndims, sizeof(int));

  for (ii=0; ii<ndims; ii++) {
    /* Handle dimension names and convert from R to C storage order */
    R_nc_check (R_nc_dim_id (dims, ncid, &dimids[ndims-1-ii], ii));
  }

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Define the variable ----------------------------------------------------*/
  R_nc_check (nc_def_var (
            ncid, varnamep, xtype, ndims, dimids, &varid));

  result = R_nc_protect (ScalarInteger (varid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*
 *  Private functions used by R_nc_get_var()                                   *
 *-----------------------------------------------------------------------------*/

/* Allocate array with dimensions specified in C order */
static SEXP
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

/* Read NC_CHAR variable as R raw array */
static SEXP
R_nc_get_var_raw (int ncid, int varid, int ndims,
                  const size_t *cstart, const size_t *ccount)
{
  SEXP result;
  result = R_nc_allocArray (RAWSXP, ndims, ccount);
  if (xlength (result) > 0) {
    R_nc_check (nc_get_vara_text (ncid, varid, cstart, ccount,
                                  (char *) RAW (result)));
  }
  return result;
}

/* Read NC_CHAR variable as R character array */
static SEXP
R_nc_get_var_char (int ncid, int varid, int ndims,
                  const size_t *cstart, const size_t *ccount)
{
  SEXP result;
  size_t strcnt, strlen;
  char *charbuf;
  if (ndims > 0) {
    /* Omit fastest-varying dimension from R character array */
    strlen = ccount[ndims-1];
  } else {
    /* Scalar character */
    strlen = 1;
  }
  result = R_nc_allocArray (STRSXP, ndims-1, ccount);
  strcnt = xlength (result);
  if (strcnt > 0) {
    charbuf = R_alloc (strcnt*strlen+1, sizeof (char));
    R_nc_check (nc_get_vara_text (ncid, varid, cstart, ccount, charbuf));
    R_nc_char_strsxp (charbuf, result, strlen, 0, strcnt);
  }
  return result;
}

/* Read NC_STRING variable as R character array */
static SEXP
R_nc_get_var_string (int ncid, int varid, int ndims,
                     const size_t *cstart, const size_t *ccount)
{
  SEXP result;
  size_t strcnt;
  char **strbuf;
  result = R_nc_allocArray (STRSXP, ndims, ccount);
  strcnt = xlength (result);
  if (strcnt > 0) {
    strbuf = (void *) R_alloc (strcnt, sizeof(char *));
    R_nc_check (nc_get_vara_string (ncid, varid, cstart, ccount, strbuf));
    R_nc_str_strsxp (strbuf, result, 0, strcnt);
    R_nc_check (nc_free_string (strcnt, strbuf));
  }
  return result;
}

/* Read numeric variable as R integer array */
static SEXP
R_nc_get_var_int (int ncid, int varid, int ndims,
                  const size_t *cstart, const size_t *ccount)
{
  SEXP result;
  result = R_nc_allocArray (INTSXP, ndims, ccount);
  if (xlength (result) > 0) {
    R_nc_check (nc_get_vara_int (ncid, varid, cstart, ccount,
                                 INTEGER (result)));
  }
  return result;
}

/* Read NC_INT64 variable as R character array */
static SEXP
R_nc_get_var_int64 (int ncid, int varid, int ndims,
                    const size_t *cstart, const size_t *ccount)
{
  SEXP result;
  size_t arrlen;
  long long *int64buf;
  result = R_nc_allocArray (STRSXP, ndims, ccount);
  arrlen = xlength (result);
  if (arrlen > 0) {
    int64buf = (void *) R_alloc (arrlen, sizeof (long long));
    R_nc_check (nc_get_vara_longlong (ncid, varid, cstart, ccount, int64buf));
    R_nc_int64_strsxp (int64buf, result, 0, arrlen, NULL);
  }
  return result;
}

/* Read NC_UINT64 variable as R character array */
static SEXP
R_nc_get_var_uint64 (int ncid, int varid, int ndims,
                     const size_t *cstart, const size_t *ccount)
{
  SEXP result;
  size_t arrlen;
  unsigned long long *uint64buf;
  result = R_nc_allocArray (STRSXP, ndims, ccount);
  arrlen = xlength (result);
  if (arrlen > 0) {
    uint64buf = (void *) R_alloc (arrlen, sizeof (unsigned long long));
    R_nc_check (nc_get_vara_ulonglong (ncid, varid, cstart, ccount, uint64buf));
    R_nc_uint64_strsxp (uint64buf, result, 0, arrlen, NULL);
  }
  return result;
}

/* Read numeric variable as R double array */
static SEXP
R_nc_get_var_double (int ncid, int varid, int ndims,
                     const size_t *cstart, const size_t *ccount)
{
  SEXP result;
  result = R_nc_allocArray (REALSXP, ndims, ccount);
  if (xlength (result) > 0) {
    R_nc_check (nc_get_vara_double (ncid, varid, cstart, ccount,
                                    REAL (result)));
  }
  return result;
}



/*-----------------------------------------------------------------------------*\
 *  R_nc_get_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_var (SEXP nc, SEXP var, SEXP start, SEXP count,
              SEXP rawchar, SEXP fitnum)
{
  int ncid, varid, ndims;
  size_t *cstart, *ccount;
  nc_type xtype;
  SEXP result=R_NilValue;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Handle NA values in start & count and reverse dimension order ----------*/
  R_nc_check ( R_nc_slice (R_NilValue, start, count, ncid, varid,
                           &ndims, &cstart, &ccount));

  /*-- Determine type of external data ----------------------------------------*/
  R_nc_check (nc_inq_vartype ( ncid, varid, &xtype));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Allocate memory and read variable from file ----------------------------*/
  switch (xtype) {
  case NC_CHAR:
    if (asLogical (rawchar) == TRUE) {
      result = R_nc_get_var_raw (ncid, varid, ndims, cstart, ccount);
    } else {
      result = R_nc_get_var_char (ncid, varid, ndims, cstart, ccount);
    }
    break;
  case NC_STRING:
    result = R_nc_get_var_string (ncid, varid, ndims, cstart, ccount);
    break;
  case NC_BYTE:
  case NC_UBYTE:
  case NC_SHORT:
  case NC_USHORT:
  case NC_INT:
  case NC_UINT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_INT64:
  case NC_UINT64:
    if (asLogical (fitnum) == TRUE) {
      switch (xtype) {
      case NC_BYTE:
      case NC_UBYTE:
      case NC_SHORT:
      case NC_USHORT:
      case NC_INT:
	result = R_nc_get_var_int (ncid, varid, ndims, cstart, ccount);
	break;
      case NC_INT64:
	result = R_nc_get_var_int64 (ncid, varid, ndims, cstart, ccount);
	break;
      case NC_UINT64:
	result = R_nc_get_var_uint64 (ncid, varid, ndims, cstart, ccount);
	break;
      }
    }
    if (result == R_NilValue) {
      result = R_nc_get_var_double (ncid, varid, ndims, cstart, ccount);
    }
    break;
  }

  if (result == R_NilValue) {
    RERROR (RNC_ETYPEDROP);
  } else {
    RRETURN (result);
  }
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_var (SEXP nc, SEXP var)
{
  int ii, ncid, varid, ndims, natts, *cdimids, *rdimids;
  const char *vartype;
  char varname[NC_MAX_NAME + 1];
  nc_type xtype;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Inquire the variable ---------------------------------------------------*/
  R_nc_check (nc_inq_var (ncid, varid, varname, &xtype, &ndims, NULL, &natts));

  if (ndims > 0) {
    cdimids = (void *) R_alloc (ndims, sizeof (int));
    R_nc_check (nc_inq_vardimid (ncid, varid, cdimids));
  }

  /*-- Convert nc_type to char ------------------------------------------------*/
  vartype = R_nc_type2str (ncid, xtype);

  /*-- Construct the output list ----------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 6));
  SET_VECTOR_ELT (result, 0, ScalarInteger (varid));
  SET_VECTOR_ELT (result, 1, mkString (varname));
  SET_VECTOR_ELT (result, 2, mkString (vartype));
  SET_VECTOR_ELT (result, 3, ScalarInteger (ndims));

  if (ndims > 0) {
    /* Return vector of dimension ids in R order */
    SET_VECTOR_ELT (result, 4, allocVector (INTSXP, ndims));
    rdimids = INTEGER (VECTOR_ELT (result, 4));
    for (ii=0; ii<ndims; ii++) {
      rdimids[ii] = cdimids[ndims-1-ii];
    }
  } else {
    /* Return single NA for scalars */
    SET_VECTOR_ELT (result, 4, allocVector (INTSXP, 1));
    INTEGER (VECTOR_ELT (result, 4))[0] = NA_INTEGER;
  }

  SET_VECTOR_ELT (result, 5, ScalarInteger (natts));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP data)
{
  int ncid, varid, ndims;
  size_t *cstart, *ccount, arrlen, strcnt, strlen;
  nc_type xtype;
  char *charbuf;
  const char **strbuf;
  long long *int64buf;
  unsigned long long *uint64buf;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Handle NA values in start & count and reverse dimension order ----------*/
  R_nc_check ( R_nc_slice (data, start, count, ncid, varid,
                           &ndims, &cstart, &ccount));

  /*-- Find total number of elements in data array ----------------------------*/
  arrlen = R_nc_length (ndims, ccount);
  if (arrlen == 0) {
    /* Nothing to write, so return immediately */
    RRETURN(R_NilValue);
  }

  /*-- Determine type of external data ----------------------------------------*/
  R_nc_check (nc_inq_vartype ( ncid, varid, &xtype));

  /*-- Ensure that data array contains enough elements ------------------------*/
  if (TYPEOF (data) != STRSXP || xtype != NC_CHAR) {
    if (xlength (data) < arrlen) {
      RERROR (RNC_EDATALEN);
    }
  } /* else check separately as a special case below */

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Write variable to file -------------------------------------------------*/
  switch (TYPEOF (data)) {
  case RAWSXP:
    if (xtype == NC_CHAR) {
      R_nc_check (nc_put_vara_text (ncid, varid, cstart, ccount,
                                    (char *) RAW (data)));
      RRETURN (R_NilValue);
    }
    break;
  case STRSXP:
    switch (xtype) {
    case NC_CHAR:
      if (ndims > 0) {
        /* Store strings along the fastest varying dimension */
        strlen = ccount[ndims-1];
        strcnt = R_nc_length (ndims-1, ccount);
      } else {
        /* Scalar character is a single string */
        strlen = 1;
        strcnt = 1;
      }
      if (xlength (data) < strcnt) {
        RERROR (RNC_EDATALEN);
      }
      charbuf = R_alloc (strcnt*strlen, sizeof (char));
      R_nc_strsxp_char (data, charbuf, 0, strcnt, strlen);
      R_nc_check (nc_put_vara_text (ncid, varid, cstart, ccount, charbuf));
      RRETURN (R_NilValue);
    case NC_STRING:
      strbuf = (void *) R_alloc (arrlen, sizeof(char *));
      R_nc_strsxp_str (data, strbuf, 0, arrlen);
      R_nc_check (nc_put_vara_string (ncid, varid, cstart, ccount, strbuf));
      RRETURN (R_NilValue);
    case NC_INT64:
      int64buf = (void *) R_alloc (arrlen, sizeof (long long));
      R_nc_strsxp_int64 (data, int64buf, 0, arrlen, NULL);
      R_nc_check (nc_put_vara_longlong (ncid, varid, cstart, ccount, int64buf));
      RRETURN (R_NilValue);
    case NC_UINT64:
        uint64buf = (void *) R_alloc (arrlen, sizeof (unsigned long long));
        R_nc_strsxp_uint64 (data, uint64buf, 0, arrlen, NULL);
        R_nc_check (nc_put_vara_ulonglong (ncid, varid, cstart, ccount, uint64buf));
        RRETURN (R_NilValue);
    }
    break;
  }

  switch (xtype) {
  case NC_BYTE:
  case NC_UBYTE:
  case NC_SHORT:
  case NC_USHORT:
  case NC_INT:
  case NC_UINT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_INT64:
  case NC_UINT64:
    switch (TYPEOF (data)) {
    case INTSXP:
    case LGLSXP:
      R_nc_check (nc_put_vara_int (ncid, varid, cstart, ccount, INTEGER (data)));
      RRETURN (R_NilValue);
    case REALSXP:
      R_nc_check (nc_put_vara_double (ncid, varid, cstart, ccount, REAL (data)));
      RRETURN (R_NilValue);
    }
    break;
  }

  /* If this point is reached, input and external types were not compatible */
  RERROR (RNC_EDATATYPE);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_var()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_var (SEXP nc, SEXP var, SEXP newname)
{
  int ncid, varid;
  const char *cnewname;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  cnewname = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the variable ----------------------------------------------------*/
  R_nc_check (nc_rename_var (ncid, varid, cnewname));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_grp()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_grp (SEXP nc, SEXP grpname)
{
  int ncid, grpid;
  const char *cgrpname;
  SEXP result;

  /* Convert arguments to netcdf ids */
  ncid = asInteger (nc);

  cgrpname = CHAR (STRING_ELT (grpname, 0));

  /* Enter define mode */
  R_nc_check( R_nc_redef (ncid));

  /* Define the group */
  R_nc_check (nc_def_grp (ncid, cgrpname, &grpid));

  result = R_nc_protect (ScalarInteger (grpid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_parent()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_parent (SEXP nc)
{
  int ncid, grpid;
  SEXP result;

  /* Get parent group */
  ncid = asInteger (nc);
  R_nc_check (nc_inq_grp_parent (ncid, &grpid));

  result = R_nc_protect (ScalarInteger (grpid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_natts()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_natts (SEXP nc)
{
  int ncid, natts;
  SEXP result;

  /* Get number of attributes in group */
  ncid = asInteger (nc);
  R_nc_check (nc_inq_natts (ncid, &natts));

  result = R_nc_protect (ScalarInteger (natts));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grpname()                                                         *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grpname (SEXP nc, SEXP full)
{
  int ncid;
  size_t namelen;
  char *name, *fullname, namebuf[NC_MAX_NAME+1];
  SEXP result;

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

  result = R_nc_protect (mkString (name));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_ncid()                                                        *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_ncid (SEXP nc, SEXP grpname, SEXP full)
{
  int ncid, grpid;
  const char *cgrpname;
  SEXP result;

  ncid = asInteger (nc);
  cgrpname = CHAR (STRING_ELT (grpname, 0));

  if (asLogical (full) == TRUE) {
    R_nc_check (nc_inq_grp_full_ncid (ncid, cgrpname, &grpid));
  } else {
    R_nc_check (nc_inq_grp_ncid (ncid, cgrpname, &grpid));
  }

  result = R_nc_protect (ScalarInteger (grpid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  Get lists of ncids for components of a group                               *
\*-----------------------------------------------------------------------------*/

/* Template function returning a list of ncids for a group */
#define INQGRPIDS(RFUN, NCFUN) \
SEXP RFUN (SEXP nc) \
{ \
  int    ncid, count; \
  SEXP result; \
  ncid = asInteger (nc); \
  R_nc_check(NCFUN(ncid, &count, NULL)); \
  result = R_nc_protect (allocVector (INTSXP, count)); \
  R_nc_check(NCFUN(ncid, NULL, INTEGER(result))); \
  RRETURN(result); \
}

INQGRPIDS (R_nc_inq_grps, nc_inq_grps)
INQGRPIDS (R_nc_inq_typeids, nc_inq_typeids)
INQGRPIDS (R_nc_inq_varids, nc_inq_varids)


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dimids()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dimids (SEXP nc, SEXP ancestors)
{
  int ncid, full, count;
  SEXP result;

  ncid = asInteger (nc);
  full = (asLogical (ancestors) == TRUE);

  R_nc_check (nc_inq_dimids (ncid, &count, NULL, full));
  result = R_nc_protect (allocVector (INTSXP, count));
  R_nc_check (nc_inq_dimids (ncid, NULL, INTEGER (result), full));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_unlimids()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_unlimids (SEXP nc)
{
  int ncid, nunlim, *unlimids;
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
 *  R_nc_rename_grp()                                                          *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_rename_grp (SEXP nc, SEXP grpname)
{
  int ncid;
  const char *cgrpname;

#if defined HAVE_DECL_NC_RENAME_GRP && HAVE_DECL_NC_RENAME_GRP
  ncid = asInteger (nc);
  cgrpname = CHAR (STRING_ELT (grpname, 0));

  /* Enter define mode */
  R_nc_check( R_nc_redef (ncid));

  /* Rename the group */
  R_nc_check (nc_rename_grp (ncid, cgrpname));

  RRETURN(R_NilValue);

#else
  RERROR ("nc_rename_grp not supported by netcdf library");
#endif
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_type()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_type (SEXP nc, SEXP typename, SEXP class, SEXP basetype, SEXP size)
{
  int ncid, typeid;
  char mode;
  const char *typenamep;
  nc_type xtype;
  size_t xsize;
  SEXP result;

  /*-- Decode arguments -------------------------------------------------------*/
  ncid = asInteger (nc);

  typenamep = CHAR (STRING_ELT (typename, 0));

  if (R_nc_strcmp (class, "compound")) {
    mode = 'c';
  } else if (R_nc_strcmp (class, "enum")) {
    mode = 'e';
  } else if (R_nc_strcmp (class, "opaque")) {
    mode = 'o';
  } else if (R_nc_strcmp (class, "vlen")) {
    mode = 'v';
  } else {
    mode = '\0';
  }

  if (mode == 'e' || mode == 'v') {
    R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (basetype, 0)), &xtype));
  } else {
    if (isInteger (size)) {
      xsize = asInteger (size);
    } else {
      xsize = asReal (size);
    }
  }

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Create the type --------------------------------------------------------*/
  switch (mode) {
  case 'c':
    R_nc_check (nc_def_compound (ncid, xsize, typenamep, &typeid));
    break;
  case 'e':
    R_nc_check (nc_def_enum (ncid, xtype, typenamep, &typeid));
    break;
  case 'o':
    R_nc_check (nc_def_opaque (ncid, xsize, typenamep, &typeid));
    break;
  case 'v':
    R_nc_check (nc_def_vlen (ncid, typenamep, xtype, &typeid));
    break;
  default:
    RERROR ("Unknown class for type definition");
  }

  result = R_nc_protect (ScalarInteger (typeid));
  RRETURN(result);
}



/*=============================================================================*\
 *  Public functions called by R for udunits interface
\*=============================================================================*/

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


/*=============================================================================*/

/*=============================================================================*\
 *  SCRATCH                                                                    *
\*=============================================================================*/
