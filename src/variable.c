/*=============================================================================*\
 *
 *  Name:       variable.c
 *
 *  Version:    2.0-1
 *
 *  Purpose:    NetCDF variable functions for RNetCDF
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
 * $Header$
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

#ifdef HAVE_UDUNITS2_UDUNITS_H
# include <udunits2/udunits.h>
#else
# include <udunits.h>
#endif

#include "common.h"
#include "convert.h"
#include "variable.h"


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
  R_nc_dim_r2c_size (start, *ndims, 1, *cstart);
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
        R_nc_dim_r2c_size (datadim, nr, 1, *ccount);
      } else {
        for (ii=0; ii<nr-1; ii++) {
          (*ccount)[ii] = 1;
        }
        (*ccount)[nr-1] = xlength (data);
      }
    }
  } else {
    R_nc_dim_r2c_size (count, *ndims, NA_SIZE, *ccount);
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


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_var (SEXP nc, SEXP varname, SEXP type, SEXP dims)
{
  int ncid, ii, jj, *dimids, ndims, varid;
  nc_type xtype;
  const char *varnamep;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  varnamep = CHAR (STRING_ELT (varname, 0));

  R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &xtype));

  ndims = length(dims);
  dimids = (void *) R_alloc (ndims, sizeof(int));

  for (ii=0, jj=ndims-1; ii<ndims; ii++, jj--) {
    /* Handle dimension names and convert from R to C storage order */
    R_nc_check (R_nc_dim_id (dims, ncid, &dimids[jj], ii));
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
 *  Private functions used by R_nc_get_var()
 *-----------------------------------------------------------------------------*/

/* Find attributes related to missing values for a netcdf variable.
   On exit, relevant parameters are returned via double pointers to
     fill, min and max, which are either NULL or allocated by R_alloc.
   Argument mode specifies the attributes used for missing values:
     0 - _FillValue, or missing_value
     1 - _FillValue only
     2 - missing_value only
     3 - none
     4 - valid range determined as described at
         http://www.unidata.ucar.edu/software/netcdf/docs/attribute_conventions.html:
         valid_min & valid_max, or valid_range, or _FillValue, or default.
   Example: R_nc_miss_att (ncid, varid, mode, &xtype, &fill, &min, &max);
  */
static void
R_nc_miss_att (int ncid, int varid, int mode,
               nc_type *xtype, void **fill, void **min, void **max)
{
  size_t cnt, size;
  nc_type atype;
  void *range, *tmpfill;
  *fill = NULL;
  *min = NULL;
  *max = NULL;
  R_nc_check (nc_inq_vartype (ncid, varid, xtype));
  R_nc_check (nc_inq_type (ncid, *xtype, NULL, &size));
  if ((mode == 0 || mode == 1) &&
      nc_inq_att (ncid, varid, "_FillValue", &atype, &cnt) == NC_NOERR &&
      cnt == 1 &&
      atype == *xtype) {
    *fill = R_alloc (1, size);
    R_nc_check (nc_get_att (ncid, varid, "_FillValue", *fill));
    return;
  } else if ((mode == 0 || mode == 2) &&
      nc_inq_att (ncid, varid, "missing_value", &atype, &cnt) == NC_NOERR &&
      cnt == 1 &&
      atype == *xtype) {
    *fill = R_alloc (1, size);
    R_nc_check (nc_get_att (ncid, varid, "missing_value", *fill));
    return;
  } else if (mode == 4) {
    if (nc_inq_att (ncid, varid, "valid_min", &atype, &cnt) == NC_NOERR &&
        cnt == 1 &&
        atype == *xtype) {
      *min = R_alloc (1, size);
      R_nc_check (nc_get_att (ncid, varid, "valid_min", *min));
    }
    if (nc_inq_att (ncid, varid, "valid_max", &atype, &cnt) == NC_NOERR &&
        cnt == 1 &&
        atype == *xtype) {
      *max = R_alloc (1, size);
      R_nc_check (nc_get_att (ncid, varid, "valid_max", *max));
    }
    if (*min || *max) {
      return;
    }
    if (nc_inq_att (ncid, varid, "valid_range", &atype, &cnt) == NC_NOERR &&
        cnt == 2 &&
        atype == *xtype) {
      range = R_alloc (2, size);
      *min = R_alloc (1, size);
      *max = R_alloc (1, size);
      R_nc_check (nc_get_att (ncid, varid, "valid_range", range));
      memcpy(*min, range, size);
      memcpy(*max, range + size, size);
      return;
    }
    /* Derive valid range from fill value */
    tmpfill = R_alloc (1, size);
    if (nc_inq_att (ncid, varid, "_FillValue", &atype, &cnt) == NC_NOERR &&
        cnt == 1 &&
        atype == *xtype) {
      R_nc_check (nc_get_att (ncid, varid, "_FillValue", tmpfill));
    } else {
      /* According to the netcdf attribute conventions:
         "If the data type is byte and _FillValue is not explicitly defined,
          then the valid range should include all possible values."
       */
      switch (*xtype) {
	case NC_SHORT:
	  *(short *) tmpfill = NC_FILL_SHORT;
	  break;
	case NC_USHORT:
	  *(unsigned short *) tmpfill = NC_FILL_USHORT;
	  break;
	case NC_INT:
	  *(int *) tmpfill = NC_FILL_INT;
	  break;
	case NC_UINT:
	  *(unsigned int *) tmpfill = NC_FILL_UINT;
	  break;
	case NC_FLOAT:
	  *(float *) tmpfill = NC_FILL_FLOAT;
	  break;
	case NC_DOUBLE:
	  *(double *) tmpfill = NC_FILL_DOUBLE;
	  break;
	case NC_INT64:
	  *(long long *) tmpfill = NC_FILL_INT64;
	  break;
	case NC_UINT64:
	  *(unsigned long long *) tmpfill = NC_FILL_UINT64;
	  break;
        default:
          return;
      }
    }
#define FILL2RANGE_REAL(TYPE, EPS) { \
  if (*(TYPE *) tmpfill > (TYPE) 0) { \
    *max = R_alloc (1, size); \
    **(TYPE **) max = *(TYPE *) tmpfill * ((TYPE) 1 - (TYPE) 2 * (TYPE) EPS); \
  } else { \
    *min = R_alloc (1, size); \
    **(TYPE **) min = *(TYPE *) tmpfill * ((TYPE) 1 + (TYPE) 2 * (TYPE) EPS); \
  } \
}
#define FILL2RANGE_INT(TYPE) { \
  if (*(TYPE *) tmpfill > (TYPE) 0) { \
    *max = R_alloc (1, size); \
    **(TYPE **) max = *(TYPE *) tmpfill - (TYPE) 1; \
  } else { \
    *min = R_alloc (1, size); \
    **(TYPE **) min = *(TYPE *) tmpfill + (TYPE) 1; \
  } \
}
    switch (*xtype) {
      case NC_BYTE:
        FILL2RANGE_INT(signed char);
        break;
      case NC_UBYTE:
        FILL2RANGE_INT(unsigned char);
        break;
      case NC_SHORT:
        FILL2RANGE_INT(short);
	break;
      case NC_USHORT:
        FILL2RANGE_INT(unsigned short);
	break;
      case NC_INT:
        FILL2RANGE_INT(int);
	break;
      case NC_UINT:
        FILL2RANGE_INT(unsigned int);
	break;
      case NC_INT64:
        FILL2RANGE_INT(long long);
	break;
      case NC_UINT64:
        FILL2RANGE_INT(unsigned long long);
	break;
      case NC_FLOAT:
        FILL2RANGE_REAL(float, FLT_EPSILON);
	break;
      case NC_DOUBLE:
        FILL2RANGE_REAL(double, DBL_EPSILON);
	break;
      default:
        return;
    }
  }
}


/* Find packing attributes for a given netcdf variable.
   On entry, pointers for results are passed from caller.
   On exit, either values are set or pointers are NULLed.
   Example: R_nc_pack_att (ncid, varid, scalep, addp);
  */
static void
R_nc_pack_att (int ncid, int varid, double **scale, double **add)
{
  size_t cnt;
  if (nc_inq_attlen (ncid, varid, "scale_factor", &cnt) != NC_NOERR ||
        cnt != 1 ||
        nc_get_att_double (ncid, varid, "scale_factor", *scale) != NC_NOERR ) {
    *scale = NULL;
  }
  if (nc_inq_attlen (ncid, varid, "add_offset", &cnt) != NC_NOERR ||
    cnt != 1 ||
    nc_get_att_double (ncid, varid, "add_offset", *add) != NC_NOERR ) {
    *add = NULL;
  }
}


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


/* Reshape R array with dimensions specified in C order */
static void
R_nc_reshape (SEXP array, int ndims, const size_t *ccount) {
  SEXP rdim;
  int *intp, ii, jj;
  if (ndims > 0) {
    rdim = R_nc_protect( allocVector (INTSXP, ndims));
    intp = INTEGER (rdim);
    for ( ii=0, jj=ndims-1; ii<ndims; ii++, jj-- ) {
      if (ccount[jj] <= INT_MAX) {
        intp[ii] = ccount[jj];
      } else {
        R_nc_error ("R array dimension cannot exceed range of type int");
      }
    }
    setAttrib(array, R_DimSymbol, rdim);
  } else {
    /* R scalar or vector with no dimensions */
    setAttrib(array, R_DimSymbol, R_NilValue);
  }
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

/* Read NC_INT64 variable as R character array */
static SEXP
R_nc_get_var_int64 (int ncid, int varid, int ndims,
                    const size_t *cstart, const size_t *ccount)
{
  SEXP result=NULL;
  size_t arrlen;
  long long *int64buf;
  arrlen = R_nc_length (ndims, ccount);
  if (arrlen > 0) {
    int64buf = (void *) R_alloc (arrlen, sizeof (long long));
    R_nc_check (nc_get_vara (ncid, varid, cstart, ccount, int64buf));
    result = R_nc_c2r (int64buf, 0, arrlen, NC_INT64, TRUE,
              NULL, NULL, NULL, NULL, NULL);

  }
  R_nc_reshape (result, ndims, ccount);
  return result;
}

/* Read NC_UINT64 variable as R character array */
static SEXP
R_nc_get_var_uint64 (int ncid, int varid, int ndims,
                     const size_t *cstart, const size_t *ccount)
{
  SEXP result=NULL;
  size_t arrlen;
  unsigned long long *uint64buf;
  arrlen = R_nc_length (ndims, ccount);
  if (arrlen > 0) {
    uint64buf = (void *) R_alloc (arrlen, sizeof (unsigned long long));
    R_nc_check (nc_get_vara (ncid, varid, cstart, ccount, uint64buf));
    result = R_nc_c2r (uint64buf, 0, arrlen, NC_UINT64, TRUE,
              NULL, NULL, NULL, NULL, NULL);
  }
  R_nc_reshape (result, ndims, ccount);
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
 *  R_nc_get_var()
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
      if (asLogical (fitnum) == TRUE) {
	result = R_nc_get_var_int (ncid, varid, ndims, cstart, ccount);
	break;
      }
    case NC_INT64:
      if (asLogical (fitnum) == TRUE) {
	result = R_nc_get_var_int64 (ncid, varid, ndims, cstart, ccount);
	break;
      }
    case NC_UINT64:
      if (asLogical (fitnum) == TRUE) {
	result = R_nc_get_var_uint64 (ncid, varid, ndims, cstart, ccount);
	break;
      }
    case NC_UINT:
    case NC_FLOAT:
    case NC_DOUBLE:
      result = R_nc_get_var_double (ncid, varid, ndims, cstart, ccount);
      break;
    default:
      RERROR (RNC_ETYPEDROP);
  }

  RRETURN (result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_var (SEXP nc, SEXP var)
{
  int ncid, varid, ndims, natts, *dimids;
  char varname[NC_MAX_NAME + 1], vartype[NC_MAX_NAME+1];
  nc_type xtype;
  SEXP result, rdimids;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Inquire the variable ---------------------------------------------------*/
  R_nc_check (nc_inq_var (ncid, varid, varname, &xtype, &ndims, NULL, &natts));

  if (ndims > 0) {
    rdimids = R_nc_protect (allocVector (INTSXP, ndims));
    dimids = INTEGER (rdimids);
    R_nc_check (nc_inq_vardimid (ncid, varid, dimids));
    /* Return dimension ids in reverse (Fortran) order */
    R_nc_rev_int (dimids, ndims);
  } else {
    /* Return single NA for scalars */
    rdimids = R_nc_protect (ScalarInteger (NA_INTEGER));
  }

  /*-- Convert nc_type to char ------------------------------------------------*/
  R_nc_check (R_nc_type2str (ncid, xtype, vartype));

  /*-- Construct the output list ----------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 6));
  SET_VECTOR_ELT (result, 0, ScalarInteger (varid));
  SET_VECTOR_ELT (result, 1, mkString (varname));
  SET_VECTOR_ELT (result, 2, mkString (vartype));
  SET_VECTOR_ELT (result, 3, ScalarInteger (ndims));
  SET_VECTOR_ELT (result, 4, rdimids);
  SET_VECTOR_ELT (result, 5, ScalarInteger (natts));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_var()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP data)
{
  int ncid, varid, ndims;
  size_t *cstart, *ccount, arrlen, strcnt, strlen;
  nc_type xtype;
  char *charbuf;
  const char **strbuf;
  void *voidbuf;

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
      {  
        long long fill = NC_FILL_INT64;
        voidbuf = R_nc_r2c (data, NULL, 0, arrlen, NC_INT64, &fill, NULL, NULL);
      }
      R_nc_check (nc_put_vara (ncid, varid, cstart, ccount, voidbuf));
      RRETURN (R_NilValue);
    case NC_UINT64:
      {
        unsigned long long fill = NC_FILL_UINT64;
        voidbuf = R_nc_r2c (data, NULL, 0, arrlen, NC_UINT64, &fill, NULL, NULL);
      }
      R_nc_check (nc_put_vara (ncid, varid, cstart, ccount, voidbuf));
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
 *  R_nc_rename_var()
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

