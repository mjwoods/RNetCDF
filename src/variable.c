/*=============================================================================*\
 *
 *  Name:       variable.c
 *
 *  Version:    2.0-4
 *
 *  Purpose:    NetCDF variable functions for RNetCDF
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright:  (C) 2004-2019 Pavel Michna, Milton Woods
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
#include "RNetCDF.h"


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_var (SEXP nc, SEXP varname, SEXP type, SEXP dims,
              SEXP chunking, SEXP chunksizes, SEXP deflate, SEXP shuffle,
              SEXP big_endian, SEXP fletcher32, SEXP filter_id,
              SEXP filter_params)
{
  int ncid, ii, jj, *dimids, ndims, varid, chunkmode, format, withnc4;
  int deflate_mode, deflate_level, shuffle_mode, endian_mode, fletcher_mode;
  int filter_mode, filtid, *filtparm;
  size_t *chunksize_t;
  nc_type xtype;
  const char *varnamep;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  varnamep = R_nc_strarg (varname);

  R_nc_check (R_nc_type_id (type, ncid, &xtype, 0));

  ndims = length(dims);
  dimids = (void *) R_alloc (ndims, sizeof(int));

  for (ii=0, jj=ndims-1; ii<ndims; ii++, jj--) {
    /* Handle dimension names and convert from R to C storage order */
    R_nc_check (R_nc_dim_id (dims, ncid, &dimids[jj], ii));
  }

  R_nc_check (nc_inq_format (ncid, &format));
  withnc4 = (format == NC_FORMAT_NETCDF4);

  if (withnc4) {
    chunkmode = asLogical (chunking);
    if (ndims == 0) {
      /* Chunking is not relevant to scalar variables */
      chunkmode = NA_LOGICAL;
    }
    if (chunkmode == TRUE) {
      if (isNull (chunksizes)) {
        chunksize_t = NULL;
      } else {
        chunksize_t = R_nc_dim_r2c_size (chunksizes, ndims, 0);
      }
    }

    deflate_level = asInteger (deflate);
    deflate_mode = (deflate_level != NA_INTEGER);

    shuffle_mode = (asLogical (shuffle) == TRUE);

#ifdef HAVE_NC_INQ_VAR_ENDIAN
    switch (asLogical (big_endian)) {
    case TRUE:
      endian_mode = NC_ENDIAN_BIG;
      break;
    case FALSE:
      endian_mode = NC_ENDIAN_LITTLE;
      break;
    default:
      endian_mode = NC_ENDIAN_NATIVE;
      break;
    }
#endif

    fletcher_mode = (asLogical (fletcher32) == TRUE);

    filtid = asInteger (filter_id);
    filter_mode = (filtid != NA_INTEGER);
    filtparm = INTEGER (filter_params);
  }

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Define the variable and details of storage -----------------------------*/
  R_nc_check (nc_def_var (
            ncid, varnamep, xtype, ndims, dimids, &varid));

  if (withnc4) {
    if (chunkmode == FALSE) {
      R_nc_check (nc_def_var_chunking (ncid, varid, NC_CONTIGUOUS, NULL));
    } else if (chunkmode == TRUE) {
      R_nc_check (nc_def_var_chunking (ncid, varid, NC_CHUNKED, chunksize_t));
    } // If NA, use storage format chosen by NetCDF

    if (deflate_mode || shuffle_mode) {
      R_nc_check (nc_def_var_deflate (ncid, varid, shuffle_mode,
                                      deflate_mode, deflate_level));
    }

#ifdef HAVE_NC_INQ_VAR_ENDIAN
    if (endian_mode != NC_ENDIAN_NATIVE) {
      R_nc_check (nc_def_var_endian (ncid, varid, endian_mode));
    }
#endif

    if (fletcher_mode) {
      R_nc_check (nc_def_var_fletcher32 (ncid, varid, fletcher_mode));
    }

#ifdef HAVE_NC_INQ_VAR_FILTER
    if (filter_mode) {
      R_nc_check (nc_def_var_filter (ncid, varid, (unsigned int) filtid,
        xlength (filter_params), (const unsigned int*) filtparm));
    }
#endif

  }

  result = R_nc_protect (ScalarInteger (varid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*
 *  Private functions used by R_nc_get_var()
 *-----------------------------------------------------------------------------*/

/* Macros to set **max or **min so that **fill is outside valid range */
#define FILL2RANGE_REAL(TYPE, EPS) { \
  if (**(TYPE **) fill > (TYPE) 0) { \
    *max = R_alloc (1, sizeof(TYPE)); \
    **(TYPE **) max = **(TYPE **) fill * ((TYPE) 1 - (TYPE) 2 * (TYPE) EPS); \
  } else { \
    *min = R_alloc (1, sizeof(TYPE)); \
    **(TYPE **) min = **(TYPE **) fill * ((TYPE) 1 + (TYPE) 2 * (TYPE) EPS); \
  } \
}
#define FILL2RANGE_INT(TYPE) { \
  if (**(TYPE **) fill > (TYPE) 0) { \
    *max = R_alloc (1, sizeof(TYPE)); \
    **(TYPE **) max = **(TYPE **) fill - (TYPE) 1; \
  } else { \
    *min = R_alloc (1, sizeof(TYPE)); \
    **(TYPE **) min = **(TYPE **) fill + (TYPE) 1; \
  }; \
}

/* Find attributes related to missing values for a netcdf variable.
   On exit, relevant parameters are returned via double pointers to
     fill, min and max, which are either NULL or allocated by R_alloc.
     The function returns the in-memory size (bytes) of a missing value.
   Argument mode specifies the attributes used for missing values:
     0 - _FillValue, or missing_value
     1 - _FillValue only
     2 - missing_value only
     3 - none
     4 - fill value and valid range determined as described at
         http://www.unidata.ucar.edu/software/netcdf/docs/attribute_conventions.html
   Example: R_nc_miss_att (ncid, varid, mode, &fill, &min, &max);
  */
static size_t
R_nc_miss_att (int ncid, int varid, int mode,
               void **fill, void **min, void **max)
{
  size_t cnt, size;
  int class;
  nc_type atype, xtype, basetype;
  char *range;
  *fill = NULL;
  *min = NULL;
  *max = NULL;

  /* Get details about type and size of netcdf variable */
  R_nc_check (nc_inq_vartype (ncid, varid, &xtype));
  if (xtype == NC_CHAR ||
      xtype == NC_STRING ||
      xtype > NC_MAX_ATOMIC_TYPE) {
    /* NetCDF attribute conventions describe the handling of missing values
       in atomic numeric types. Let users handle other types as needed.
     */
    return 0;
  }
  R_nc_check (nc_inq_type (ncid, xtype, NULL, &size));

  if ((mode == 0 || mode == 1) &&
      nc_inq_att (ncid, varid, "_FillValue", &atype, &cnt) == NC_NOERR &&
      cnt == 1 &&
      atype == xtype) {
    *fill = R_alloc (1, size);
    R_nc_check (nc_get_att (ncid, varid, "_FillValue", *fill));

  } else if ((mode == 0 || mode == 2) &&
      nc_inq_att (ncid, varid, "missing_value", &atype, &cnt) == NC_NOERR &&
      cnt == 1 &&
      atype == xtype) {
    *fill = R_alloc (1, size);
    R_nc_check (nc_get_att (ncid, varid, "missing_value", *fill));

  } else if (mode == 3) {
    /* Let user code handle missing values */
    return 0;

  } else if (mode == 4) {

    /* Special rules apply to byte data */
    if ((xtype == NC_BYTE) || (xtype == NC_UBYTE)) {

      /* For byte data, valid_range, valid_min and valid_max attributes
       * may differ from the type of variable, so we read the attributes
       * with routines that convert to the expected type.
       */
      if (nc_inq_att (ncid, varid, "valid_min", &atype, &cnt) == NC_NOERR &&
          cnt == 1) {
        *min = R_alloc (1, 1);
        if (xtype == NC_UBYTE) {
          R_nc_check (nc_get_att_uchar (ncid, varid, "valid_min", *min));
        } else {
          R_nc_check (nc_get_att_schar (ncid, varid, "valid_min", *min));
        }
      }
      if (nc_inq_att (ncid, varid, "valid_max", &atype, &cnt) == NC_NOERR &&
          cnt == 1) {
        *max = R_alloc (1, 1);
        if (xtype == NC_UBYTE) {
          R_nc_check (nc_get_att_uchar (ncid, varid, "valid_max", *max));
        } else {
          R_nc_check (nc_get_att_schar (ncid, varid, "valid_max", *max));
        }
      }
      if (!*min && !*max &&
          nc_inq_att (ncid, varid, "valid_range", &atype, &cnt) == NC_NOERR &&
          cnt == 2) {
        range = R_alloc (2, 1);
        *min = range;
        *max = range + 1;
        if (xtype == NC_UBYTE) {
          R_nc_check (nc_get_att_uchar (
                        ncid, varid, "valid_range", (unsigned char *) range));
        } else {
          R_nc_check (nc_get_att_schar (
                        ncid, varid, "valid_range", (signed char *) range));
        }
      }

      /* Only set fill value if explicitly defined.
       * _FillValue attribute should have same type as variable.
       */
      if (nc_inq_att (ncid, varid, "_FillValue", &atype, &cnt) == NC_NOERR &&
          cnt == 1 &&
          atype == xtype) {
        *fill = R_alloc (1, 1);
        R_nc_check (nc_get_att (ncid, varid, "_FillValue", *fill));
      }

      /* If _FillValue is defined without a valid range,
       * set the valid range to exclude _FillValue
       */
      if (*fill && !*max && !*min) {
        if (xtype == NC_UBYTE) {
          FILL2RANGE_INT(unsigned char)
        } else {
          FILL2RANGE_INT(signed char)
        }
      }

    } else {
      /* All types other than byte data */

      /* Type of valid_* attribute must match type of variable data */
      if (nc_inq_att (ncid, varid, "valid_min", &atype, &cnt) == NC_NOERR &&
          cnt == 1 &&
          atype == xtype) {
        *min = R_alloc (1, size);
        R_nc_check (nc_get_att (ncid, varid, "valid_min", *min));
      }
      if (nc_inq_att (ncid, varid, "valid_max", &atype, &cnt) == NC_NOERR &&
          cnt == 1 &&
          atype == xtype) {
        *max = R_alloc (1, size);
        R_nc_check (nc_get_att (ncid, varid, "valid_max", *max));
      }
      if (!*min && !*max &&
          nc_inq_att (ncid, varid, "valid_range", &atype, &cnt) == NC_NOERR &&
          cnt == 2 &&
          atype == xtype) {
        range = R_alloc (2, size);
        *min = range;
        *max = range + size;
        R_nc_check (nc_get_att (ncid, varid, "valid_range", range));
      }

      /* Get fill value from attribute or use default */
      if (nc_inq_att (ncid, varid, "_FillValue", &atype, &cnt) == NC_NOERR &&
          cnt == 1 &&
          atype == xtype) {
        *fill = R_alloc (1, size);
        R_nc_check (nc_get_att (ncid, varid, "_FillValue", *fill));
      } else {
        *fill = R_alloc (1, size);
        switch (xtype) {
          case NC_SHORT:
            **(short **) fill = NC_FILL_SHORT;
            break;
          case NC_USHORT:
            **(unsigned short **) fill = NC_FILL_USHORT;
            break;
          case NC_INT:
            **(int **) fill = NC_FILL_INT;
            break;
          case NC_UINT:
            **(unsigned int **) fill = NC_FILL_UINT;
            break;
          case NC_FLOAT:
            **(float **) fill = NC_FILL_FLOAT;
            break;
          case NC_DOUBLE:
            **(double **) fill = NC_FILL_DOUBLE;
            break;
          case NC_INT64:
            **(long long **) fill = NC_FILL_INT64;
            break;
          case NC_UINT64:
            **(unsigned long long **) fill = NC_FILL_UINT64;
            break;
          default:
            R_nc_error ("Default fill value not implemented");
        }
      }

      /* If _FillValue is defined without a valid range,
       * set the valid range to exclude _FillValue
       */
      if (*fill && !*max && !*min) {
        switch (xtype) {
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
            R_nc_error ("Default valid range not implemented");
        }
      }

    }
  } else {
    R_nc_error ("Unknown mode for handling missing values");

  }
  return size;
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


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_var()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_var (SEXP nc, SEXP var, SEXP start, SEXP count,
              SEXP rawchar, SEXP fitnum, SEXP namode, SEXP unpack,
              SEXP cache_bytes, SEXP cache_slots, SEXP cache_preemption)
{
  int ncid, varid, ndims, ii, israw, isfit, inamode, isunpack;
  size_t *cstart=NULL, *ccount=NULL;
  nc_type xtype;
  SEXP result=R_NilValue;
  void *buf;
  R_nc_buf io;
  double add, scale, *addp=NULL, *scalep=NULL;
  void *fillp=NULL, *minp=NULL, *maxp=NULL;
  size_t fillsize, bytes, slots;
  float preemption;
  double bytes_in, slots_in, preempt_in;

  /*-- Convert arguments ------------------------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  israw = (asLogical (rawchar) == TRUE);
  isfit = (asLogical (fitnum) == TRUE);
  inamode = asInteger (namode);
  isunpack = (asLogical (unpack) == TRUE);

  /*-- Chunk cache options for netcdf4 files ----------------------------------*/
#ifdef HAVE_NC_GET_VAR_CHUNK_CACHE
  if (nc_get_var_chunk_cache(ncid, varid,
                             &bytes, &slots, &preemption) == NC_NOERR) {
    bytes_in = asReal (cache_bytes);
    slots_in = asReal (cache_slots);
    preempt_in = asReal (cache_preemption);
    if (R_FINITE(bytes_in) || R_FINITE(slots_in) || R_FINITE(preempt_in)) {
      if (R_FINITE(bytes_in)) {
	bytes = bytes_in;
      }
      if (R_FINITE(slots_in)) {
	slots = slots_in;
      }
      if (R_FINITE(preempt_in)) {
	preemption = preempt_in;
      }
      R_nc_check (nc_set_var_chunk_cache(ncid, varid,
                                         bytes, slots, preemption));
    }
  }
#endif

  /*-- Get type and rank of the variable --------------------------------------*/
  R_nc_check (nc_inq_var (ncid, varid, NULL, &xtype, &ndims, NULL, NULL));

  /*-- Convert start and count from R to C indices ----------------------------*/
  if (ndims > 0) {
    cstart = R_nc_dim_r2c_size (start, ndims, 0);
    ccount = R_nc_dim_r2c_size (count, ndims, 0);
    for (ii=0; ii<ndims; ii++) {
      cstart[ii] -= 1;
    }
  }

  /*-- Get fill attributes (if any) -------------------------------------------*/
  fillsize = R_nc_miss_att (ncid, varid, inamode, &fillp, &minp, &maxp);

  /*-- Get packing attributes (if any) ----------------------------------------*/
  if (isunpack) {
    scalep = &scale;
    addp = &add;
    R_nc_pack_att (ncid, varid, &scalep, &addp);
  }

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Allocate memory and read variable from file ----------------------------*/
  buf = R_nc_c2r_init (&io, NULL, ncid, xtype, ndims, ccount,
                       israw, isfit, fillp, minp, maxp, scalep, addp);

  if (R_nc_length (ndims, ccount) > 0) {
    R_nc_check (nc_get_vara (ncid, varid, cstart, ccount, buf));
  }
  result = R_nc_c2r (&io);

  RRETURN (result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_var (SEXP nc, SEXP var)
{
  int ncid, varid, idim, ndims, natts, *dimids, storeprop, format, withnc4;
  int shuffle, deflate, deflate_level, endian, fletcher;
  int status, szip_options, szip_bits;
  int filter_id;
  size_t filter_nparams;
  size_t *chunksize_t, cache_bytes, cache_slots;
  float cache_preemption;
  double *chunkdbl;
  char varname[NC_MAX_NAME + 1], vartype[NC_MAX_NAME+1];
  nc_type xtype;
  SEXP result, rdimids, rchunks, rbytes, rslots, rpreempt,
       rshuffle, rdeflate, rendian, rfletcher,
       rszip_options, rszip_bits, rfilter_id, rfilter_params;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Inquire the variable ---------------------------------------------------*/
  R_nc_check (nc_inq_format (ncid, &format));
  withnc4 = (format == NC_FORMAT_NETCDF4);

  R_nc_check (nc_inq_var (ncid, varid, varname, &xtype, &ndims, NULL, &natts));

  if (ndims > 0) {
    rdimids = R_nc_protect (allocVector (INTSXP, ndims));
    dimids = INTEGER (rdimids);
    R_nc_check (nc_inq_vardimid (ncid, varid, dimids));
    /* Return dimension ids in reverse (Fortran) order */
    R_nc_rev_int (dimids, ndims);

    rchunks = R_NilValue;
    if (withnc4) {
      R_nc_check (nc_inq_var_chunking (ncid, varid, &storeprop, NULL));
      if (storeprop == NC_CHUNKED) {
	rchunks = R_nc_protect (allocVector (REALSXP, ndims));
	chunkdbl = REAL (rchunks);
	chunksize_t = (size_t *) R_alloc (ndims, sizeof(size_t));
	R_nc_check (nc_inq_var_chunking (ncid, varid, NULL, chunksize_t));
	/* Return chunk sizes as double precision in reverse (Fortran) order */
	R_nc_rev_size (chunksize_t, ndims);
	for (idim=0; idim<ndims; idim++) {
	  chunkdbl[idim] = chunksize_t[idim];
	}
      }

#ifdef HAVE_NC_GET_VAR_CHUNK_CACHE
      R_nc_check (nc_get_var_chunk_cache (ncid, varid, &cache_bytes,
                                          &cache_slots, &cache_preemption));
      rbytes = R_nc_protect (ScalarReal (cache_bytes));
      rslots = R_nc_protect (ScalarReal (cache_slots));
      rpreempt = R_nc_protect (ScalarReal (cache_preemption));
#else
      rbytes = R_NilValue;
      rslots = R_NilValue;
      rpreempt = R_NilValue;
#endif
    }

  } else {
    /* Return single NA for scalars */
    rdimids = R_nc_protect (ScalarInteger (NA_INTEGER));

    /* Chunks not defined for scalars */
    rchunks = R_NilValue;
    rbytes = R_nc_protect (ScalarReal (NA_REAL));
    rslots = R_nc_protect (ScalarReal (NA_REAL));
    rpreempt = R_nc_protect (ScalarReal (NA_REAL));
  }

  if (withnc4) {
    /* deflate and shuffle */
    R_nc_check (nc_inq_var_deflate (ncid, varid, &shuffle,
                                    &deflate, &deflate_level));
    if (deflate) {
      rdeflate = R_nc_protect (ScalarInteger (deflate_level));
    } else {
      rdeflate = R_nc_protect (ScalarInteger (NA_INTEGER));
    }
    rshuffle = R_nc_protect (ScalarLogical (shuffle));

    /* endian */
#ifdef HAVE_NC_INQ_VAR_ENDIAN
    R_nc_check (nc_inq_var_endian (ncid, varid, &endian));
    if (endian == NC_ENDIAN_LITTLE) {
      rendian = R_nc_protect (ScalarLogical (0));
    } else if (endian == NC_ENDIAN_BIG) {
      rendian = R_nc_protect (ScalarLogical (1));
    } else {
      rendian = R_nc_protect (ScalarLogical (NA_LOGICAL));
    }
#else
    rendian = R_NilValue;
#endif

    /* fletcher32 */
    R_nc_check (nc_inq_var_fletcher32 (ncid, varid, &fletcher));
    rfletcher = R_nc_protect (ScalarLogical (fletcher == NC_FLETCHER32));

    /* szip */
#ifdef HAVE_NC_INQ_VAR_SZIP
    status = nc_inq_var_szip (ncid, varid, &szip_options, &szip_bits);
    if (status == NC_NOERR) {
      rszip_options = R_nc_protect (ScalarInteger (szip_options));
      rszip_bits = R_nc_protect (ScalarInteger (szip_bits));
#  if defined NC_EFILTER
    } else if (status == NC_EFILTER) {
      rszip_options = R_nc_protect (ScalarInteger (NA_INTEGER));
      rszip_bits = R_nc_protect (ScalarInteger (NA_INTEGER));
#  endif
    } else {
      R_nc_check (status);
      return R_NilValue;
    }
#else
    rszip_options = R_NilValue;
    rszip_bits = R_NilValue;
#endif

    /* filter */
#ifdef HAVE_NC_INQ_VAR_FILTER
    status = nc_inq_var_filter (ncid, varid,
                                (unsigned int *) &filter_id,
                                &filter_nparams, NULL);
    if (status == NC_NOERR) {
      rfilter_id = R_nc_protect (ScalarInteger (filter_id));
      rfilter_params = R_nc_protect (allocVector (INTSXP, filter_nparams));
      if (filter_nparams > 0) {
        R_nc_check (nc_inq_var_filter (ncid, varid, NULL, NULL,
                      (unsigned int *) INTEGER (rfilter_params)));
      }
    } else if (status == NC_EFILTER) {
      rfilter_id = R_nc_protect (ScalarInteger (NA_INTEGER));
      rfilter_params = R_nc_protect (ScalarInteger (NA_INTEGER));
    } else {
      R_nc_check (status);
      return R_NilValue;
    }
#else
    rfilter_id = R_NilValue;
    rfilter_params = R_NilValue;
#endif

  } else {
    rdeflate = R_NilValue;
    rshuffle = R_NilValue;
    rendian = R_NilValue;
    rfletcher = R_NilValue;
    rszip_bits = R_NilValue;
    rszip_options = R_NilValue;
    rfilter_id = R_NilValue;
    rfilter_params = R_NilValue;
  }

  /*-- Convert nc_type to char ------------------------------------------------*/
  R_nc_check (R_nc_type2str (ncid, xtype, vartype));

  /*-- Construct the output list ----------------------------------------------*/
  if (withnc4) {
    result = R_nc_protect (allocVector (VECSXP, 18));
  } else {
    result = R_nc_protect (allocVector (VECSXP, 6));
  }

  SET_VECTOR_ELT (result, 0, ScalarInteger (varid));
  SET_VECTOR_ELT (result, 1, mkString (varname));
  SET_VECTOR_ELT (result, 2, mkString (vartype));
  SET_VECTOR_ELT (result, 3, ScalarInteger (ndims));
  SET_VECTOR_ELT (result, 4, rdimids);
  SET_VECTOR_ELT (result, 5, ScalarInteger (natts));

  if (withnc4) {
    SET_VECTOR_ELT (result, 6, rchunks);
    SET_VECTOR_ELT (result, 7, rbytes);
    SET_VECTOR_ELT (result, 8, rslots);
    SET_VECTOR_ELT (result, 9, rpreempt);
    SET_VECTOR_ELT (result, 10, rdeflate);
    SET_VECTOR_ELT (result, 11, rshuffle);
    SET_VECTOR_ELT (result, 12, rendian);
    SET_VECTOR_ELT (result, 13, rfletcher);
    SET_VECTOR_ELT (result, 14, rszip_options);
    SET_VECTOR_ELT (result, 15, rszip_bits);
    SET_VECTOR_ELT (result, 16, rfilter_id);
    SET_VECTOR_ELT (result, 17, rfilter_params);
  }

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_var()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP data,
              SEXP namode, SEXP pack,
              SEXP cache_bytes, SEXP cache_slots, SEXP cache_preemption)
{
  int ncid, varid, ndims, ii, inamode, ispack;
  size_t *cstart=NULL, *ccount=NULL;
  nc_type xtype;
  const void *buf;
  double scale, add, *scalep=NULL, *addp=NULL;
  void *fillp=NULL, *minp=NULL, *maxp=NULL;
  size_t fillsize, bytes, slots;
  float preemption;
  double bytes_in, slots_in, preempt_in;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  inamode = asInteger (namode);
  ispack = (asLogical (pack) == TRUE);

  /*-- Chunk cache options for netcdf4 files ----------------------------------*/
#ifdef HAVE_NC_GET_VAR_CHUNK_CACHE
  if (nc_get_var_chunk_cache(ncid, varid,
                             &bytes, &slots, &preemption) == NC_NOERR) {
    bytes_in = asReal (cache_bytes);
    slots_in = asReal (cache_slots);
    preempt_in = asReal (cache_preemption);
    if (R_FINITE(bytes_in) || R_FINITE(slots_in) || R_FINITE(preempt_in)) {
      if (R_FINITE(bytes_in)) {
	bytes = bytes_in;
      }
      if (R_FINITE(slots_in)) {
	slots = slots_in;
      }
      if (R_FINITE(preempt_in)) {
	preemption = preempt_in;
      }
      R_nc_check (nc_set_var_chunk_cache(ncid, varid,
                                         bytes, slots, preemption));
    }
  }
#endif

  /*-- Get type and rank of the variable --------------------------------------*/
  R_nc_check (nc_inq_var (ncid, varid, NULL, &xtype, &ndims, NULL, NULL));

  /*-- Convert start and count from R to C indices ----------------------------*/
  if (ndims > 0) {
    cstart = R_nc_dim_r2c_size (start, ndims, 0);
    ccount = R_nc_dim_r2c_size (count, ndims, 0);
    for (ii=0; ii<ndims; ii++) {
      cstart[ii] -= 1;
    }
  }

  /*-- Get fill attributes (if any) -------------------------------------------*/
  fillsize = R_nc_miss_att (ncid, varid, inamode, &fillp, &minp, &maxp);

  /*-- Get packing attributes (if any) ----------------------------------------*/
  if (ispack) {
    scalep = &scale;
    addp = &add;
    R_nc_pack_att (ncid, varid, &scalep, &addp);
  }

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Write variable to file -------------------------------------------------*/
  if (R_nc_length (ndims, ccount) > 0) {
    buf = R_nc_r2c (data, ncid, xtype, ndims, ccount, fillp, scalep, addp);
    R_nc_check (nc_put_vara (ncid, varid, cstart, ccount, buf));
  }

  RRETURN (R_NilValue);
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

  cnewname = R_nc_strarg (newname);

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the variable ----------------------------------------------------*/
  R_nc_check (nc_rename_var (ncid, varid, cnewname));

  RRETURN(R_NilValue);
}

