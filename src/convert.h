/*=============================================================================*\
 *
 *  Name:       convert.h
 *
 *  Version:    2.1-1
 *
 *  Purpose:    Type conversions for RNetCDF
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

#ifndef RNC_CONVERT_H_INCLUDED
#define RNC_CONVERT_H_INCLUDED


/* Find total number of elements in an array from dimension lengths.
   Result is 1 for a scalar or product of dimensions for an array.
   The special case ndims < 0 implies a vector of length count[0].
 */
size_t
R_nc_length (int ndims, const size_t *count);

size_t
R_nc_length_sexp (SEXP count);


/* Allocate array with dimensions specified in C order.
   ndims > 0 implies an array with ndims dimension lengths in ccount[].
   ndims == 0 implies a scalar (vector of length 1).
   ndims < 0 implies a dimensionless vector of length ccount[0].
 */
SEXP
R_nc_allocArray (SEXPTYPE type, int ndims, const size_t *ccount);


/* Structure whose members are used by R_nc_c2r_init and R_nc_c2r.
   Other functions should not access members directly. */
typedef struct {
  SEXP rxp;
  void *cbuf, *rbuf;
  nc_type xtype;
  int ncid, ndim, rawchar, fitnum;
  size_t *xdim, fillsize;
  void *fill, *min, *max;
  double *scale, *add;
  } R_nc_buf;


/* Convert an R vector to a netcdf external type (xtype).
   Memory for the results is allocated by R_alloc (freed by R),
   except in special cases where no modification of the input is required,
   when the output is a pointer to the input data.
   The number and lengths of netcdf dimensions are ndim and xdim (C-order).
   An error is raised for out-of-range values.
   Missing and NaN values are replaced by a fill value.
   Packing is performed if either scale or add are not NULL.
 */
const void *
R_nc_r2c (SEXP rv, int ncid, nc_type xtype, int ndim, const size_t *xdim,
          size_t fillsize, const void *fill,
          const double *scale, const double *add);


/* Convert an array of netcdf external type (xtype) to R.
   Memory buffers for R and (optionally) C arrays are allocated by R_nc_c2r_init;
   the C to R conversion is performed by R_nc_c2r, and memory is freed by R.
   The SEXP result of R_nc_c2r_init should be PROTECTed by the caller.
   Argument io is a pointer to an existing R_nc_buf (must not be NULL).
   Argument cbuf is a pointer to a pointer to a buffer for netcdf data,
   which will be allocated internally if *cbuf is NULL.
   The number and lengths of netcdf dimensions are ndim and xdim (C-order).
   The special case ndims < 0 gives a vector (no dim attribute) of length xdim[0].
   If fitnum is true (non-zero), rv is the smallest compatible R numeric type,
     otherwise rv is double precision.
   If rawchar is true, NC_CHAR is returned to R as raw bytes, otherwise
     all elements in the fastest-varying dimension are combined into R strings.
   Elements are set to missing if they equal the fill value.
   Unpacking is performed if either scale or add are not NULL.
 */
SEXP \
R_nc_c2r_init (R_nc_buf *io, void **cbuf,
               int ncid, nc_type xtype, int ndim, const size_t *xdim,
               int rawchar, int fitnum, size_t fillsize,
               const void *fill, const void *min, const void *max,
               const double *scale, const double *add);

void \
R_nc_c2r (R_nc_buf *io);


/* Reverse a vector in-place.
   Example: R_nc_rev_int (cv, cnt);
 */
#define R_NC_REVERSE_H(FUN, TYPE) \
void \
FUN (TYPE *data, size_t cnt);
R_NC_REVERSE_H(R_nc_rev_int, int)
R_NC_REVERSE_H(R_nc_rev_size, size_t)
/* Define R_nc_rev for other types as needed */


/* Copy the leading nr elements of R vector rv to C vector cv,
   converting type to TYPE and reversing from Fortran to C storage order.
   Elements beyond the length of rv and non-finite values are stored as fillval.
 */
#define R_NC_DIM_R2C_H(FUN, TYPE) \
TYPE * \
FUN (SEXP rv, size_t nr, TYPE fillval);

R_NC_DIM_R2C_H (R_nc_dim_r2c_int, int)
R_NC_DIM_R2C_H (R_nc_dim_r2c_size, size_t)


#endif /* RNC_CONVERT_H_INCLUDED */

