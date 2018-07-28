/*=============================================================================*\
 *
 *  Name:       convert.h
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
 * $Header$
\*=============================================================================*/

#ifndef RNC_CONVERT_H_INCLUDED
#define RNC_CONVERT_H_INCLUDED


#define NA_SIZE SIZE_MAX

/* Definitions for integer64 as provided by bit64 package */
#define NA_INTEGER64 LLONG_MIN
int isInt64(SEXP rv);


/* Structure for buffers used in IO.
   Element buf may point to memory inside rxp.
 */
typedef struct {
  SEXP rxp;
  void *buf;
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
R_nc_r2c (SEXP rv, int ncid, nc_type xtype, int ndim, size_t *xdim,
          void *fill, double *scale, double *add);


/* Convert an array of netcdf external type (xtype) to R.
   Memory for the results is allocated by allocArray.
   The number and lengths of netcdf dimensions are ndim and xdim (C-order).
   If fitnum is true (non-zero), the rv is the smallest R numeric type,
     otherwise rv is double precision.
   Elements are set to missing if they equal the fill value.
   Unpacking is performed if either scale or add are not NULL.
 */
SEXP
R_nc_c2r (void *cv, int ncid, nc_type xtype, int ndim, size_t *xdim, int fitnum,
          void *fill, double *scale, double *add);


/* Reverse a vector in-place.
   Example: R_nc_rev_int (cv, cnt);
 */
#define R_NC_REVERSE_H(FUN, TYPE) \
void \
FUN (TYPE *data, size_t cnt);
R_NC_REVERSE_H(R_nc_rev_int, int);
R_NC_REVERSE_H(R_nc_rev_size, size_t);
/* Define R_nc_rev for other types as needed */


/* Copy the leading nr elements of R vector rv to C vector cv,
   converting type to TYPE and reversing from Fortran to C storage order.
   Elements beyond the length of rv and non-finite values are stored as fillval.
 */
#define R_NC_DIM_R2C_H(FUN, TYPE) \
void \
FUN (SEXP rv, size_t nr, TYPE fillval, TYPE *cv);

R_NC_DIM_R2C_H (R_nc_dim_r2c_int, int);
R_NC_DIM_R2C_H (R_nc_dim_r2c_size, size_t);


#endif /* RNC_CONVERT_H_INCLUDED */

