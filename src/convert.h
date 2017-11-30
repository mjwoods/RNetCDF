/*=============================================================================*\
 *									       *
 *  Name:       convert.h						       *
 *									       *
 *  Version:    2.0-1							       *
 *									       *
 *  Purpose:    Type conversions for RNetCDF                                   *
 *									       *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)			       *
 *              Milton Woods (miltonjwoods@gmail.com)                              *
 *									       *
 *  Copyright:  (C) 2004-2017 Pavel Michna, Milton Woods                       *
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

#ifndef RNC_CONVERT_H_INCLUDED
#define RNC_CONVERT_H_INCLUDED


#define NA_SIZE SIZE_MAX

/* TODO: roll these into R_nc_r2c and R_nc_c2r */

void
R_nc_strsxp_char (SEXP rstr, char *carr, size_t imin, size_t cnt,
                  size_t strlen);

void
R_nc_char_strsxp (char *carr, SEXP rstr,
                  size_t len, size_t imin, size_t cnt);


void
R_nc_strsxp_str (SEXP rstr, const char **cstr, size_t imin, size_t cnt);


void
R_nc_str_strsxp (char **cstr, SEXP rstr, size_t imin, size_t cnt);


/* Convert an R vector to a netcdf external type.
   Argument rv contains at least cnt values from index imin.
   Argument cv is a pointer to a C vector.
     If cv is NULL on entry, memory is allocated by R_alloc.
   An error is raised for out-of-range values.
   Missing and NaN values are replaced by a fill value.
   Packing is performed if either scale or add are not NULL.
   Example: R_nc_r2c (rv, cv, cnt, &fill, &scale, &add);
 */
void *
R_nc_r2c (SEXP rv, void *cv, size_t imin, size_t cnt, nc_type xtype,
          void *fill, double *scale, double *add);


/* Convert a vector of netcdf external type to R.
   Argument cv is assumed to contain cnt values of the external type.
   If fitnum is true (non-zero), the rv is the smallest compatible R type,
     otherwise rv is double precision.
   Elements are set to missing if they are NaN,
     equal to the fill value or outside the valid range (if not NULL).
   Unpacking is performed if either scale or add are not NULL.
   Example: R_nc_c2r (cv, rv, imin, cnt, xtype, 1,
                      &fill, &min, &max, &scale, &add);
 */
SEXP
R_nc_c2r (void *cv, size_t imin, size_t cnt, nc_type xtype, int fitnum,
          void *fill, void *min, void *max, double *scale, double *add);


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

