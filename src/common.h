/*=============================================================================*\
 *
 *  Name:       common.h
 *
 *  Version:    2.0-2
 *
 *  Purpose:    Common definitions for RNetCDF functions
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

#ifndef RNC_COMMON_H_INCLUDED
#define RNC_COMMON_H_INCLUDED

#include <limits.h>
#include <stdint.h>

#ifndef NC_MAX_ATOMIC_TYPE
  #define NC_MAX_ATOMIC_TYPE NC_STRING
#endif

#define RRETURN(object) { R_nc_unprotect (); return (object); }

#define RERROR(msg) { R_nc_error (msg); return NULL; }

#define NA_SIZE SIZE_MAX

/* Definition of missing value used by bit64 package */
#define NA_INTEGER64 LLONG_MIN

/* Common error strings */
static const char RNC_EDATALEN[]="Not enough data", \
  RNC_EDATATYPE[]="Incompatible data for external type", \
  RNC_ETYPEDROP[]="Unsupported external type";

/* Protect an object from garbage collection by R */
SEXP
R_nc_protect (SEXP obj);

/* Unprotect all objects to enable garbage collection by R */
void
R_nc_unprotect (void);

/* Raise an error in R */
void
R_nc_error(const char *msg);

/* If status is a netcdf error, raise an R error with a suitable message,
   otherwise return to caller. */
int
R_nc_check(int status);

/* Determine if a C string matches the first element of an R variable.
   Result is a logical value. */
int
R_nc_strcmp (SEXP var, const char *str);

/* Determine if an R object inherits from a given class.
   Result is a logical value. */
int
R_nc_inherits (SEXP var, const char *class);

/* Convert dimension identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
int
R_nc_dim_id (SEXP dim, int ncid, int *dimid, int idx);

/* Convert variable identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
int
R_nc_var_id (SEXP var, int ncid, int *varid);

/* Convert type identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
int
R_nc_type_id (SEXP type, int ncid, nc_type *xtype, int idx);

/* Convert netcdf type code to string label.
   Return NC_NOERR if ok, netcdf error code otherwise.
   The string buffer is assumed to have length NC_MAX_NAME or more.
 */
int
R_nc_type2str (int ncid, nc_type xtype, char *str);


/* Convert netcdf string label to type code.
   Return NC_NOERR if ok, netcdf error code otherwise.
 */
int
R_nc_str2type (int ncid, const char *str, nc_type * xtype);


/* Extract C string from R character vector argument.
   Raise an error if no string is found.
 */
const char *
R_nc_strarg (SEXP str);


/* Convert R numeric scalar argument to size_t.
   Raise an error if R type or value is not compatible.
 */
size_t
R_nc_sizearg (SEXP size);


/* Enter netcdf define mode if possible.
   Returns netcdf error code if an unhandled error occurs.
 */
int
R_nc_redef (int ncid);


/* Enter netcdf data mode if possible.
   Errors are ignored to avoid false alarms with some datasets (e.g. OPeNDAP),
   but we assume that subsequent function calls are checked for errors.
 */
int
R_nc_enddef (int ncid);


#endif /* RNC_COMMON_H_INCLUDED */
