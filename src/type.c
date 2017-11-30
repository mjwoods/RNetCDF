/*=============================================================================*\
 *
 *  Name:       type.c
 *
 *  Version:    2.0-1
 *
 *  Purpose:    NetCDF type functions for RNetCDF
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
#include "type.h"


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_type()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_type (SEXP nc, SEXP typename, SEXP class, SEXP basetype, SEXP size)
{
  int ncid;
  char mode;
  const char *typenamep;
  nc_type typeid=0, xtype=0;
  size_t xsize=0;
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

  switch (mode) {
  case 'e':
  case 'v':
    R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (basetype, 0)), &xtype));
    break;
  case 'c':
  case 'o':
    if (isInteger (size)) {
      xsize = asInteger (size);
    } else {
      xsize = asReal (size);
    }
    break;
  default:
    RERROR ("Unknown class for type definition");
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
  }

  result = R_nc_protect (ScalarInteger (typeid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_insert_type()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_insert_type (SEXP nc, SEXP type, SEXP name, SEXP value,
  SEXP offset, SEXP subtype, SEXP dimsizes)
{
  int ncid, ndims=0;
  nc_type typeid, xtype;
  const char *fldname;
  int class, *csizes=NULL;
  size_t coffset=0;
  void *tmpval=NULL;

  /*-- Decode arguments -------------------------------------------------------*/
  ncid = asInteger (nc);

  if (isString (type)) {
    R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &typeid));
  } else {
    typeid = asInteger (type);
  }

  fldname = CHAR (STRING_ELT (name, 0));

  R_nc_check (nc_inq_user_type (ncid, typeid, NULL, NULL, &xtype, NULL, &class));

  if (class == NC_ENUM) {
    if (!isNull (value)) {
      R_nc_r2c (value, &tmpval, 0, 1, xtype, NULL, NULL, NULL);
    } else {
      RERROR ("No value given for enumerated type");
    }
  } else if (class == NC_COMPOUND) {
    if (!isNull (offset) && !isNull (subtype)) {

      if (isInteger (offset)) {
        coffset = asInteger (offset);
      } else {
        /* offset could be larger than integer */
        coffset = asReal (offset);
      }

      if (isString (subtype)) {
        R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (subtype, 0)),
                                   &xtype));
      } else {
        xtype = asInteger (subtype);
      }

      if (isNull (dimsizes)) {
        ndims = 0;
      } else {
        ndims = length (dimsizes);
        if (ndims > 0) {
          csizes = (void *) R_alloc (ndims, sizeof (int));
          R_nc_dim_r2c_int(dimsizes, ndims, -1, csizes);
        }
      }
    } else {
      RERROR ("No offset or subtype given for compound type");
    }
  } else {
    RERROR ("Expected enumerated or compound type");
  }

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Insert the member or field ---------------------------------------------*/
  if (class == NC_ENUM) {
    R_nc_check (nc_insert_enum (ncid, typeid, fldname, &tmpval));
  } else if (class == NC_COMPOUND) {
    if (ndims > 0) {
      R_nc_check (nc_insert_array_compound (ncid, typeid, fldname,
                    coffset, xtype, ndims, csizes));
    } else {
      R_nc_check (nc_insert_compound (ncid, typeid, fldname, coffset, xtype));
    }
  }

  RRETURN(R_NilValue);
}

