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
#include "RNetCDF.h"

#ifndef NC_MAX_ATOMIC_TYPE
  #define NC_MAX_ATOMIC_TYPE NC_STRING
#endif


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
      tmpval = R_nc_r2c (value, ncid, xtype, 0, NULL, NULL, NULL, NULL);
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
          csizes = R_nc_dim_r2c_int(dimsizes, ndims, -1);
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
    R_nc_check (nc_insert_enum (ncid, typeid, fldname, tmpval));
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


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_type()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_type (SEXP nc, SEXP type, SEXP fields)
{
  int ncid, class, extend;
  nc_type xtype, basetype, subtype;
  char typename[NC_MAX_NAME + 1], basename[NC_MAX_NAME + 1];
  char fieldname[NC_MAX_NAME + 1], subname[NC_MAX_NAME + 1];
  size_t size, nfields, offset;
  int ii, imax, ndims;
  void *cval;
  SEXP result, resultnames;
  SEXP fieldnames, values, offsets, subnames, dimsize, dimsizes;
  R_nc_buf io;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);
  R_nc_check (R_nc_type_id (type, ncid, &xtype));
  extend = (asLogical (fields) == TRUE);

  /*-- General properties -----------------------------------------------------*/
  R_nc_check (nc_inq_type (ncid, xtype, NULL, &size));
  R_nc_check (R_nc_type2str (ncid, xtype, typename));

  if (xtype > NC_MAX_ATOMIC_TYPE) {
  /*-- User-defined types -----------------------------------------------------*/

    R_nc_check (nc_inq_user_type (ncid, xtype, NULL, NULL, &basetype, &nfields, &class));

    switch (class) {
    case NC_COMPOUND:
      if (extend) {
	/* Read named vectors of offsets, typenames, list of array dimensions */
	fieldnames = R_nc_protect (allocVector (STRSXP, nfields));
	offsets = R_nc_protect (allocVector (REALSXP, nfields));
	subnames = R_nc_protect (allocVector (STRSXP, nfields));
	dimsizes = R_nc_protect (allocVector (VECSXP, nfields));

	imax = nfields; // netcdf field index is int
	for (ii=0; ii < imax; ii++) {
	  R_nc_check (nc_inq_compound_field (ncid, xtype, ii, fieldname,
		      &offset, &subtype, &ndims, NULL));
	  SET_STRING_ELT (fieldnames, ii, mkChar (fieldname));
	  REAL (offsets)[ii] = offset;
	  R_nc_check (R_nc_type2str (ncid, subtype, subname));
	  SET_STRING_ELT (subnames, ii, mkChar (subname));
	  if (ndims > 0) {
	    dimsize = R_nc_protect (allocVector (INTSXP, ndims));
	    R_nc_check (nc_inq_compound_fielddim_sizes (
			ncid, xtype, ii, INTEGER (dimsize)));
	    SET_VECTOR_ELT (dimsizes, ii, dimsize);
	  }
	}

        setAttrib (offsets, R_NamesSymbol, fieldnames);
        setAttrib (subnames, R_NamesSymbol, fieldnames);
        setAttrib (dimsizes, R_NamesSymbol, fieldnames);

        result = R_nc_protect (allocVector (VECSXP, 7));
        SET_VECTOR_ELT (result, 4, offsets);
        SET_VECTOR_ELT (result, 5, subnames);
        SET_VECTOR_ELT (result, 6, dimsizes);

        resultnames = R_nc_protect (allocVector (STRSXP, 7));
        SET_STRING_ELT (resultnames, 4, mkChar ("offset"));
        SET_STRING_ELT (resultnames, 5, mkChar ("subtype"));
        SET_STRING_ELT (resultnames, 6, mkChar ("dimsizes"));

      } else {
        result = R_nc_protect (allocVector (VECSXP, 4));
        resultnames = R_nc_protect (allocVector (STRSXP, 4));
      }

      SET_VECTOR_ELT (result, 2, mkString ("compound"));

      break;
    case NC_ENUM:
      R_nc_check (R_nc_type2str (ncid, basetype, basename));

      if (extend) {
	/* Read named vector of member values */
	fieldnames = R_nc_protect (allocVector (STRSXP, nfields));
        cval = R_nc_c2r_init (&io, ncid, basetype, -1, &nfields,
                              0, 1, NULL, NULL, NULL);

	imax = nfields; // netcdf member index is int
	for (ii=0; ii < imax; ii++, cval+=size) {
	  R_nc_check (nc_inq_enum_member (ncid, xtype, ii, fieldname, cval));
	  SET_STRING_ELT (fieldnames, ii, mkChar (fieldname));
	}
	values = R_nc_c2r (&io);
	setAttrib (values, R_NamesSymbol, fieldnames);

	result = R_nc_protect (allocVector (VECSXP, 6));
	SET_VECTOR_ELT (result, 5, values);

	resultnames = R_nc_protect (allocVector (STRSXP, 6));
	SET_STRING_ELT (resultnames, 5, mkChar ("value"));

      } else {
	result = R_nc_protect (allocVector (VECSXP, 5));
	resultnames = R_nc_protect (allocVector (STRSXP, 5));
      }
      SET_VECTOR_ELT (result, 2, mkString ("enum"));
      SET_VECTOR_ELT (result, 4, mkString (basename));
      SET_STRING_ELT (resultnames, 4, mkChar ("basetype"));

      break;
    case NC_OPAQUE:

      result = R_nc_protect (allocVector (VECSXP, 4));
      SET_VECTOR_ELT (result, 2, mkString ("opaque"));

      resultnames = R_nc_protect (allocVector (STRSXP, 4));

      break;
    case NC_VLEN:
      R_nc_check (R_nc_type2str (ncid, basetype, basename));

      result = R_nc_protect (allocVector (VECSXP, 5));
      SET_VECTOR_ELT (result, 2, mkString ("vlen"));
      SET_VECTOR_ELT (result, 4, mkString (basename));

      resultnames = R_nc_protect (allocVector (STRSXP, 5));
      SET_STRING_ELT (resultnames, 4, mkChar ("basetype"));

      break;
    default:
      R_nc_error ("Unknown class of user defined type");
    }

  } else {
  /*-- Built-in types ---------------------------------------------------------*/

      result = R_nc_protect (allocVector (VECSXP, 4));
      SET_VECTOR_ELT (result, 2, mkString ("builtin"));

      resultnames = R_nc_protect (allocVector (STRSXP, 4));
  }

  /*-- Common components of output list ----------------------------------------------*/
  SET_VECTOR_ELT (result, 0, ScalarInteger (xtype));
  SET_VECTOR_ELT (result, 1, mkString (typename));
  SET_VECTOR_ELT (result, 3, ScalarReal (size));

  SET_STRING_ELT (resultnames, 0, mkChar ("id"));
  SET_STRING_ELT (resultnames, 1, mkChar ("name"));
  SET_STRING_ELT (resultnames, 2, mkChar ("class"));
  SET_STRING_ELT (resultnames, 3, mkChar ("size"));
  setAttrib (result, R_NamesSymbol, resultnames);

  RRETURN(result);
}
