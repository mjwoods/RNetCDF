/*=============================================================================*\
 *
 *  Name:       type.c
 *
 *  Version:    2.10-3
 *
 *  Purpose:    NetCDF type functions for RNetCDF
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright (C) 2004-2025 Pavel Michna and Milton Woods.
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

#include "config.h"

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
 *  R_nc_def_type()
\*-----------------------------------------------------------------------------*/

/* Private functions called by R_nc_def_type */

static nc_type
R_nc_def_compound (int ncid, const char *typename,
                   SEXP names, SEXP subtypes, SEXP dimsizes)
{
  int ndims, status;
  nc_type typeid, *xtypes;
  int class, *csizes;
  size_t ifld, nfld, *coffset, xsize, xsizemax, xcnt, typesize, align, nskip;
  SEXP shape;

  /*-- Check arguments -------------------------------------------------------*/
  nfld = xlength (names);
  if ((size_t) xlength (subtypes) != nfld ||
      (size_t) xlength (dimsizes) != nfld) {
    error ("Lengths of names, subtypes and dimsizes must match");
  } else if (nfld < 1) {
    error ("Compound type must have at least one field");
  }

  /*-- Calculate field offsets with suitable alignment for each native subtype,
     recording the total size of the compound type and its largest subtype ---*/
  coffset = (size_t *) R_alloc (nfld, sizeof(size_t));
  xtypes = (nc_type *) R_alloc (nfld, sizeof(nc_type));
  typesize = 0;
  xsizemax = 0;
  for (ifld=0; ifld<nfld; ifld++) {
    R_nc_check (R_nc_type_id (subtypes, ncid, &(xtypes[ifld]), ifld));
    R_nc_check (nc_inq_type(ncid, xtypes[ifld], NULL, &xsize));
    if (xsize > xsizemax) {
      xsizemax = xsize;
    }
    xcnt = R_nc_length_sexp (VECTOR_ELT (dimsizes, ifld));

    align = xsize < 8 ? xsize : 8;
    if (typesize % align == 0) {
      coffset[ifld] = typesize;
    } else {
      coffset[ifld] = align * (typesize / align + 1);
    }
    typesize = coffset[ifld] + xsize * xcnt;
  }

  /*-- Pad the compound type to align the largest native subtype -------------*/
  align = xsizemax < 8 ? xsizemax : 8;
  if (typesize % align != 0) {
    typesize = align * (typesize / align + 1);
  }

  /*-- Enter define mode -----------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Define the type, continuing if a compatible type already exists -------*/
  status = nc_def_compound (ncid, typesize, typename, &typeid);
  if (status == NC_ENAMEINUSE) {
    R_nc_check (nc_inq_typeid (ncid, typename, &typeid));
    R_nc_check (nc_inq_user_type (ncid, typeid, NULL, &xsize,
                                  NULL, NULL, &class));
    if (class != NC_COMPOUND || xsize != typesize) {
      error ("Existing type has same name but different class or size");
    }
    warning("Inserting fields in existing type %s", typename);
  } else {
    R_nc_check (status);
  }

  /*-- Insert fields, skipping any field that already exists -----------------*/
  nskip = 0;
  for (ifld=0; ifld<nfld; ifld++) {
    shape = VECTOR_ELT (dimsizes, ifld);

    csizes = NULL;
    if (isNull (shape)) {
      ndims = 0;
    } else if (isNumeric (shape)) {
      ndims = length (shape);
      if (ndims > 0) {
	csizes = R_nc_dim_r2c_int(shape, ndims, 0);
      }
    } else {
      error ("Dimensions of field must be numeric or null");
      return NC_NAT;
    }

    status = nc_insert_array_compound (ncid, typeid,
               CHAR (STRING_ELT(names, ifld)),
               coffset[ifld], xtypes[ifld], ndims, csizes);
    if (status == NC_ENAMEINUSE) {
      nskip++;
    } else {
      R_nc_check (status);
    }
  }
  if (nskip > 0) {
    warning("Skipped existing fields of type %s", typename);
  }

  return typeid;
}


static nc_type
R_nc_def_enum (int ncid, const char *typename, SEXP basetype,
               SEXP names, SEXP values)
{
  nc_type xtype, xtype2, typeid;
  int status, class;
  size_t size, ival, nval, nskip;
  const char *tmpname, *cvals, *thisval;

  /*-- Decode arguments -------------------------------------------------------*/
  R_nc_check (R_nc_type_id (basetype, ncid, &xtype, 0));
  nval = xlength (values);
  if ((size_t) xlength (names) != nval) {
    error ("Lengths of names and values must match");
  }

  cvals = R_nc_r2c (values, ncid, xtype, 1, &nval, 0, NULL, NULL, NULL);

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check (R_nc_redef (ncid));

  /*-- Define the type, continuing if a compatible type already exists --------*/
  status = nc_def_enum (ncid, xtype, typename, &typeid);
  if (status == NC_ENAMEINUSE) {
    R_nc_check (nc_inq_typeid (ncid, typename, &typeid));
    R_nc_check (nc_inq_user_type (ncid, typeid, NULL, NULL,
                                  &xtype2, NULL, &class));
    if (class != NC_ENUM || xtype != xtype2) {
      error ("Existing type has same name but different class or basetype");
    }
    warning("Inserting members in existing type %s", typename);
  } else {
    R_nc_check (status);
  }

  /*-- Insert members, skipping any member that already exists ----------------*/
  R_nc_check (nc_inq_type(ncid, typeid, NULL, &size));
  nskip = 0;
  for (ival=0, thisval=cvals; ival<nval; ival++, thisval+=size) {
    tmpname = CHAR (STRING_ELT (names, ival));
    status = nc_insert_enum (ncid, typeid, tmpname, thisval);
    if (status == NC_ENAMEINUSE) {
      nskip++;
    } else {
      R_nc_check (status);
    }
  }
  if (nskip > 0) {
    warning("Skipped existing members of type %s", typename);
  }

  return typeid;
}


SEXP
R_nc_def_type (SEXP nc, SEXP typename, SEXP class, SEXP size, SEXP basetype,
               SEXP names, SEXP values, SEXP subtypes, SEXP dimsizes)
{
  int ncid;
  const char *typenamep;
  nc_type typeid=0, xtype=0;
  size_t xsize=0;

  /*-- Decode arguments -------------------------------------------------------*/
  ncid = asInteger (nc);

  typenamep = R_nc_strarg (typename);

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Create the type --------------------------------------------------------*/
  if (R_nc_strcmp (class, "compound")) {
    typeid = R_nc_def_compound (ncid, typenamep, names, subtypes, dimsizes);
  } else if (R_nc_strcmp (class, "enum")) {
    typeid = R_nc_def_enum (ncid, typenamep, basetype, names, values);
  } else if (R_nc_strcmp (class, "opaque")) {
    xsize = R_nc_sizearg (size);
    R_nc_check (nc_def_opaque (ncid, xsize, typenamep, &typeid));
  } else if (R_nc_strcmp (class, "vlen")) {
    R_nc_check (R_nc_type_id (basetype, ncid, &xtype, 0));
    R_nc_check (nc_def_vlen (ncid, typenamep, xtype, &typeid));
  } else {
    error ("Unknown class for type definition");
  }

  return ScalarInteger (typeid);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_insert_type()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_insert_type (SEXP nc, SEXP type, SEXP name, SEXP value,
  SEXP offset, SEXP subtype, SEXP dimsizes)
{
  int ncid, idim, ndims=0;
  nc_type typeid, xtype;
  const char *fldname;
  int class, *csizes=NULL;
  size_t coffset=0, xsize, subsize, nelem;
  const void *tmpval=NULL;

  /*-- Decode arguments -------------------------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_type_id (type, ncid, &typeid, 0));

  fldname = R_nc_strarg (name);

  R_nc_check (nc_inq_user_type (ncid, typeid, NULL, &xsize, &xtype, NULL, &class));

  if (class == NC_ENUM) {
    if (!isNull (value)) {
      tmpval = R_nc_r2c (value, ncid, xtype, 0, NULL, 0, NULL, NULL, NULL);
    } else {
      error ("No value given for enumerated type");
    }
  } else if (class == NC_COMPOUND) {
    if (!isNull (offset) && !isNull (subtype)) {

      coffset = R_nc_sizearg (offset);

      R_nc_check (R_nc_type_id (subtype, ncid, &xtype, 0));
      R_nc_check (nc_inq_type (ncid, xtype, NULL, &subsize));

      nelem = 1;
      if (isNull (dimsizes)) {
        ndims = 0;
      } else {
        ndims = length (dimsizes);
        if (ndims > 0) {
          csizes = R_nc_dim_r2c_int(dimsizes, ndims, -1);
          for (idim=0; idim<ndims; idim++) {
            nelem *= csizes[idim];
          }
        }
      }

      if ( (coffset + subsize*nelem) > xsize) {
        error("Field exceeds size of compound type");
      }
// Keep size checks; allow repeat definition with same details.
    } else {
      error ("Missing offset or subtype for compound type");
    }
  } else {
    error ("Expected enumerated or compound type");
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

  return R_NilValue;
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
  char *cval;
  SEXP result=R_NilValue, resultnames=R_NilValue;
  SEXP fieldnames, values, offsets, subnames, dimsize, dimsizes;
  R_nc_buf io;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);
  R_nc_check (R_nc_type_id (type, ncid, &xtype, 0));
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
        result = PROTECT(allocVector (VECSXP, 7));

        resultnames = PROTECT(allocVector (STRSXP, 7));
        setAttrib (result, R_NamesSymbol, resultnames);
        UNPROTECT(1);
        SET_STRING_ELT (resultnames, 4, PROTECT(mkChar ("offset")));
        SET_STRING_ELT (resultnames, 5, PROTECT(mkChar ("subtype")));
        SET_STRING_ELT (resultnames, 6, PROTECT(mkChar ("dimsizes")));
        UNPROTECT(3);

	/* Read named vectors of offsets, typenames, list of array dimensions */
	offsets = PROTECT(allocVector (REALSXP, nfields));
	subnames = PROTECT(allocVector (STRSXP, nfields));
	dimsizes = PROTECT(allocVector (VECSXP, nfields));
        SET_VECTOR_ELT (result, 4, offsets);
        SET_VECTOR_ELT (result, 5, subnames);
        SET_VECTOR_ELT (result, 6, dimsizes);
        UNPROTECT(3);

	fieldnames = PROTECT(allocVector (STRSXP, nfields));
        setAttrib (offsets, R_NamesSymbol, fieldnames);
        setAttrib (subnames, R_NamesSymbol, fieldnames);
        setAttrib (dimsizes, R_NamesSymbol, fieldnames);
        UNPROTECT(1);

	imax = nfields; // netcdf field index is int
	for (ii=0; ii < imax; ii++) {
	  R_nc_check (nc_inq_compound_field (ncid, xtype, ii, fieldname,
		      &offset, &subtype, &ndims, NULL));
	  SET_STRING_ELT (fieldnames, ii, PROTECT(mkChar (fieldname)));
	  REAL (offsets)[ii] = offset;
	  R_nc_check (R_nc_type2str (ncid, subtype, subname));
	  SET_STRING_ELT (subnames, ii, PROTECT(mkChar (subname)));
          UNPROTECT(2);
	  if (ndims > 0) {
	    dimsize = PROTECT(allocVector (INTSXP, ndims));
	    R_nc_check (nc_inq_compound_fielddim_sizes (
			ncid, xtype, ii, INTEGER (dimsize)));
	    SET_VECTOR_ELT (dimsizes, ii, dimsize);
            UNPROTECT(1);
	  }
	}

      } else {
        result = PROTECT(allocVector (VECSXP, 4));
        resultnames = PROTECT(allocVector (STRSXP, 4));
        setAttrib (result, R_NamesSymbol, resultnames);
        UNPROTECT(1);
      }

      SET_VECTOR_ELT (result, 2, PROTECT(mkString ("compound")));
      UNPROTECT(1);
      break;
    case NC_ENUM:
      R_nc_check (R_nc_type2str (ncid, basetype, basename));

      if (extend) {
	result = PROTECT(allocVector (VECSXP, 6));

	resultnames = PROTECT(allocVector (STRSXP, 6));
        setAttrib (result, R_NamesSymbol, resultnames);
	SET_STRING_ELT (resultnames, 5, PROTECT(mkChar ("value")));
        UNPROTECT(2);

	/* Read named vector of member values */
	fieldnames = PROTECT(allocVector (STRSXP, nfields));
        cval = NULL;
        values = PROTECT(R_nc_c2r_init (&io, (void **) &cval, ncid, basetype,
                           -1, &nfields, 0, 1,
                           0, NULL, NULL, NULL, NULL, NULL));

	imax = nfields; // netcdf member index is int
	for (ii=0; ii < imax; ii++, cval+=size) {
	  R_nc_check (nc_inq_enum_member (ncid, xtype, ii, fieldname, cval));
	  SET_STRING_ELT (fieldnames, ii, PROTECT(mkChar (fieldname)));
          UNPROTECT(1);
	}
	R_nc_c2r (&io);
	SET_VECTOR_ELT (result, 5, values);
	setAttrib (values, R_NamesSymbol, fieldnames);
        UNPROTECT(2);

      } else {
	result = PROTECT(allocVector (VECSXP, 5));
	resultnames = PROTECT(allocVector (STRSXP, 5));
        setAttrib (result, R_NamesSymbol, resultnames);
        UNPROTECT(1);
      }

      SET_VECTOR_ELT (result, 2, PROTECT(mkString ("enum")));
      SET_VECTOR_ELT (result, 4, PROTECT(mkString (basename)));
      SET_STRING_ELT (resultnames, 4, PROTECT(mkChar ("basetype")));
      UNPROTECT(3);
      break;
    case NC_OPAQUE:

      result = PROTECT(allocVector (VECSXP, 4));
      SET_VECTOR_ELT (result, 2, PROTECT(mkString ("opaque")));

      resultnames = PROTECT(allocVector (STRSXP, 4));
      setAttrib (result, R_NamesSymbol, resultnames);
      UNPROTECT(2);

      break;
    case NC_VLEN:
      R_nc_check (R_nc_type2str (ncid, basetype, basename));

      result = PROTECT(allocVector (VECSXP, 5));
      SET_VECTOR_ELT (result, 2, PROTECT(mkString ("vlen")));
      SET_VECTOR_ELT (result, 4, PROTECT(mkString (basename)));

      resultnames = PROTECT(allocVector (STRSXP, 5));
      setAttrib (result, R_NamesSymbol, resultnames);
      SET_STRING_ELT (resultnames, 4, PROTECT(mkChar ("basetype")));
      UNPROTECT(4);

      break;
    default:
      error ("Unknown class of user defined type");
    }

  } else {
  /*-- Built-in types ---------------------------------------------------------*/

      result = PROTECT(allocVector (VECSXP, 4));
      SET_VECTOR_ELT (result, 2, PROTECT(mkString ("builtin")));

      resultnames = PROTECT(allocVector (STRSXP, 4));
      setAttrib (result, R_NamesSymbol, resultnames);
      UNPROTECT(2);
  }

  /*-- Common components of output list ----------------------------------------------*/
  SET_VECTOR_ELT (result, 0, PROTECT(ScalarInteger (xtype)));
  SET_VECTOR_ELT (result, 1, PROTECT(mkString (typename)));
  SET_VECTOR_ELT (result, 3, PROTECT(ScalarReal (size)));

  SET_STRING_ELT (resultnames, 0, PROTECT(mkChar ("id")));
  SET_STRING_ELT (resultnames, 1, PROTECT(mkChar ("name")));
  SET_STRING_ELT (resultnames, 2, PROTECT(mkChar ("class")));
  SET_STRING_ELT (resultnames, 3, PROTECT(mkChar ("size")));

  UNPROTECT(8);
  return result;
}
