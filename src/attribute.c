/*=============================================================================*
 *
 *  Name:       attribute.c
 *
 *  Version:    2.0-1
 *
 *  Purpose:    NetCDF attribute functions for RNetCDF
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


/* Convert attribute identifier from R string or number to a C string.
   Argument attname must have space for NC_MAX_NAME+1 characters.
   Result is a netcdf status value.
 */
static int
R_nc_att_name (SEXP att, int ncid, int varid, char *attname)
{
  if (isNumeric (att)) {
    return nc_inq_attname (ncid, varid, asInteger (att), attname);
  } else if (isString (att) && xlength (att) > 0) {
    strncpy (attname, CHAR (STRING_ELT (att, 0)), NC_MAX_NAME);
    attname[NC_MAX_NAME] = '\0';
    return NC_NOERR;
  } else {
    return NC_EINVAL;
  }
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_copy_att()
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
 *  R_nc_delete_att()
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
 *  R_nc_get_att()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_att (SEXP nc, SEXP var, SEXP att, SEXP rawchar, SEXP fitnum)
{
  int ncid, varid, israw, isfit;
  char attname[NC_MAX_NAME+1];
  size_t cnt;
  nc_type xtype;
  SEXP result;
  void *buf;
  R_nc_buf io;

  /*-- Convert arguments ------------------------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  R_nc_check (R_nc_att_name (att, ncid, varid, attname));

  israw = (asLogical (rawchar) == TRUE);
  isfit = (asLogical (fitnum) == TRUE);

  /*-- Get the attribute's type and size --------------------------------------*/
  R_nc_check(nc_inq_att (ncid, varid, attname, &xtype, &cnt));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Allocate memory and read attribute from file ---------------------------*/
  buf = R_nc_c2r_init (&io, NULL, ncid, xtype, -1, &cnt,
                       israw, isfit, NULL, NULL, NULL, NULL, NULL);
  if (cnt > 0) {
    R_nc_check (nc_get_att (ncid, varid, attname, buf));
  }
  result = R_nc_c2r (&io);

  RRETURN (result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_att()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_att (SEXP nc, SEXP var, SEXP att)
{
  int ncid, varid, attid;
  char attname[NC_MAX_NAME+1], atttype[NC_MAX_NAME+1];
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
  R_nc_check (R_nc_type2str (ncid, type, atttype));

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
 *  R_nc_put_att()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_att (SEXP nc, SEXP var, SEXP att, SEXP type, SEXP data)
{
  int ncid, varid, class, idim, ndimfld, *dimlenfld, ismatch;
  size_t cnt, xsize, ilist, nlist, datalen, typelen;
  nc_type xtype;
  const char *attname;
  const void *buf;
  char namefld[NC_MAX_NAME+1];
  SEXP namelist;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  attname = R_nc_strarg (att);

  R_nc_check (R_nc_type_id (type, ncid, &xtype, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Find number of input items of the specified type -----------------------*/
  if (xtype > NC_MAX_ATOMIC_TYPE) {
    R_nc_check (nc_inq_user_type (ncid, xtype, NULL, &xsize, NULL, NULL, &class));

    if (class == NC_COMPOUND && TYPEOF(data) == VECSXP) {

      /* Find number of elements in first field of compound type */
      R_nc_check (nc_inq_compound_field (ncid, xtype, 0, namefld,
                  NULL, NULL, &ndimfld, NULL));
      if (ndimfld > 0) {
        dimlenfld = (int *) R_alloc (ndimfld, sizeof(int));
        R_nc_check (nc_inq_compound_fielddim_sizes (ncid, xtype, 0, dimlenfld));
        typelen = 1;
        for (idim=0; idim<ndimfld; idim++) {
          typelen *= dimlenfld[idim];
        }
      } else {
        typelen = 1;
      }

      /* Find the field by name in the R input list */
      namelist = getAttrib (data, R_NamesSymbol);
      if (!isString (namelist)) {
	R_nc_error ("Named list required for conversion to compound type");
      }
      nlist = xlength (namelist);

      ismatch = 0;
      for (ilist=0; ilist<nlist; ilist++) {
        if (strcmp (CHAR (STRING_ELT (namelist, ilist)), namefld) == 0) {
          // ilist is the matching list index
          ismatch = 1;
          break;
        }
      }
      if (!ismatch) {
        R_nc_error ("Name of compound field not found in input list");
      }

      /* Find length of field in R input list */
      datalen = xlength (VECTOR_ELT (data, ilist));

      /* Number of compound elements in the R input list */
      cnt = datalen / typelen;

    } else if (class == NC_OPAQUE && xsize > 0) {
      cnt = xlength(data) / xsize;
    } else {
      cnt = xlength(data);
    }
  } else if (xtype == NC_CHAR && isString (data)) {
    cnt = strlen (R_nc_strarg (data));
  } else {
    cnt = xlength (data);
  }

  /* -- Write attribute to file -----------------------------------------------*/
  if (cnt > 0) {
    buf = R_nc_r2c (data, ncid, xtype, 1, &cnt, NULL, NULL, NULL);
    R_nc_check (nc_put_att (ncid, varid, attname, xtype, cnt, buf));
  }

  RRETURN (R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_att()
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_att (SEXP nc, SEXP var, SEXP att, SEXP newname)
{
  int ncid, varid;
  const char *attname, *newattname;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  attname = R_nc_strarg (att);

  newattname = R_nc_strarg (newname);

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the attribute ---------------------------------------------------*/
  R_nc_check (nc_rename_att (ncid, varid, attname, newattname));

  RRETURN(R_NilValue);
}


