/*=============================================================================*\
 *									       *
 *  Name:       attribute.c						       *
 *									       *
 *  Version:    2.0-1							       *
 *									       *
 *  Purpose:    NetCDF attribute functions for RNetCDF              	       *
 *									       *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)			       *
 *              Milton Woods (miltonjwoods@gmail.com)                              *
 *									       *
 *  Copyright:  (C) 2004-2017 Pavel Michna                                     *
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
#include "attribute.h"


/* Convert attribute identifier from R string or number to a C string.
   Argument attname must have space for NC_MAX_NAME+1 characters.
   Result is a netcdf status value.
 */
static int
R_nc_att_name (SEXP att, int ncid, int varid, char *attname)
{
  if (isNumeric (att)) {
    return nc_inq_attname (ncid, varid, asInteger (att), attname);
  } else if (isString (att)) {
    strcpy (attname, CHAR (STRING_ELT (att, 0)));
    return NC_NOERR;
  } else {
    return NC_EINVAL;
  }
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_copy_att()                                                            *
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
 *  R_nc_delete_att()                                                          *
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
 *  Private functions used by R_nc_get_att()                                   *
\*-----------------------------------------------------------------------------*/


/* Read NC_CHAR attribute as R raw */
static SEXP
R_nc_get_att_raw (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  result = R_nc_protect (allocVector (RAWSXP, cnt));
  if (cnt > 0) {
    R_nc_check (nc_get_att_text (ncid, varid, attname,
                                 (char *) RAW (result)));
  }
  return result;
}

/* Read NC_CHAR attribute as a single R string */
static SEXP
R_nc_get_att_char (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  char *charbuf;
  result = R_nc_protect (allocVector (STRSXP, 1));
  if (cnt > 0) {
    charbuf = R_alloc (cnt + 1, sizeof (char));
    R_nc_check (nc_get_att_text (ncid, varid, attname, charbuf));
    R_nc_char_strsxp (charbuf, result, cnt, 0, 1);
  }
  return result;
}

/* Read NC_STRING attribute as a vector of R strings */
static SEXP
R_nc_get_att_string (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  char **strbuf;
  result = R_nc_protect (allocVector (STRSXP, cnt));
  if (cnt > 0) {
    strbuf = (void *) R_alloc (cnt, sizeof(char *));
    R_nc_check (nc_get_att_string (ncid, varid, attname, strbuf));
    R_nc_str_strsxp (strbuf, result, 0, cnt);
    R_nc_check (nc_free_string (cnt, strbuf));
  }
  return result;
}

/* Read NC_INT64 attribute as R strings */
static SEXP
R_nc_get_att_int64 (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result=NULL;
  long long *int64buf;
  if (cnt > 0) {
    int64buf = (void *) R_alloc (cnt, sizeof (long long));
    R_nc_check (nc_get_att (ncid, varid, attname, int64buf));
    result = R_nc_c2r (int64buf, 0, cnt, NC_INT64, TRUE,
              NULL, NULL, NULL, NULL, NULL);
  }
  return result;
}

/* Read NC_UINT64 attribute as R strings */
static SEXP
R_nc_get_att_uint64 (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result=NULL;
  unsigned long long *uint64buf;
  if (cnt > 0) {
    uint64buf = (void *) R_alloc (cnt, sizeof (unsigned long long));
    R_nc_check (nc_get_att (ncid, varid, attname, uint64buf));
    result = R_nc_c2r (uint64buf, 0, cnt, NC_UINT64, TRUE,
              NULL, NULL, NULL, NULL, NULL);
  }
  return result;
}

/* Read numeric attribute as R integers */
static SEXP
R_nc_get_att_int (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  result = R_nc_protect (allocVector (INTSXP, cnt));
  if (cnt > 0) {
    R_nc_check (nc_get_att_int (ncid, varid, attname, INTEGER (result)));
  }
  return result;
}

/* Read numeric attribute as R double precision */
static SEXP
R_nc_get_att_double (int ncid, int varid, const char *attname, size_t cnt)
{
  SEXP result;
  result = R_nc_protect (allocVector (REALSXP, cnt));
  if (cnt > 0) {
    R_nc_check (nc_get_att_double (ncid, varid, attname, REAL (result)));
  }
  return result;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_att (SEXP nc, SEXP var, SEXP att, SEXP rawchar, SEXP fitnum)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];
  size_t cnt;
  nc_type xtype;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  R_nc_check (R_nc_att_name (att, ncid, varid, attname));

  /*-- Get the attribute's type and size --------------------------------------*/
  R_nc_check(nc_inq_att (ncid, varid, attname, &xtype, &cnt));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Allocate memory and read attribute from file ---------------------------*/
  switch (xtype) {
    case NC_CHAR:
      if (asLogical (rawchar) == TRUE) {
	result = R_nc_get_att_raw (ncid, varid, attname, cnt);
      } else {
	result = R_nc_get_att_char (ncid, varid, attname, cnt);
      }
      break;
    case NC_STRING:
      result = R_nc_get_att_string (ncid, varid, attname, cnt);
      break;
    case NC_BYTE:
    case NC_UBYTE:
    case NC_SHORT:
    case NC_USHORT:
    case NC_INT:
      if (asLogical (fitnum) == TRUE) {
	result = R_nc_get_att_int (ncid, varid, attname, cnt);
	break;
      }
    case NC_INT64:
      if (asLogical (fitnum) == TRUE) {
	result = R_nc_get_att_int64 (ncid, varid, attname, cnt);
	break;
      }
    case NC_UINT64:
      if (asLogical (fitnum) == TRUE) {
	result = R_nc_get_att_uint64 (ncid, varid, attname, cnt);
	break;
      }
    case NC_UINT:
    case NC_FLOAT:
    case NC_DOUBLE:
      result = R_nc_get_att_double (ncid, varid, attname, cnt);
      break;
    default:
      RERROR (RNC_ETYPEDROP);
  }

  RRETURN (result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_att()                                                             *
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
  R_nc_check (nc_inq_type (ncid, type, atttype, NULL));  

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
 *  R_nc_put_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_att (SEXP nc, SEXP var, SEXP att, SEXP type, SEXP data)
{
  int ncid, varid;
  size_t cnt;
  nc_type xtype;
  const char *attname, *charbuf, **strbuf;
  void *voidbuf;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  attname = CHAR (STRING_ELT (att, 0));

  /*-- Convert char to nc_type ------------------------------------------------*/
  R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &xtype));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Write attribute to file ------------------------------------------------*/
  switch (TYPEOF (data)) {
  case RAWSXP:
    if (xtype == NC_CHAR) {
      charbuf = (char *) RAW (data);
      cnt = xlength (data);
      R_nc_check (nc_put_att_text (ncid, varid, attname, cnt, charbuf));
      RRETURN (R_NilValue);
    }
    break;
  case STRSXP:
    switch (xtype) {
    case NC_CHAR:
      /* Only write a single string */
      charbuf = CHAR (STRING_ELT (data, 0));
      cnt = strlen (charbuf);
      R_nc_check (nc_put_att_text (ncid, varid, attname, cnt, charbuf));
      RRETURN (R_NilValue);
    case NC_STRING:
      cnt = xlength (data);
      strbuf = (void *) R_alloc (cnt, sizeof(char *));
      R_nc_strsxp_str (data, strbuf, 0, cnt);
      R_nc_check (nc_put_att_string (ncid, varid, attname, cnt, strbuf));
      RRETURN (R_NilValue);
    case NC_INT64:
      cnt = xlength (data);
      {  
        long long fill = NC_FILL_INT64;
        voidbuf = R_nc_r2c (data, NULL, 0, cnt, NC_INT64, &fill, NULL, NULL);
      }
      R_nc_check (nc_put_att (ncid, varid, attname,
                              xtype, cnt, voidbuf));
      RRETURN (R_NilValue);
    case NC_UINT64:
      cnt = xlength (data);
      {
        unsigned long long fill = NC_FILL_UINT64;
        voidbuf = R_nc_r2c (data, NULL, 0, cnt, NC_UINT64, &fill, NULL, NULL);
      }
      R_nc_check (nc_put_att (ncid, varid, attname,
                              xtype, cnt, voidbuf));
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
      cnt = xlength (data);
      R_nc_check (nc_put_att_int (ncid, varid, attname, xtype, cnt, INTEGER (data)));
      RRETURN (R_NilValue);
    case REALSXP:
      cnt = xlength (data);
      R_nc_check (nc_put_att_double (ncid, varid, attname, xtype, cnt, REAL (data)));
      RRETURN (R_NilValue);
    }
    break;
  }

  /* If this point is reached, input and external types were not compatible */
  RERROR (RNC_EDATATYPE);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_att (SEXP nc, SEXP var, SEXP att, SEXP newname)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];
  const char *newnamep;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    R_nc_check (R_nc_var_id (var, ncid, &varid));
  }

  R_nc_check (R_nc_att_name (att, ncid, varid, attname));

  newnamep = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the attribute ---------------------------------------------------*/
  R_nc_check (nc_rename_att (ncid, varid, attname, newnamep));

  RRETURN(R_NilValue);
}


