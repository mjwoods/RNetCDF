/*=============================================================================*\
 *									       *
 *  Name:       RNetCDF.c						       *
 *									       *
 *  Version:    2.0-1							       *
 *									       *
 *  Purpose:    NetCDF interface for R.					       *
 *									       *
 *  Author:     Pavel Michna (michna@giub.unibe.ch)			       *
 *              Milton Woods (m.woods@bom.gov.au)                              *
 *									       *
 *  Copyright:  (C) 2004-2016 Pavel Michna                                     *
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
 *  Author   Date       Description					       *
 *  ------   ----       -----------					       *
 *  pm       12/06/04   First implementation				       *
 *  pm       09/07/04   Support scalar variables   		               *
 *  pm       21/07/04   Changed error handling	                               *
 *  pm       03/01/05   Corrected minor bugs	                               *
 *  pm       25/07/06   Changed SET_STRING_ELT to SET_VECTOR_ELT               *
 *  mw       14/04/08   Changed nc_redef and nc_enddef usage                   *
 *                      to avoid unnecessary data movement within a file       *
 *  mw       14/04/08   Added new modes (large, prefill, share) to             *
 *                      functions nc_open and nc_create                        *
 *  pm       24/11/10   Restored nc_redef and nc_enddef usage and added        *
 *                      enddef option for having the same effect               *
 *  pm       01/12/10   Removed argument SEXP enddef, checking for NC_DEFINE   *
 *  pm       03/12/10   Minor bug corrections at possible memory leaks         *
 *  pm       15/12/10   Minor bug corrections                                  *
 *  pm       25/12/10   Added UDUNITS-2 message override handling (R_ut_init)  *
 *  pm       04/01/11   Corrected string handling in R_nc_get_vara_text        *
 *  pm       05/01/11   Removed extra zeroing after Calloc                     *
 *  pm       26/05/14   Corrected memory leak issue (lines 1338 and 1593)      *
 *  mw       05/09/14   Support reading and writing raw character arrays,      *
 *                      avoid temporary arrays when reading/writing variables  *
 *  mw       08/09/14   Handle reading and writing of zero-sized arrays        *
 *  mw       01/02/15   Remove redundant ut_read_xml from R_ut_init            *
 *  mw       24/04/15   Initialise and free utunit when using udunits2,        *
 *                      to fix memory errors reported by valgrind.             *
 *                      Allow udunits2 headers to be in udunits2 directory.    *
 *  mw       26/01/16   Fix memory leak from abnormal exit of calendar funcs.  *
 *  mw       24/02/16   Support creation of files in netcdf4 (hdf5) format.    *
 *  mw       21/05/16   Add functions for netcdf4 groups.                      *
 *									       *
\*=============================================================================*/


/*=============================================================================*\
 *  Includes								       *
\*=============================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <netcdf.h>

#ifdef HAVE_UDUNITS2_UDUNITS_H
#include <udunits2/udunits.h>
#else
#include <udunits.h>
#endif

#include <R.h>
#include <Rinternals.h>

/*=============================================================================*\
 *  Local macros, constants and variables
\*=============================================================================*/

#define NA_SIZE ((size_t) -1)

#define RRETURN(object) { R_nc_unprotect (); return (object); }

static int R_nc_protect_count = 0;

static const char RNC_EDATALEN[]="Not enough data", \
  RNC_EDATATYPE[]="Incompatible data for external type", \
  RNC_ETYPEDROP[]="Unsupported external type";

/*=============================================================================*\
 *  Reusable internal functions
\*=============================================================================*/

/* Protect and unprotect objects from garbage collection by R */
static SEXP
R_nc_protect (SEXP obj)
{
  PROTECT(obj);
  R_nc_protect_count++;
  return obj;
}

static void
R_nc_unprotect (void) {
  if (R_nc_protect_count > 0) {
    UNPROTECT (R_nc_protect_count);
    R_nc_protect_count = 0;
  }
}

/* Enter netcdf define mode if possible.
   Returns netcdf error code if an unhandled error occurs.
 */
static int
R_nc_redef (int ncid)
{
  int status;
  status = nc_redef(ncid);
  if (status == NC_EINDEFINE) {
    status = NC_NOERR;
  }
  return status;
}


/* Enter netcdf data mode if possible.
   Returns netcdf error code if an unhandled error occurs.
 */
static int
R_nc_enddef (int ncid)
{
  int status;
  status = nc_enddef(ncid);
  if (status == NC_ENOTINDEFINE) {
    status = NC_NOERR;
  }
  return status;
}


/* Convert netcdf type code to string label.
 */
static const char *
R_nc_type2str (int ncid, nc_type xtype)
{
  char *str;

  switch (xtype) {
  case NC_BYTE:
    return "NC_BYTE";
  case NC_UBYTE:
    return "NC_UBYTE";
  case NC_CHAR:
    return "NC_CHAR";
  case NC_SHORT:
    return "NC_SHORT";
  case NC_USHORT:
    return "NC_USHORT";
  case NC_INT:
    return "NC_INT";
  case NC_UINT:
    return "NC_UINT";
  case NC_INT64:
    return "NC_INT64";
  case NC_UINT64:
    return "NC_UINT64";
  case NC_FLOAT:
    return "NC_FLOAT";
  case NC_DOUBLE:
    return "NC_DOUBLE";
  case NC_STRING:
    return "NC_STRING";
  default:
    /* Try to get name of a user defined type */
    str = R_alloc (NC_MAX_NAME + 1, sizeof (char));
    if (nc_inq_user_type (ncid, xtype, str, NULL, NULL, NULL, NULL) ==
        NC_NOERR) {
      return str;
    } else {
      return "UNKNOWN";
    };
  }
}


/* Convert netcdf string label to type code.
   Return NC_NOERR if ok, netcdf error code otherwise.
 */
static int
R_nc_str2type (int ncid, const char *str, nc_type * xtype)
{
  size_t typelen;
  typelen = strlen (str);
  *xtype = NC_NAT;
  if (typelen >= 6) {
    switch (str[3]) {
    case 'B':
      if (strcmp (str, "NC_BYTE") == 0) {
        *xtype = NC_BYTE;
      }
      break;
    case 'C':
      if (strcmp (str, "NC_CHAR") == 0) {
        *xtype = NC_CHAR;
      }
      break;
    case 'D':
      if (strcmp (str, "NC_DOUBLE") == 0) {
        *xtype = NC_DOUBLE;
      }
      break;
    case 'F':
      if (strcmp (str, "NC_FLOAT") == 0) {
        *xtype = NC_FLOAT;
      }
      break;
    case 'I':
      switch (str[6]) {
      case '\0':
        if (strcmp (str, "NC_INT") == 0) {
          *xtype = NC_INT;
        }
        break;
      case '6':
        if (strcmp (str, "NC_INT64") == 0) {
          *xtype = NC_INT64;
        }
        break;
      }
      break;
    case 'L':
      if (strcmp (str, "NC_LONG") == 0) {
        *xtype = NC_LONG;
      }
      break;
    case 'S':
      switch (str[4]) {
      case 'H':
        if (strcmp (str, "NC_SHORT") == 0) {
          *xtype = NC_SHORT;
        }
        break;
      case 'T':
        if (strcmp (str, "NC_STRING") == 0) {
          *xtype = NC_STRING;
        }
        break;
      }
      break;
    case 'U':
      if (typelen >= 7) {
        switch (str[7]) {
        case '\0':
          if (strcmp (str, "NC_UINT") == 0) {
            *xtype = NC_UINT;
          }
          break;
        case '6':
          if (strcmp (str, "NC_UINT64") == 0) {
            *xtype = NC_UINT64;
          }
          break;
        case 'E':
          if (strcmp (str, "NC_UBYTE") == 0) {
            *xtype = NC_UBYTE;
          }
          break;
        case 'R':
          if (strcmp (str, "NC_USHORT") == 0) {
            *xtype = NC_USHORT;
          }
          break;
        }
      }
      break;
    }
  }

  if (*xtype == NC_NAT) {
    /* Try to get id of a user defined type */
    return nc_inq_typeid (ncid, str, xtype);
  } else {
    return NC_NOERR;
  }
}


/* Convert udunits error code to a string */
static const char *
R_nc_uterror (int errcode)
{
  switch (errcode) {
  case UT_EOF:
    return "end-of-file encountered (udunits)";
  case UT_ENOFILE:
    return "no units-file (udunits)";
  case UT_ESYNTAX:
    return "syntax error (udunits)";
  case UT_EUNKNOWN:
    return "unknown specification (udunits)";
  case UT_EIO:
    return "I/O error (udunits)";
  case UT_EINVALID:
    return "invalid unit-structure (udunits)";
  case UT_ENOINIT:
    return "package not initialized (udunits)";
  case UT_ECONVERT:
    return "two units are not convertable (udunits)";
  case UT_EALLOC:
    return "memory allocation failure (udunits)";
  case UT_ENOROOM:
    return "insufficient room supplied (udunits)";
  case UT_ENOTTIME:
    return "not a unit of time (udunits)";
  default:
    return "unknown error (udunits)";
  }
}


/* Convert netcdf file format code to string label.
 */
static const char *
R_nc_format2str (int format)
{
  switch (format) {
  case NC_FORMAT_CLASSIC:
    return "classic";
#ifdef NC_FORMAT_64BIT
  case NC_FORMAT_64BIT:
#elif defined NC_FORMAT_64BIT_OFFSET
  case NC_FORMAT_64BIT_OFFSET:
#endif
    return "offset64";
#ifdef NC_FORMAT_CDF5
  case NC_FORMAT_CDF5:
    return "cdf5";
#endif
  case NC_FORMAT_NETCDF4:
    return "netcdf4";
  case NC_FORMAT_NETCDF4_CLASSIC:
    return "classic4";
  default:
    return "unknown";
  }
}


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


/* Convert dimension identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
static int
R_nc_dim_id (SEXP dim, int ncid, int *dimid, int idx)
{
  if (isInteger (dim)) {
    *dimid = INTEGER (dim)[idx];
    return NC_NOERR;
  } else if (isReal (dim)) {
    *dimid = REAL (dim)[idx];
    return NC_NOERR;
  } else if (isString (dim)) {
    return nc_inq_dimid (ncid, CHAR (STRING_ELT (dim, idx)), dimid);
  } else {
    return NC_EINVAL;
  }
}


/* Convert variable identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
static int
R_nc_var_id (SEXP var, int ncid, int *varid)
{
  if (isNumeric (var)) {
    *varid = asInteger (var);
    return NC_NOERR;
  } else if (isString (var)) {
    return nc_inq_varid (ncid, CHAR (STRING_ELT (var, 0)), varid);
  } else {
    return NC_EINVAL;
  }
}


/* Determine if a C string matches the first element of an R variable.
   Result is a logical value. */
static int
R_nc_strcmp (SEXP var, const char *str)
{
  return (isString(var) &&
          xlength(var) >= 1 &&
          strcmp(CHAR (STRING_ELT (var, 0)), str) == 0);
}


/* Raise an error in R */
static void
R_nc_error(const char *msg)
{
  R_nc_unprotect ();
  error ("%s", msg);
}

/* If status is a netcdf error, raise an R error with a suitable message,
   otherwise return to caller. */
static int
R_nc_check(int status)
{
  if (status != NC_NOERR) {
    R_nc_error (nc_strerror (status));
  }
  return status;
}


/* Copy R vector rv to C vector cv, converting type to size_t and reversing order.
   The length of cv is specified by nc.
   Elements beyond the length of rv and non-finite values are stored as fillval.
 */
static void
R_nc_size_r2c(SEXP rv, size_t nc, size_t fillval, size_t *cv)
{
  double *realp;
  int *intp;
  size_t nr, ii;

  nr = xlength (rv);
  nr = (nr < nc) ? nr : nc;

  /* Copy elements */
  if (isReal (rv)) {
    realp = REAL (rv);
    for ( ii=0; ii<nr; ii++ ) {
      if (R_FINITE (realp[ii])) {
        cv[nc-1-ii] = realp[ii];
      } else {
        cv[nc-1-ii] = fillval;
      }
    }
  } else if (isInteger (rv)) {
    intp = INTEGER (rv);
    for ( ii=0; ii<nr; ii++ ) {
      if (intp[ii] == NA_INTEGER) {
        cv[nc-1-ii] = fillval;
      } else {
        cv[nc-1-ii] = intp[ii];
      }
    }
  } else {
    nr = 0;
  }    

  /* Fill remaining elements */
  for ( ii=nr; ii<nc; ii++ ) {
    cv[nc-1-ii] = fillval;
  }

}


/* Handle NA values in user-specified variable slices.
   Store slice ranges in cstart and ccount vectors with C dimension order.
   The number of dimensions is returned in ndims,
   and both C vectors are allocated (via R_alloc) to length ndims.
   Result is a netcdf status value.
 */
static int
R_nc_slice (SEXP start, SEXP count, int ncid, int varid,
            int *ndims, size_t **cstart, size_t **ccount)
{
  int ii, status, *dimids;
  size_t clen;

  /* Get dimension identifiers of the variable */
  status = nc_inq_var (ncid, varid, NULL, NULL, ndims, NULL, NULL);
  if (status != NC_NOERR) {
    return(status);
  }

  dimids = (void *) R_alloc (*ndims, sizeof (int));
  *cstart = (void *) R_alloc (*ndims, sizeof (int));
  *ccount = (void *) R_alloc (*ndims, sizeof (int));

  status = nc_inq_vardimid (ncid, varid, dimids);
  if (status != NC_NOERR) {
    return(status);
  }

  /* Store start in C dimension order as size_t,
     converting missing values to 1 */
  R_nc_size_r2c(start, *ndims, 1, *cstart);

  /* Convert Fortran indices (1-based) to C (0-based) */
  for (ii=0; ii<*ndims; ii++) {
    (*cstart)[ii] -= 1;
  }
  
  /* Store count in C dimension order as size_t,
     handling missing values so that corresponding dimensions are
     read/written from specified start index to the highest index.
   */
  R_nc_size_r2c(count, *ndims, NA_SIZE, *ccount);
  for ( ii=0; ii<*ndims; ii++ ) {
    if ((*ccount)[ii] == NA_SIZE) {
      status = nc_inq_dimlen (ncid, dimids[ii], &clen);
      if (status != NC_NOERR) {
        return(status);
      }
      (*ccount)[ii] = clen - (*cstart)[ii];
    }
  }

  return(NC_NOERR);
}


/* Find total number of elements in an array from dimension lengths.
   Result is 1 for a scalar or product of dimensions for an array. */
static size_t
R_nc_length (int ndims, const size_t *count)
{
  int ii;
  size_t length;

  length = 1;
  for ( ii=0; ii<ndims; ii++ ) {
    length *= count[ii]; 
  }
  return (length);
}


/*=============================================================================*\
 *  NetCDF library functions						       *
\*=============================================================================*/

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
 *  R_nc_get_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_att (SEXP nc, SEXP var, SEXP att, SEXP rawchar)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];
  size_t cnt, ii;
  nc_type xtype;
  char *charbuf, **strbuf;
  SEXP result=R_NilValue;

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
    if (asLogical (rawchar)) {
      result = R_nc_protect (allocVector (RAWSXP, cnt));
      if (cnt > 0) {
        R_nc_check (nc_get_att_text (ncid, varid, attname,
                                    (char *) RAW (result)));
      }
    } else {
      result = R_nc_protect (allocVector (STRSXP, 1));
      if (cnt > 0) {
        /* Read characters as a single string */
        charbuf = R_alloc (cnt + 1, sizeof (char));
        R_nc_check (nc_get_att_text (ncid, varid, attname, charbuf));
        charbuf[cnt] = '\0';
        SET_STRING_ELT (result, 0, mkChar (charbuf));
      }
    }
    break;
  case NC_STRING:
    result = R_nc_protect (allocVector (STRSXP, cnt));
    if (cnt > 0) {
      strbuf = (void *) R_alloc (cnt, sizeof(char *));
      R_nc_check (nc_get_att_string (ncid, varid, attname, strbuf));
      for (ii=0; ii<cnt; ii++) {
        SET_STRING_ELT (result, ii, mkChar (strbuf[ii]));
      }
      R_nc_check (nc_free_string (cnt, strbuf));
    }
    break;
  case NC_BYTE:
  case NC_SHORT:
  case NC_INT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_UBYTE:
  case NC_USHORT:
  case NC_UINT:
  case NC_INT64:
  case NC_UINT64:
    result = R_nc_protect (allocVector (REALSXP, cnt));
    if (cnt > 0) {
      R_nc_check (nc_get_att_double (ncid, varid, attname, REAL (result)));
    }
    break;
  default:
    R_nc_error (RNC_ETYPEDROP);
  }

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_att (SEXP nc, SEXP var, SEXP att)
{
  int ncid, varid, attid;
  char attname[NC_MAX_NAME+1];
  const char *atttype;
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
  atttype = R_nc_type2str (ncid, type);

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
  size_t cnt, ii;
  nc_type xtype;
  const char *attname, *charbuf, **strbuf;

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
  switch (xtype) {
  case NC_CHAR:
    if (TYPEOF (data) == RAWSXP) {
      charbuf = (char *) RAW (data);
      cnt = xlength (data);
      R_nc_check (nc_put_att_text (ncid, varid, attname, cnt, charbuf));
    } else if (isString (data)) {
      /* Only write a single string */
      charbuf = CHAR (STRING_ELT (data, 0));
      cnt = strlen (charbuf);
      R_nc_check (nc_put_att_text (ncid, varid, attname, cnt, charbuf));
    } else {
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  case NC_STRING:
    if (isString (data)) {
      cnt = xlength (data);
      strbuf = (void *) R_alloc (cnt, sizeof(char *));
      for (ii=0; ii<cnt; ii++) {
	strbuf[ii] = CHAR( STRING_ELT (data, ii));
      }
      R_nc_check (nc_put_att_string (ncid, varid, attname, cnt, strbuf));
    } else {
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  case NC_BYTE:
  case NC_SHORT:
  case NC_INT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_UBYTE:
  case NC_USHORT:
  case NC_UINT:
  case NC_INT64:
  case NC_UINT64:
    if (isReal (data)) {
      cnt = xlength (data);
      R_nc_check (nc_put_att_double (ncid, varid, attname, xtype, cnt, REAL (data)));
    } else if (isInteger (data) || isLogical (data)) {
      cnt = xlength (data);
      R_nc_check (nc_put_att_int (ncid, varid, attname, xtype, cnt, INTEGER (data)));
    } else {
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  default:
    R_nc_error (RNC_ETYPEDROP);
  }

  RRETURN(R_NilValue);
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


/*-----------------------------------------------------------------------------*\
 *  R_nc_close()                                                               *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_close (SEXP ptr)
{
  int *fileid;

  fileid = R_ExternalPtrAddr (ptr);
  if (!fileid) {
    RRETURN(R_NilValue);
  }

  R_nc_check (nc_close (*fileid));
  R_Free (fileid);
  R_ClearExternalPtr (ptr);

  RRETURN(R_NilValue);
}

/* Private function used as finalizer during garbage collection.
   It is required to have no return value. */
static void
R_nc_finalizer (SEXP ptr)
{
  R_nc_close (ptr);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_create()                                                              *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_create (SEXP filename, SEXP clobber, SEXP share, SEXP prefill,
             SEXP format)
{
  int cmode, fillmode, old_fillmode, ncid, *fileid;
  SEXP Rptr, result;

  /*-- Determine the cmode ----------------------------------------------------*/
  if (asLogical(clobber)) {
    cmode = NC_CLOBBER;
  } else {
    cmode = NC_NOCLOBBER;
  }

  /*-- Determine which buffer scheme shall be used ----------------------------*/
  if (asLogical(share)) {
    cmode = cmode | NC_SHARE;
  }

  /*-- Determine the fillmode -------------------------------------------------*/
  if (asLogical(prefill)) {
    fillmode = NC_FILL;
  } else {
    fillmode = NC_NOFILL;
  }

  /*-- Set file format (default is netcdf classic) ----------------------------*/
  if (R_nc_strcmp(format, "netcdf4")) {
    cmode = cmode | NC_NETCDF4;
  } else if (R_nc_strcmp(format, "classic4")) {
    cmode = cmode | NC_NETCDF4 | NC_CLASSIC_MODEL;
  } else if (R_nc_strcmp(format, "offset64")) {
    cmode = cmode | NC_64BIT_OFFSET;
  }

  /*-- Create the file --------------------------------------------------------*/
  R_nc_check (nc_create (R_ExpandFileName (CHAR (STRING_ELT (filename, 0))),
                       cmode, &ncid));
  result = R_nc_protect (ScalarInteger (ncid));

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = R_nc_protect (R_MakeExternalPtr (fileid, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (result, install ("handle_ptr"), Rptr);

  /*-- Set the fill mode ------------------------------------------------------*/
  R_nc_check (nc_set_fill (ncid, fillmode, &old_fillmode));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_dim (SEXP nc, SEXP dimname, SEXP size, SEXP unlim)
{
  int ncid, dimid;
  const char *dimnamep;
  size_t nccnt;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  dimnamep = CHAR (STRING_ELT (dimname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Create the dimension ---------------------------------------------------*/
  if (asLogical(unlim)) {
    nccnt = NC_UNLIMITED;
  } else {
    /* Allow size to be a double, which can be larger than integer */
    if (isInteger(size)) {
      nccnt = asInteger(size);
    } else {
      nccnt = asReal(size);
    }
  }

  R_nc_check (nc_def_dim (ncid, dimnamep, nccnt, &dimid));

  result = R_nc_protect (ScalarInteger (dimid));
  RRETURN(result);
}


/* Private function to find unlimited dimensions of a file or group.
   Returns netcdf status. If no error occurs, nunlim and unlimids are set.
   Note - some netcdf4 versions only return unlimited dimensions defined in a group,
     not those defined in the group and its ancestors as claimed in documentation.
 */
static int
R_nc_unlimdims (int ncid, int *nunlim, int **unlimids)
{
  int status, format;

  *nunlim = 0;

  status = nc_inq_format (ncid, &format);
  if (status != NC_NOERR) {
    return status;
  }

  if (format == NC_FORMAT_NETCDF4) {
    status = nc_inq_unlimdims (ncid, nunlim, NULL);
    if (status != NC_NOERR) {
      return status;
    }

    *unlimids = (void *) (R_alloc (*nunlim, sizeof (int)));

    status = nc_inq_unlimdims (ncid, NULL, *unlimids);

  } else {
    *unlimids = (void *) (R_alloc (1, sizeof (int)));
    status = nc_inq_unlimdim (ncid, *unlimids);
    if (status == NC_NOERR && **unlimids != -1) {
      *nunlim = 1;
    }
  }

  return status;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dim (SEXP nc, SEXP dim)
{
  int ncid, nunlim, *unlimids, isunlim, dimid, ii;
  size_t dimlen;
  char dimname[NC_MAX_NAME + 1];
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_dim_id (dim, ncid, &dimid, 0));

  /*-- Inquire the dimension --------------------------------------------------*/
  R_nc_check (nc_inq_dim (ncid, dimid, dimname, &dimlen));

  /*-- Check if it is an unlimited dimension ----------------------------------*/
  R_nc_check (R_nc_unlimdims (ncid, &nunlim, &unlimids));

  isunlim = 0;
  for (ii = 0; ii < nunlim; ii++) {
    if (unlimids[ii] == dimid) {
      isunlim = 1;
      break;
    }
  }

  /*-- Returning the list -----------------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 4));
  SET_VECTOR_ELT (result, 0, ScalarInteger (dimid));
  SET_VECTOR_ELT (result, 1, mkString (dimname));
  /* Dimension length may be larger than integer, so return as double */
  SET_VECTOR_ELT (result, 2, ScalarReal (dimlen));
  SET_VECTOR_ELT (result, 3, ScalarLogical (isunlim));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_dim()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_dim (SEXP nc, SEXP dim, SEXP newname)
{
  int ncid, dimid;
  const char *newnamep;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_dim_id (dim, ncid, &dimid, 0));

  newnamep = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the dimension ---------------------------------------------------*/
  R_nc_check (nc_rename_dim (ncid, dimid, newnamep));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_file (SEXP nc)
{
  int ncid, ndims, nvars, ngatts, unlimdimid, format;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  /*-- Inquire about the NetCDF dataset ---------------------------------------*/
  R_nc_check (nc_inq (ncid, &ndims, &nvars, &ngatts, &unlimdimid));
  if (unlimdimid == -1 ) {
    unlimdimid = NA_INTEGER;
  }

  /*-- Inquire about the NetCDF format ----------------------------------------*/
  R_nc_check (nc_inq_format (ncid, &format));

  /*-- Returning the list -----------------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 5)); 
  SET_VECTOR_ELT (result, 0, ScalarInteger (ndims));
  SET_VECTOR_ELT (result, 1, ScalarInteger (nvars));
  SET_VECTOR_ELT (result, 2, ScalarInteger (ngatts));
  SET_VECTOR_ELT (result, 3, ScalarInteger (unlimdimid));
  SET_VECTOR_ELT (result, 4, mkString (R_nc_format2str (format)));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_open()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill)
{
  int ncid, omode, fillmode, old_fillmode, *fileid;
  SEXP Rptr, result;

  /*-- Determine the omode ----------------------------------------------------*/
  if (asLogical(write)) {
    omode = NC_WRITE;
  } else {
    omode = NC_NOWRITE;
  }

  if (asLogical(share)) {
    omode = omode | NC_SHARE;
  }

  /*-- Determine the fillmode -------------------------------------------------*/
  if (asLogical(prefill)) {
    fillmode = NC_FILL;
  } else {
    fillmode = NC_NOFILL;
  }

  /*-- Open the file ----------------------------------------------------------*/
  R_nc_check (nc_open (R_ExpandFileName (CHAR (STRING_ELT (filename, 0))),
                     omode, &ncid));
  result = R_nc_protect (ScalarInteger (ncid));

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = R_nc_protect (R_MakeExternalPtr (fileid, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (result, install ("handle_ptr"), Rptr);

  /*-- Set the fill mode ------------------------------------------------------*/
  if (asLogical(write)) {
    R_nc_check (nc_set_fill (ncid, fillmode, &old_fillmode));
  }

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_sync()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_sync (SEXP nc)
{
  int ncid;

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  ncid = asInteger(nc);
  R_nc_check( R_nc_enddef (ncid));

  /*-- Sync the file ----------------------------------------------------------*/
  R_nc_check (nc_sync (ncid));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_var (SEXP nc, SEXP varname, SEXP type, SEXP dims)
{
  int ncid, ii, *dimids, ndims, varid;
  nc_type xtype;
  const char *varnamep;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  varnamep = CHAR (STRING_ELT (varname, 0));

  R_nc_check (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &xtype));

  ndims = length(dims);
  dimids = (void *) R_alloc (ndims, sizeof(int));

  for (ii=0; ii<ndims; ii++) {
    /* Handle dimension names and convert from R to C storage order */
    R_nc_check (R_nc_dim_id (dims, ncid, &dimids[ndims-1-ii], ii));
  }

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Define the variable ----------------------------------------------------*/
  R_nc_check (nc_def_var (
            ncid, varnamep, xtype, ndims, dimids, &varid));

  result = R_nc_protect (ScalarInteger (varid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP rawchar)
{
  int ncid, varid, ndims, rank, *intp;
  size_t ii, inext,  arrlen, strcnt, strlen;
  size_t *cstart, *ccount;
  nc_type xtype;
  char nextchar, *charbuf, **strbuf;
  SEXP rdim, result=R_NilValue;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Handle NA values in start & count and reverse dimension order ----------*/
  R_nc_check ( R_nc_slice (start, count, ncid, varid,
                           &ndims, &cstart, &ccount));

  /*-- Determine total number of elements in data array -----------------------*/
  arrlen = R_nc_length (ndims, ccount);
  rank = ndims;

  /*-- Determine type of external data ----------------------------------------*/
  R_nc_check (nc_inq_vartype ( ncid, varid, &xtype));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Allocate memory and read variable from file ----------------------------*/
  switch (xtype) {
  case NC_CHAR:
    if (asLogical (rawchar)) {
      result = R_nc_protect (allocVector (RAWSXP, arrlen));
      if (arrlen > 0) {
        R_nc_check (nc_get_vara_text (ncid, varid, cstart, ccount,
                                    (char *) RAW (result)));
      }
    } else {
      if (ndims > 0) {
        /* Form strings along the fastest varying dimension -------------------*/
        strlen = ccount[ndims-1];
        strcnt = R_nc_length (ndims-1, ccount);
        rank = ndims - 1;
      } else {
        /* Scalar character is a single string */
        strlen = 1;
        strcnt = 1;
        rank = 0;
      }
      result = R_nc_protect (allocVector (STRSXP, strcnt));
      if (arrlen > 0) {
        charbuf = R_alloc (arrlen+1, sizeof (char));
        R_nc_check (nc_get_vara_text (ncid, varid, cstart, ccount, charbuf));
        for (ii=0; ii<strcnt; ii++) {
          /* Rows of character array may not be null-terminated, so set
             first character of next row to null before passing each row to R. */
          inext = (ii+1)*strlen;
          nextchar = charbuf[inext];
          charbuf[inext] = '\0';
          SET_STRING_ELT (result, ii, mkChar(&charbuf[ii*strlen]));
          charbuf[inext] = nextchar;
        }
      }
    }
    break;
  case NC_STRING:
    result = R_nc_protect (allocVector (STRSXP, arrlen));
    if (arrlen > 0) {
      strbuf = (void *) R_alloc (arrlen, sizeof(char *));
      R_nc_check (nc_get_vara_string (ncid, varid, cstart, ccount, strbuf));
      for (ii=0; ii<arrlen; ii++) {
        SET_STRING_ELT (result, ii, mkChar (strbuf[ii]));
      }
      R_nc_check (nc_free_string (arrlen, strbuf));
    }
    break;
  case NC_BYTE:
  case NC_SHORT:
  case NC_INT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_UBYTE:
  case NC_USHORT:
  case NC_UINT:
  case NC_INT64:
  case NC_UINT64:
    result = R_nc_protect (allocVector (REALSXP, arrlen));
    if (arrlen > 0) {
      R_nc_check (nc_get_vara_double (ncid, varid,
                                    cstart, ccount, REAL (result)));
    }
    break;
  default:
    R_nc_error (RNC_ETYPEDROP);
  }

  /*-- Set dimension attribute for arrays -------------------------------------*/
  if (rank > 0) {
    rdim = R_nc_protect( allocVector (INTSXP, rank));
    intp = INTEGER (rdim);
    for ( ii=0; ii<rank; ii++ ) {
      intp[ii] = ccount[rank-1-ii];
    }
    setAttrib(result, R_DimSymbol, rdim);
  }

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_var (SEXP nc, SEXP var)
{
  int ii, ncid, varid, ndims, natts, *cdimids, *rdimids;
  const char *vartype;
  char varname[NC_MAX_NAME + 1];
  nc_type xtype;
  SEXP result;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Inquire the variable ---------------------------------------------------*/
  R_nc_check (nc_inq_var (ncid, varid, varname, &xtype, &ndims, NULL, &natts));

  if (ndims > 0) {
    cdimids = (void *) R_alloc (ndims, sizeof (int));
    R_nc_check (nc_inq_vardimid (ncid, varid, cdimids));
  }

  /*-- Convert nc_type to char ------------------------------------------------*/
  vartype = R_nc_type2str (ncid, xtype);

  /*-- Construct the output list ----------------------------------------------*/
  result = R_nc_protect (allocVector (VECSXP, 6));
  SET_VECTOR_ELT (result, 0, ScalarInteger (varid));
  SET_VECTOR_ELT (result, 1, mkString (varname));
  SET_VECTOR_ELT (result, 2, mkString (vartype));
  SET_VECTOR_ELT (result, 3, ScalarInteger (ndims));

  if (ndims > 0) {
    /* Return vector of dimension ids in R order */
    SET_VECTOR_ELT (result, 4, allocVector (INTSXP, ndims));
    rdimids = INTEGER (VECTOR_ELT (result, 4));
    for (ii=0; ii<ndims; ii++) {
      rdimids[ii] = cdimids[ndims-1-ii];
    }
  } else {
    /* Return single NA for scalars */
    SET_VECTOR_ELT (result, 4, allocVector (INTSXP, 1));
    INTEGER (VECTOR_ELT (result, 4))[0] = NA_INTEGER;
  }

  SET_VECTOR_ELT (result, 5, ScalarInteger (natts));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP data)
{
  int ncid, varid, ndims;
  size_t ii, *cstart, *ccount, arrlen, strcnt, strlen;
  nc_type xtype;
  char *charbuf;
  const char **strbuf;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  /*-- Handle NA values in start & count and reverse dimension order ----------*/
  R_nc_check ( R_nc_slice (start, count, ncid, varid,
                           &ndims, &cstart, &ccount));

  /*-- Find total number of elements in data array ----------------------------*/
  arrlen = R_nc_length (ndims, ccount);
  if (arrlen == 0) {
    /* Nothing to write, so return immediately */
    RRETURN(R_NilValue);
  }

  /*-- Determine type of external data ----------------------------------------*/
  R_nc_check (nc_inq_vartype ( ncid, varid, &xtype));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  R_nc_check (R_nc_enddef (ncid));

  /*-- Write variable to file -------------------------------------------------*/
  switch (xtype) {
  case NC_CHAR:
    if (TYPEOF (data) == RAWSXP) {
      if (xlength (data) >= arrlen) {
        R_nc_check (nc_put_vara_text (ncid, varid, cstart, ccount,
                                    (char *) RAW (data)));
      } else {
        R_nc_error (RNC_EDATALEN);
      }
    } else if (isString (data)) {
      if (ndims > 0) {
        /* Store strings along the fastest varying dimension ------------------*/
        strlen = ccount[ndims-1];
        strcnt = R_nc_length (ndims-1, ccount);
      } else {
        /* Scalar character is a single string */
        strlen = 1;
        strcnt = 1;
      }
      if (xlength (data) >= strcnt) {
        charbuf = R_alloc (strcnt*strlen, sizeof (char));
        for (ii=0; ii<strcnt; ii++) {
          /* Copy strings from R to buffer,
             trimming or padding with '\0' to length strlen */
	  strncpy(&charbuf[ii*strlen], CHAR( STRING_ELT (data, ii)), strlen);
        }
        R_nc_check (nc_put_vara_text (ncid, varid, cstart, ccount, charbuf));
      } else {
        R_nc_error (RNC_EDATALEN);
      }
    } else {
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  case NC_STRING:
    if (isString (data)) {
      if (xlength (data) >= arrlen) {
	strbuf = (void *) R_alloc (arrlen, sizeof(char *));
	for (ii=0; ii<arrlen; ii++) {
	  strbuf[ii] = CHAR( STRING_ELT( data, ii));
	}
	R_nc_check (nc_put_vara_string (ncid, varid, cstart, ccount, strbuf));
      } else {
        R_nc_error (RNC_EDATALEN);
      }
    } else {
      R_nc_error (RNC_EDATATYPE);
    }
    break;
  case NC_BYTE:
  case NC_SHORT:
  case NC_INT:
  case NC_FLOAT:
  case NC_DOUBLE:
  case NC_UBYTE:
  case NC_USHORT:
  case NC_UINT:
  case NC_INT64:
  case NC_UINT64:
    if (xlength (data) >= arrlen) {
      if (isReal (data)) {
	R_nc_check (nc_put_vara_double (ncid, varid, cstart, ccount, REAL (data)));
      } else if (isInteger (data) || isLogical (data)) {
	R_nc_check (nc_put_vara_int (ncid, varid, cstart, ccount, INTEGER (data)));
      } else {
        R_nc_error (RNC_EDATATYPE);
      }
    } else {
      R_nc_error (RNC_EDATALEN);
    }
    break;
  default:
    R_nc_error (RNC_ETYPEDROP);
  }

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_var()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_var (SEXP nc, SEXP var, SEXP newname)
{
  int ncid, varid;
  const char *cnewname;

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  R_nc_check (R_nc_var_id (var, ncid, &varid));

  cnewname = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  R_nc_check( R_nc_redef (ncid));

  /*-- Rename the variable ----------------------------------------------------*/
  R_nc_check (nc_rename_var (ncid, varid, cnewname));

  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_grp()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_grp (SEXP nc, SEXP grpname)
{
  int ncid, grpid;
  const char *cgrpname;
  SEXP result;

  /* Convert arguments to netcdf ids */
  ncid = asInteger (nc);

  cgrpname = CHAR (STRING_ELT (grpname, 0));

  /* Enter define mode */
  R_nc_check( R_nc_redef (ncid));

  /* Define the group */
  R_nc_check (nc_def_grp (ncid, cgrpname, &grpid));

  result = R_nc_protect (ScalarInteger (grpid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_parent()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_parent (SEXP nc)
{
  int ncid, grpid;
  SEXP result;

  /* Get parent group */
  ncid = asInteger (nc);
  R_nc_check (nc_inq_grp_parent (ncid, &grpid));

  result = R_nc_protect (ScalarInteger (grpid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_natts()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_natts (SEXP nc)
{
  int ncid, natts;
  SEXP result;

  /* Get number of attributes in group */
  ncid = asInteger (nc);
  R_nc_check (nc_inq_natts (ncid, &natts));

  result = R_nc_protect (ScalarInteger (natts));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grpname()                                                         *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grpname (SEXP nc, SEXP full)
{
  int ncid;
  size_t namelen;
  char *name, *fullname, namebuf[NC_MAX_NAME+1];
  SEXP result;

  ncid = asInteger (nc);

  if (asLogical (full)) {
    R_nc_check (nc_inq_grpname_full (ncid, &namelen, NULL));

    fullname = R_alloc (namelen + 1, sizeof (char));
    R_nc_check (nc_inq_grpname_full (ncid, NULL, fullname));
    name = fullname;
  } else {
    R_nc_check (nc_inq_grpname (ncid, namebuf));
    name = namebuf;
  }

  result = R_nc_protect (mkString (name));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_ncid()                                                        *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_ncid (SEXP nc, SEXP grpname, SEXP full)
{
  int ncid, grpid;
  const char *cgrpname;
  SEXP result;

  ncid = asInteger (nc);
  cgrpname = CHAR (STRING_ELT (grpname, 0));

  if (asLogical (full)) {
    R_nc_check (nc_inq_grp_full_ncid (ncid, cgrpname, &grpid));
  } else {
    R_nc_check (nc_inq_grp_ncid (ncid, cgrpname, &grpid));
  }

  result = R_nc_protect (ScalarInteger (grpid));
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  Get lists of ncids for components of a group                               *
\*-----------------------------------------------------------------------------*/

/* Template function returning a list of ncids for a group */
#define INQGRPIDS(RFUN, NCFUN) \
SEXP RFUN (SEXP nc) \
{ \
  int    ncid, count; \
  SEXP result; \
  ncid = asInteger (nc); \
  R_nc_check(NCFUN(ncid, &count, NULL)); \
  result = R_nc_protect (allocVector (INTSXP, count)); \
  R_nc_check(NCFUN(ncid, NULL, INTEGER(result))); \
  RRETURN(result); \
}

INQGRPIDS (R_nc_inq_grps, nc_inq_grps)
INQGRPIDS (R_nc_inq_typeids, nc_inq_typeids)
INQGRPIDS (R_nc_inq_varids, nc_inq_varids)


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dimids()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dimids (SEXP nc, SEXP ancestors)
{
  int ncid, full, count;
  SEXP result;

  ncid = asInteger (nc);
  full = asLogical (ancestors);

  R_nc_check (nc_inq_dimids (ncid, &count, NULL, full));
  result = R_nc_protect (allocVector (INTSXP, count));
  R_nc_check (nc_inq_dimids (ncid, NULL, INTEGER (result), full));

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_unlimids()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_unlimids (SEXP nc)
{
  int ncid, nunlim, *unlimids;
  SEXP result;

  ncid = asInteger (nc);

  R_nc_check (R_nc_unlimdims (ncid, &nunlim, &unlimids));

  result = R_nc_protect (allocVector (INTSXP, nunlim));

  /* Sort temporary results and copy to output structure */
  if (nunlim > 0) {
    R_isort(unlimids, nunlim);
    memcpy (INTEGER (result), unlimids, nunlim * sizeof (int));
  }

  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_grp()                                                          *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_rename_grp (SEXP nc, SEXP grpname)
{
  int ncid;
  const char *cgrpname;

#if defined HAVE_DECL_NC_RENAME_GRP && HAVE_DECL_NC_RENAME_GRP
  ncid = asInteger (nc);
  cgrpname = CHAR (STRING_ELT (grpname, 0));

  /* Enter define mode */
  R_nc_check( R_nc_redef (ncid));

  /* Rename the group */
  R_nc_check (nc_rename_grp (ncid, cgrpname));

  RRETURN(R_NilValue);

#else
  R_nc_error ("nc_rename_grp not supported by netcdf library");
#endif
}


/*=============================================================================*\
 *  Udunits library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_nc_calendar()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_calendar (SEXP unitstring, SEXP values)
{
  int year, month, day, hour, minute, status, isreal;
  float second;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  double dtmp, *dout;
  size_t ii, count;
  utUnit utunit;
  SEXP result;

  /* Handle arguments and initialise outputs */
  cstring = CHAR (STRING_ELT (unitstring, 0));
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values);

  result = R_nc_protect (allocMatrix (REALSXP, count, 6));
  dout = REAL (result);

  /*-- Scan unitstring --------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
  utIni (&utunit);
#endif

  status = utScan (cstring, &utunit);
  if (status != 0) {
    goto cleanup;
  }

  /*-- Check if unit is time and has origin -----------------------------------*/
  if (!utIsTime (&utunit)) {
    status = UT_ENOTTIME;
    goto cleanup;
  }

  if (!utHasOrigin (&utunit)) {
    status = UT_EINVALID;
    goto cleanup;
  }

  /*-- Convert values ---------------------------------------------------------*/
  for (ii = 0; ii < count; ii++) {
    if (isreal) {
      dtmp = dvals[ii];
    } else {
      dtmp = (ivals[ii] == NA_INTEGER) ? NA_REAL : ((double) ivals[ii]);
    } 
    if (R_FINITE (dtmp)) {
      status = utCalendar (dtmp, &utunit, &year, &month, &day,
                           &hour, &minute, &second);
      if (status != 0) {
	goto cleanup;
      }
      dout[ii] = year;
      dout[ii + count] = month;
      dout[ii + 2 * count] = day;
      dout[ii + 3 * count] = hour;
      dout[ii + 4 * count] = minute;
      dout[ii + 5 * count] = second;
    } else {
      dout[ii] = NA_REAL;
      dout[ii + count] = NA_REAL;
      dout[ii + 2 * count] = NA_REAL;
      dout[ii + 3 * count] = NA_REAL;
      dout[ii + 4 * count] = NA_REAL;
      dout[ii + 5 * count] = NA_REAL;
    }
  }

  /*-- Returning the array ----------------------------------------------------*/
cleanup:
#ifdef HAVE_LIBUDUNITS2
  utFree (&utunit);
#endif
  if (status != 0) {
    R_nc_error (R_nc_uterror (status));
  }
  RRETURN(result);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_utinit()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_utinit (SEXP path)
{
  int status;

  /*-- Avoid "overriding default" messages from UDUNITS-2 (1/2) ---------------*/
#ifdef HAVE_LIBUDUNITS2
  ut_set_error_message_handler (ut_ignore);
#endif

  /*-- Initialize udunits library ---------------------------------------------*/
  status = utInit (R_ExpandFileName (CHAR (STRING_ELT (path, 0))));

  /*-- Avoid "overriding default" messages from UDUNITS-2 (2/2) ---------------*/
#ifdef HAVE_LIBUDUNITS2
  ut_set_error_message_handler (ut_write_to_stderr);
#endif

  /*-- Returning the list -----------------------------------------------------*/
  if (status != 0) {
    R_nc_error (R_nc_uterror (status));
  }
  RRETURN(R_NilValue);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inv_calendar()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inv_calendar (SEXP unitstring, SEXP values)
{
  int status, itmp, isreal, isfinite;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  double datetime[6], *dout, dtmp;
  size_t ii, jj, count;
  utUnit utunit;
  SEXP result;

  /* Handle arguments and initialise outputs */
  cstring = CHAR (STRING_ELT (unitstring, 0));
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values) / 6;

  result = R_nc_protect (allocVector (REALSXP, count));
  dout = REAL (result);

  /*-- Scan unitstring --------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
  utIni (&utunit);
#endif

  status = utScan (cstring, &utunit);
  if (status != 0) {
    goto cleanup;
  }

  /*-- Check if unit is time and has origin -----------------------------------*/
  if (!utIsTime (&utunit)) {
    status = UT_ENOTTIME;
    goto cleanup;
  }

  if (!utHasOrigin (&utunit)) {
    status = UT_EINVALID;
    goto cleanup;
  }

  /*-- Convert values ---------------------------------------------------------*/
  for (ii = 0; ii < count; ii++) {
    isfinite = 1;
    if (isreal) {
      for (jj = 0; jj < 6; jj++) {
        dtmp = dvals[ii + jj*count];
        if (R_FINITE (dtmp)) {
          datetime[jj] = dtmp;
        } else {
          isfinite = 0;
          break;
        }
      }
    } else {
      for (jj = 0; jj < 6; jj++) {
        itmp = ivals[ii + jj*count];
        if (itmp == NA_INTEGER) {
          isfinite = 0;
          break;
        } else {
          datetime[jj] = itmp;
        }
      }
    }
    if (isfinite) {
      status = utInvCalendar (datetime[0], datetime[1], datetime[2],
                              datetime[3], datetime[4], datetime[5],
                              &utunit, &dout[ii]);
      if (status != 0) {
        goto cleanup;
      }
    } else {
      dout[ii] = NA_REAL;
    }
  }

  /*-- Returning the list -----------------------------------------------------*/
cleanup:
#ifdef HAVE_LIBUDUNITS2
  utFree (&utunit);
#endif
  if (status != 0) {
    R_nc_error (R_nc_uterror (status));
  }
  RRETURN(result);
}


/*=============================================================================*/

/*=============================================================================*\
 *  SCRATCH                                                                    *
\*=============================================================================*/
