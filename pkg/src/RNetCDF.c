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
 *  Local macro definitions                                                    *
\*=============================================================================*/

#define NA_SIZE ((size_t) -1)
#define NOSXP ((SEXPTYPE) -11111)
#define E_UNSUPPORTED -22222

#define RDATADEF(RTYPE,RLEN) \
  if (RTYPE != NOSXP) { \
    SET_VECTOR_ELT(retlist, 2, allocVector(RTYPE, RLEN)); \
  }

#define ROBJDEF(RTYPE,RLEN) \
  SEXP retlist; \
  PROTECT(retlist = allocVector(VECSXP, 3)); \
  RDATADEF(RTYPE,RLEN);

#define RDATASET VECTOR_ELT(retlist,2)

#define RNCRETURN(STATUS) \
  { R_nc_status(retlist, STATUS, 1); return(retlist); }

#define RNCCHECK(STATUS) \
  { if (R_nc_status(retlist, STATUS, 0)) return(retlist); }

#define RUTRETURN(STATUS) \
  { R_ut_status(retlist, STATUS, 1); return(retlist); }


/*=============================================================================*\
 *  Reusable internal functions
\*=============================================================================*/

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
  static char str[NC_MAX_NAME + 1];
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
    if (nc_inq_user_type (ncid, xtype, str, NULL, NULL, NULL, NULL) ==
        NC_NOERR) {
      return str;
    } else {
      return "UNKNOWN";
    };
  }
}


/* Convert netcdf string label to type code.
   Return NC_NOERR if ok, NC_EBADTYPE otherwise.
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
R_ut_strerror (int errcode)
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


/* Set return status and netcdf error message in list returned to R.
   The retlist is only modified if status indicates an error
   or if logical argument force is true (non-zero).
   The return value is true if retlist was modified, false otherwise.
 */
static int
R_nc_status(SEXP retlist, int status, int force)
{
  if (force || status != NC_NOERR) {
    SET_VECTOR_ELT(retlist, 0, ScalarInteger(status));
    if (status == E_UNSUPPORTED) {
      SET_VECTOR_ELT(retlist, 1, mkString(
                       "Operation requires RNetCDF built with newer netcdf library"));
    } else if (status != NC_NOERR) {
      SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
    }
    UNPROTECT(1);
    return 1;
  }
  return 0;
}


/* Set return status and udunits error message in list returned to R.
   The retlist is only modified if status indicates an error
   or if logical argument force is true (non-zero).
   The return value is true if retlist was modified, false otherwise.
 */
static int
R_ut_status(SEXP retlist, int status, int force)
{
  if (force || status != 0) {
    SET_VECTOR_ELT(retlist, 0, ScalarInteger(status));
    if (status != 0) {
      SET_VECTOR_ELT(retlist, 1, mkString(R_ut_strerror(status)));
    }
    UNPROTECT(1);
    return 1;
  }
  return 0;
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
   Both C vectors should have allocated length MAX_NC_DIMS,
   and the number of dimensions actually stored is returned in ndims.
   Result is a netcdf status value.
 */
static int
R_nc_slice (SEXP start, SEXP count, int ncid, int varid,
            int *ndims, size_t *cstart, size_t *ccount)
{
  int ii, status, dimids[MAX_NC_DIMS];
  size_t clen;

  /* Get dimension identifiers of the variable */
  status = nc_inq_var (ncid, varid, NULL, NULL, ndims, dimids, NULL);
  if (status != NC_NOERR) {
    return(status);
  }

  /* Store start in C dimension order as size_t,
     converting missing values to 1 */
  R_nc_size_r2c(start, *ndims, 1, cstart);

  /* Convert Fortran indices (1-based) to C (0-based) */
  for (ii=0; ii<*ndims; ii++) {
    cstart[ii] -= 1;
  }
  
  /* Store count in C dimension order as size_t,
     handling missing values so that corresponding dimensions are
     read/written from specified start index to the highest index.
   */
  R_nc_size_r2c(count, *ndims, NA_SIZE, ccount);
  for ( ii=0; ii<*ndims; ii++ ) {
    if (ccount[ii] == NA_SIZE) {
      status = nc_inq_dimlen (ncid, dimids[ii], &clen);
      if (status != NC_NOERR) {
        return(status);
      }
      ccount[ii] = clen - cstart[ii];
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
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid_in = asInteger (nc_in);
  ncid_out = asInteger (nc_out);

  if (R_nc_strcmp(var_in, "NC_GLOBAL")) {
    varid_in = NC_GLOBAL;
  } else {
    RNCCHECK (R_nc_var_id (var_in, ncid_in, &varid_in));
  }

  if (R_nc_strcmp(var_out, "NC_GLOBAL")) {
    varid_out = NC_GLOBAL;
  } else {
    RNCCHECK (R_nc_var_id (var_out, ncid_out, &varid_out));
  }

  RNCCHECK (R_nc_att_name (att, ncid_in, varid_in, attname));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid_out));

  /*-- Copy the attribute -----------------------------------------------------*/
  RNCCHECK (nc_copy_att (ncid_in, varid_in, attname,
                         ncid_out, varid_out));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_delete_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_delete_att (SEXP nc, SEXP var, SEXP att)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    RNCCHECK (R_nc_var_id (var, ncid, &varid));
  }

  RNCCHECK (R_nc_att_name (att, ncid, varid, attname));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid));

  /*-- Delete the attribute ---------------------------------------------------*/
  RNCCHECK (nc_del_att (ncid, varid, attname));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_att (SEXP nc, SEXP var, SEXP att)
{
  int ncid, varid;
  char attname[NC_MAX_NAME+1];
  char *cvalue;
  nc_type type;
  size_t cnt;
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    RNCCHECK (R_nc_var_id (var, ncid, &varid));
  }

  RNCCHECK (R_nc_att_name (att, ncid, varid, attname));

  /*-- Get the attribute's type and size --------------------------------------*/
  RNCCHECK(nc_inq_att (ncid, varid, attname, &type, &cnt));

  /*-- Get the attribute ------------------------------------------------------*/
  if (type==NC_CHAR) {
    RDATADEF (STRSXP, 1);
    cvalue = R_alloc (cnt + 1, sizeof (char));
    RNCCHECK (nc_get_att_text (ncid, varid, attname, cvalue));
    cvalue[cnt] = '\0';
    SET_STRING_ELT (RDATASET, 0, mkChar (cvalue));
  } else {
    RDATADEF (REALSXP, cnt);
    RNCCHECK (nc_get_att_double (ncid, varid, attname, REAL (RDATASET)));
  }

  RNCRETURN (NC_NOERR);
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
  ROBJDEF (VECSXP, 4);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    RNCCHECK (R_nc_var_id (var, ncid, &varid));
  }

  RNCCHECK (R_nc_att_name (att, ncid, varid, attname));

  /*-- Inquire about the attribute --------------------------------------------*/
  RNCCHECK (nc_inq_attid (ncid, varid, attname, &attid));

  RNCCHECK (nc_inq_att (ncid, varid, attname, &type, &cnt));

  /*-- Convert nc_type to char ------------------------------------------------*/
  atttype = R_nc_type2str (ncid, type);

  /*-- Returning the list -----------------------------------------------------*/
  SET_VECTOR_ELT (RDATASET, 0, ScalarInteger (attid));
  SET_VECTOR_ELT (RDATASET, 1, mkString (attname));
  SET_VECTOR_ELT (RDATASET, 2, mkString (atttype));
  /* cnt may not fit in integer, so return as double */
  SET_VECTOR_ELT (RDATASET, 3, ScalarReal (cnt));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_att (SEXP nc, SEXP var, SEXP att,
              SEXP type, SEXP value)
{
  int ncid, varid;
  const char *attname, *charval=NULL;
  const double *realval=NULL;
  nc_type nctype;
  size_t  nccnt;
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    RNCCHECK (R_nc_var_id (var, ncid, &varid));
  }

  attname = CHAR (STRING_ELT (att, 0));

  /*-- Convert char to nc_type ------------------------------------------------*/
  RNCCHECK (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &nctype));

  /*-- Find length of the attribute -------------------------------------------*/
  if (nctype==NC_CHAR) {
    charval = CHAR (STRING_ELT (value, 0));
    nccnt = strlen (charval);
  } else {
    realval = REAL (value);
    nccnt = xlength(value);
  }

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid));

  /*-- Create the attribute ---------------------------------------------------*/
  if (nctype==NC_CHAR) {
    RNCCHECK (nc_put_att_text (ncid, varid, attname, nccnt, charval));
  } else {
    RNCCHECK (nc_put_att_double (ncid, varid, attname, nctype, nccnt, realval));
  }

  RNCRETURN (NC_NOERR);
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
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  if (R_nc_strcmp(var, "NC_GLOBAL")) {
    varid = NC_GLOBAL;
  } else {
    RNCCHECK (R_nc_var_id (var, ncid, &varid));
  }

  RNCCHECK (R_nc_att_name (att, ncid, varid, attname));

  newnamep = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid));

  /*-- Rename the attribute ---------------------------------------------------*/
  RNCCHECK (nc_rename_att (ncid, varid, attname, newnamep));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_close()                                                               *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_close (SEXP ptr)
{
  int *fileid;
  ROBJDEF (NOSXP, 0);

  fileid = R_ExternalPtrAddr (ptr);
  if (!fileid) {
    RNCRETURN (NC_NOERR);
  }

  RNCCHECK (nc_close (*fileid));
  R_Free (fileid);
  R_ClearExternalPtr (ptr);

  RNCRETURN (NC_NOERR);
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
  SEXP Rptr;
  ROBJDEF (INTSXP, 1);

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
  RNCCHECK (nc_create (R_ExpandFileName (CHAR (STRING_ELT (filename, 0))),
                       cmode, &ncid));
  INTEGER (RDATASET)[0] = ncid;

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = R_MakeExternalPtr (fileid, R_NilValue, R_NilValue);
  PROTECT (Rptr);
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (RDATASET, install ("handle_ptr"), Rptr);
  UNPROTECT (1);

  /*-- Set the fill mode ------------------------------------------------------*/
  RNCCHECK (nc_set_fill (ncid, fillmode, &old_fillmode));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_dim (SEXP nc, SEXP dimname, SEXP size, SEXP unlim)
{
  int ncid;
  const char *dimnamep;
  size_t nccnt;
  ROBJDEF (INTSXP, 1);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  dimnamep = CHAR (STRING_ELT (dimname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid));

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

  RNCCHECK (nc_def_dim (ncid, dimnamep, nccnt, INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/* Private function to find all unlimited dimensions visible in a file or group.
   The netcdf4 function nc_inq_unlimdims does not check ancestors of a group.
   Returns netcdf status. If no error occurs, nunlim and unlimids are set.
 */
static int
R_nc_unlimdims (int ncid, int *nunlim, int **unlimids, int ancestors)
{
  int status, format, ndims, ntmp, *tmpdims;

  *nunlim = 0;

  status = nc_inq_format (ncid, &format);
  if (status != NC_NOERR) {
    return status;
  }

  if (format == NC_FORMAT_NETCDF4) {
    status = nc_inq_dimids (ncid, &ndims, NULL, 1);
    if (status != NC_NOERR) {
      return (status);
    }

    /* At most, all visible dimensions could be unlimited */
    *unlimids = (void *) (R_alloc (ndims, sizeof (int)));
    tmpdims = (void *) (R_alloc (ndims, sizeof (int)));

    /* Get unlimited dimensions in this group and (optionally) its ancestors */
    do {
      status = nc_inq_unlimdims (ncid, &ntmp, tmpdims);
      if (status != NC_NOERR) {
        return status;
      }
      if ((ntmp + *nunlim) <= ndims) {
        memcpy (&*unlimids[*nunlim], tmpdims, ntmp * sizeof (int));
        *nunlim += ntmp;
      } else {
        /* Avoid a segfault in case nc_inq_unlimdims starts checking ancestors */
        return NC_ENOMEM;
      }
    } while (ancestors && nc_inq_grp_parent (ncid, &ncid) == NC_NOERR);

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
  ROBJDEF (VECSXP, 4);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  RNCCHECK (R_nc_dim_id (dim, ncid, &dimid, 0));

  /*-- Inquire the dimension --------------------------------------------------*/
  RNCCHECK (nc_inq_dim (ncid, dimid, dimname, &dimlen));

  /*-- Check if it is an unlimited dimension ---------------------------------*/
  RNCCHECK (R_nc_unlimdims (ncid, &nunlim, &unlimids, 1));

  isunlim = 0;
  for (ii = 0; ii < nunlim; ii++) {
    if (unlimids[ii] == dimid) {
      isunlim = 1;
      break;
    }
  }

  /*-- Returning the list -----------------------------------------------------*/
  SET_VECTOR_ELT (RDATASET, 0, ScalarInteger (dimid));
  SET_VECTOR_ELT (RDATASET, 1, mkString (dimname));
  /* Dimension length may be larger than integer, so return as double */
  SET_VECTOR_ELT (RDATASET, 2, ScalarReal (dimlen));
  SET_VECTOR_ELT (RDATASET, 3, ScalarLogical (isunlim));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_dim()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_dim (SEXP nc, SEXP dim, SEXP newname)
{
  int ncid, dimid;
  const char *newnamep;
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  RNCCHECK (R_nc_dim_id (dim, ncid, &dimid, 0));

  newnamep = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid));

  /*-- Rename the dimension ---------------------------------------------------*/
  RNCCHECK (nc_rename_dim (ncid, dimid, newnamep));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_file (SEXP nc)
{
  int ncid, ndims, nvars, ngatts, unlimdimid, format;
  ROBJDEF (VECSXP, 5);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  /*-- Inquire about the NetCDF dataset ---------------------------------------*/
  RNCCHECK (nc_inq (ncid, &ndims, &nvars, &ngatts, &unlimdimid));
  if (unlimdimid == -1 ) {
    unlimdimid = NA_INTEGER;
  }

  /*-- Inquire about the NetCDF format ----------------------------------------*/
  RNCCHECK (nc_inq_format (ncid, &format));

  /*-- Returning the list -----------------------------------------------------*/
  SET_VECTOR_ELT (RDATASET, 0, ScalarInteger (ndims));
  SET_VECTOR_ELT (RDATASET, 1, ScalarInteger (nvars));
  SET_VECTOR_ELT (RDATASET, 2, ScalarInteger (ngatts));
  SET_VECTOR_ELT (RDATASET, 3, ScalarInteger (unlimdimid));
  SET_VECTOR_ELT (RDATASET, 4, mkString (R_nc_format2str (format)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_open()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill)
{
  int ncid, omode, fillmode, old_fillmode, *fileid;
  SEXP Rptr;
  ROBJDEF (INTSXP, 1);

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
  RNCCHECK (nc_open (R_ExpandFileName (CHAR (STRING_ELT (filename, 0))),
                     omode, &ncid));
  INTEGER (RDATASET)[0] = ncid;

  /*-- Arrange for file to be closed if handle is garbage collected -----------*/
  fileid = R_Calloc (1, int);
  *fileid = ncid;
  Rptr = R_MakeExternalPtr (fileid, R_NilValue, R_NilValue);
  PROTECT (Rptr);
  R_RegisterCFinalizerEx (Rptr, &R_nc_finalizer, TRUE);
  setAttrib (RDATASET, install ("handle_ptr"), Rptr);
  UNPROTECT (1);

  /*-- Set the fill mode ------------------------------------------------------*/
  if (asLogical(write)) {
    RNCCHECK (nc_set_fill (ncid, fillmode, &old_fillmode));
  }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_sync()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_sync (SEXP nc)
{
  int ncid;
  ROBJDEF (NOSXP, 0);

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  ncid = asInteger(nc);
  RNCCHECK( R_nc_enddef (ncid));

  /*-- Sync the file ----------------------------------------------------------*/
  RNCCHECK (nc_sync (ncid));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_var (SEXP nc, SEXP varname, SEXP type, SEXP dims)
{
  int ncid, ii, dimids[NC_MAX_VAR_DIMS], ndims;
  nc_type xtype;
  const char *varnamep;
  ROBJDEF (INTSXP, 1);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  varnamep = CHAR (STRING_ELT (varname, 0));

  RNCCHECK (R_nc_str2type (ncid, CHAR (STRING_ELT (type, 0)), &xtype));

  ndims = length(dims);
  if (ndims > NC_MAX_VAR_DIMS) {
    RNCRETURN(NC_EMAXDIMS);
  }

  for (ii=0; ii<ndims; ii++) {
    /* Handle dimension names and convert from R to C storage order */
    RNCCHECK (R_nc_dim_id (dims, ncid, &dimids[ndims-1-ii], ii));
  }

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid));

  /*-- Define the variable ----------------------------------------------------*/
  RNCCHECK (nc_def_var (
            ncid, varnamep, xtype, ndims, dimids, INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP rawchar)
{
  int ncid, varid, ndims, rank, *intp;
  size_t ii, cstart[MAX_NC_DIMS], ccount[MAX_NC_DIMS], arrlen, strcnt, strlen;
  nc_type xtype;
  char *charbuf, *charcpy, **strbuf;
  SEXP rdim;
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  RNCCHECK (R_nc_var_id (var, ncid, &varid));

  /*-- Handle NA values in start & count and reverse dimension order ----------*/
  RNCCHECK ( R_nc_slice (start, count, ncid, varid, &ndims, cstart, ccount));

  /*-- Determine total number of elements in data array -----------------------*/
  arrlen = R_nc_length (ndims, ccount);
  rank = ndims;

  /*-- Determine type of external data ----------------------------------------*/
  RNCCHECK (nc_inq_vartype ( ncid, varid, &xtype));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  RNCCHECK (R_nc_enddef (ncid));

  /*-- Allocate memory and read variable from file ----------------------------*/
  switch (xtype) {
  case NC_CHAR:
    if (asLogical (rawchar)) {
      RDATADEF (RAWSXP, arrlen);
      if (arrlen > 0) {
        RNCCHECK (nc_get_vara_text (ncid, varid, cstart, ccount,
                                    (char *) RAW (RDATASET)));
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
      RDATADEF (STRSXP, strcnt);
      if (arrlen > 0) {
        charbuf = R_alloc (arrlen, sizeof (char));
        charcpy = R_alloc (strlen+1, sizeof (char));
        charcpy[strlen] = '\0';
        RNCCHECK (nc_get_vara_text (ncid, varid, cstart, ccount, charbuf));
        for (ii=0; ii<strcnt; ii++) {
          /* mkCharLen+strnlen would be more efficient than strncpy+mkChar,
             but strnlen is not available on all supported R platforms */
          strncpy(charcpy, &charbuf[ii*strlen], strlen);
          SET_STRING_ELT (RDATASET, ii, mkChar(charcpy));
        }
      }
    }
    break;
  case NC_STRING:
    RDATADEF (STRSXP, arrlen);
    if (arrlen > 0) {
      strbuf = (void *) R_alloc (arrlen, sizeof(char *));
      RNCCHECK (nc_get_vara_string (ncid, varid, cstart, ccount, strbuf));
      for (ii=0; ii<arrlen; ii++) {
        SET_STRING_ELT (RDATASET, ii, mkChar (strbuf[ii]));
      }
      RNCCHECK (nc_free_string (arrlen, strbuf));
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
    RDATADEF (REALSXP, arrlen);
    if (arrlen > 0) {
      RNCCHECK (nc_get_vara_double (ncid, varid,
                                    cstart, ccount, REAL (RDATASET)));
    }
    break;
  default:
    RNCRETURN (NC_EBADTYPE);
  }

  /*-- Set dimension attribute for arrays -------------------------------------*/
  if (rank > 0) {
    rdim = PROTECT( allocVector (INTSXP, rank));
    intp = INTEGER (rdim);
    for ( ii=0; ii<rank; ii++ ) {
      intp[ii] = ccount[rank-1-ii];
    }
    setAttrib(RDATASET, R_DimSymbol, rdim);
    UNPROTECT(1);
  }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_var (SEXP nc, SEXP var)
{
  int ii, ncid, varid, ndims, natts, cdimids[MAX_NC_DIMS], *rdimids;
  const char *vartype;
  char varname[NC_MAX_NAME + 1];
  nc_type xtype;
  ROBJDEF (VECSXP, 6);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  RNCCHECK (R_nc_var_id (var, ncid, &varid));

  /*-- Inquire the variable ---------------------------------------------------*/
  RNCCHECK (nc_inq_var (ncid, varid, varname, &xtype, &ndims,
                        cdimids, &natts));

  /*-- Convert nc_type to char ------------------------------------------------*/
  vartype = R_nc_type2str (ncid, xtype);

  /*-- Construct the output list ----------------------------------------------*/
  SET_VECTOR_ELT (RDATASET, 0, ScalarInteger (varid));
  SET_VECTOR_ELT (RDATASET, 1, mkString (varname));
  SET_VECTOR_ELT (RDATASET, 2, mkString (vartype));
  SET_VECTOR_ELT (RDATASET, 3, ScalarInteger (ndims));

  if (ndims > 0) {
    /* Return vector of dimension ids in R order */
    SET_VECTOR_ELT (RDATASET, 4, allocVector (INTSXP, ndims));
    rdimids = INTEGER (VECTOR_ELT (RDATASET, 4));
    for (ii=0; ii<ndims; ii++) {
      rdimids[ii] = cdimids[ndims-1-ii];
    }
  } else {
    /* Return single NA for scalars */
    SET_VECTOR_ELT (RDATASET, 4, allocVector (INTSXP, 1));
    INTEGER (VECTOR_ELT (RDATASET, 4))[0] = NA_INTEGER;
  }

  SET_VECTOR_ELT (RDATASET, 5, ScalarInteger (natts));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP data)
{
  int ncid, varid, ndims;
  size_t ii, cstart[MAX_NC_DIMS], ccount[MAX_NC_DIMS], arrlen, strcnt, strlen;
  nc_type xtype;
  char *charbuf;
  const char **strbuf;
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  RNCCHECK (R_nc_var_id (var, ncid, &varid));

  /*-- Handle NA values in start & count and reverse dimension order ----------*/
  RNCCHECK ( R_nc_slice (start, count, ncid, varid, &ndims, cstart, ccount));

  /*-- Find total number of elements in data array ----------------------------*/
  arrlen = R_nc_length (ndims, ccount);
  if (arrlen == 0) {
    /* Nothing to write, so return immediately */
    RNCRETURN(NC_NOERR)
  }

  /*-- Determine type of external data ----------------------------------------*/
  RNCCHECK (nc_inq_vartype ( ncid, varid, &xtype));

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  RNCCHECK (R_nc_enddef (ncid));

  /*-- Write variable to file -------------------------------------------------*/
  switch (xtype) {
  case NC_CHAR:
    if (TYPEOF (data) == RAWSXP) {
      if (xlength (data) >= arrlen) {
        RNCCHECK (nc_put_vara_text (ncid, varid, cstart, ccount,
                                    (char *) RAW (data)));
        RNCRETURN (NC_NOERR);
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
        RNCCHECK (nc_put_vara_text (ncid, varid, cstart, ccount, charbuf));
        RNCRETURN (NC_NOERR);
      }
    }
    break;
  case NC_STRING:
    if (isString (data)) {
      if (xlength (data) >= arrlen) {
	strbuf = (void *) R_alloc (arrlen, sizeof(char *));
	for (ii=0; ii<arrlen; ii++) {
	  strbuf[ii] = CHAR( STRING_ELT( data, ii));
	}
	RNCCHECK (nc_put_vara_string (ncid, varid, cstart, ccount, strbuf));
        RNCRETURN (NC_NOERR);
      }
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
	RNCCHECK (nc_put_vara_double (ncid, varid, cstart, ccount, REAL (data)));
        RNCRETURN (NC_NOERR);
      } else if (isInteger (data) || isLogical (data)) {
	RNCCHECK (nc_put_vara_int (ncid, varid, cstart, ccount, INTEGER (data)));
        RNCRETURN (NC_NOERR);
      }
    }
    break;
  default:
    RNCRETURN (NC_EBADTYPE);
  }

  RNCRETURN (NC_EINVAL);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_var()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_var (SEXP nc, SEXP var, SEXP newname)
{
  int ncid, varid;
  const char *cnewname;
  ROBJDEF (NOSXP, 0);

  /*-- Convert arguments to netcdf ids ----------------------------------------*/
  ncid = asInteger (nc);

  RNCCHECK (R_nc_var_id (var, ncid, &varid));

  cnewname = CHAR (STRING_ELT (newname, 0));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCCHECK( R_nc_redef (ncid));

  /*-- Rename the variable ----------------------------------------------------*/
  RNCCHECK (nc_rename_var (ncid, varid, cnewname));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_grp()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_grp (SEXP nc, SEXP grpname)
{
  int ncid;
  const char *cgrpname;
  ROBJDEF (INTSXP, 1);

  /* Convert arguments to netcdf ids */
  ncid = asInteger (nc);

  cgrpname = CHAR (STRING_ELT (grpname, 0));

  /* Enter define mode */
  RNCCHECK( R_nc_redef (ncid));

  /* Define the group */
  RNCCHECK (nc_def_grp (ncid, cgrpname, INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_parent()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_parent (SEXP nc)
{
  ROBJDEF (INTSXP, 1);

  /* Get parent group */
  RNCCHECK (nc_inq_grp_parent (asInteger (nc), INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_natts()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_natts (SEXP nc)
{
  ROBJDEF (INTSXP, 1);

  /* Get number of attributes in group */
  RNCCHECK (nc_inq_natts (asInteger (nc), INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
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
  ROBJDEF (STRSXP, 1);

  ncid = asInteger (nc);

  if (asLogical (full)) {
    RNCCHECK (nc_inq_grpname_full (ncid, &namelen, NULL));

    fullname = R_alloc (namelen + 1, sizeof (char));
    RNCCHECK (nc_inq_grpname_full (ncid, NULL, fullname));
    name = fullname;
  } else {
    RNCCHECK (nc_inq_grpname (ncid, namebuf));
    name = namebuf;
  }

  SET_STRING_ELT (RDATASET, 0, mkChar (name));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_ncid()                                                        *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_ncid (SEXP nc, SEXP grpname, SEXP full)
{
  int ncid;
  const char *cgrpname;
  ROBJDEF (INTSXP, 1);

  ncid = asInteger (nc);
  cgrpname = CHAR (STRING_ELT (grpname, 0));

  if (asLogical (full)) {
    RNCCHECK (nc_inq_grp_full_ncid (ncid, cgrpname, INTEGER (RDATASET)));
  } else {
    RNCCHECK (nc_inq_grp_ncid (ncid, cgrpname, INTEGER (RDATASET)));
  }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  Get lists of ncids for components of a group                               *
\*-----------------------------------------------------------------------------*/

/* Template function returning a list of ncids for a group */
#define INQGRPIDS(RFUN, NCFUN) \
SEXP RFUN (SEXP nc) \
{ \
  int    ncid, count; \
  ncid = asInteger (nc); \
  ROBJDEF(NOSXP,0); \
  RNCCHECK(NCFUN(ncid, &count, NULL)); \
  RDATADEF(INTSXP,count); \
  RNCCHECK(NCFUN(ncid, NULL, INTEGER(RDATASET))); \
  RNCRETURN(NC_NOERR); \
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
  ROBJDEF (NOSXP, 0);

  ncid = asInteger (nc);
  full = asLogical (ancestors);

  RNCCHECK (nc_inq_dimids (ncid, &count, NULL, full));
  RDATADEF (INTSXP, count);
  RNCCHECK (nc_inq_dimids (ncid, NULL, INTEGER (RDATASET), full));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_unlimids()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_unlimids (SEXP nc, SEXP ancestors)
{
  int ncid, full, nunlim, *unlimids;
  ROBJDEF (NOSXP, 0);

  ncid = asInteger (nc);
  full = asLogical (ancestors);

  RNCCHECK (R_nc_unlimdims (ncid, &nunlim, &unlimids, full));

  RDATADEF (INTSXP, nunlim);

  /* Sort temporary results and copy to output structure */
  if (nunlim > 0) {
    R_isort(unlimids, nunlim);
    memcpy (INTEGER (RDATASET), unlimids, nunlim * sizeof (int));
  }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_grp()                                                          *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_rename_grp (SEXP nc, SEXP grpname)
{
  int ncid;
  const char *cgrpname;
  ROBJDEF (NOSXP, 0);

#if defined HAVE_DECL_NC_RENAME_GRP && HAVE_DECL_NC_RENAME_GRP
  ncid = asInteger (nc);
  cgrpname = CHAR (STRING_ELT (grpname, 0));

  /* Enter define mode */
  RNCCHECK( R_nc_redef (ncid));

  /* Rename the group */
  RNCCHECK (nc_rename_grp (ncid, cgrpname));

  RNCRETURN (NC_NOERR);

#else
  RNCRETURN (E_UNSUPPORTED);
#endif
}


/*=============================================================================*\
 *  Udunits library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_ut_calendar()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_ut_calendar (SEXP unitstring, SEXP values)
{
  int year, month, day, hour, minute, status, isreal;
  float second;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  double dtmp, *dout;
  size_t ii, count;
  utUnit utunit;
  ROBJDEF (NOSXP, 0);

  /* Handle arguments and initialise outputs */
  cstring = CHAR (STRING_ELT (unitstring, 0));
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values);

  RDATADEF (REALSXP, count * 6);
  dout = REAL (RDATASET);

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

  /*-- Returning the list -----------------------------------------------------*/
cleanup:
#ifdef HAVE_LIBUDUNITS2
  utFree (&utunit);
#endif
  RUTRETURN (status);
}


/*-----------------------------------------------------------------------------*\
 *  R_ut_init()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_ut_init (SEXP path)
{
  int status;
  ROBJDEF (NOSXP, 0);

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
  RUTRETURN (status);
}


/*-----------------------------------------------------------------------------*\
 *  R_ut_inv_calendar()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP
R_ut_inv_calendar (SEXP unitstring, SEXP values)
{
  int status, itmp, isreal, isfinite;
  const int *ivals=NULL;
  const double *dvals=NULL;
  const char *cstring;
  double datetime[6], *dout, dtmp;
  size_t ii, jj, count;
  utUnit utunit;
  ROBJDEF (NOSXP, 0);

  /* Handle arguments and initialise outputs */
  cstring = CHAR (STRING_ELT (unitstring, 0));
  isreal = isReal (values);
  if (isreal) {
    dvals = REAL (values);
  } else {
    ivals = INTEGER (values);
  }
  count = xlength (values) / 6;

  RDATADEF (REALSXP, count);
  dout = REAL (RDATASET);

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
  RUTRETURN (status);
}

/*=============================================================================*/

/*=============================================================================*\
 *  SCRATCH                                                                    *
\*=============================================================================*/
