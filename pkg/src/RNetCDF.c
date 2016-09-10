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
 *  Macros to initialise and return R data structures                          *
\*=============================================================================*/

#define NOSXP -11111
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
  { int result = STATUS; \
    SET_VECTOR_ELT(retlist, 0, ScalarInteger(result)); \
    if (result == E_UNSUPPORTED) { \
      SET_VECTOR_ELT(retlist, 1, \
        mkString("Operation requires RNetCDF built with newer netcdf library")); \
    } else if (result != NC_NOERR) { \
      SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(result))); \
    } \
    UNPROTECT(1); \
    return(retlist); \
  }

#define RNCCHECK(STATUS) \
  { int result = STATUS; \
    if (result != NC_NOERR) { \
        RNCRETURN(result); \
    } \
  }

#define RNCREDEF(NCIDINT) \
  { int result = nc_redef(NCIDINT); \
    if ((result != NC_NOERR) && (result != NC_EINDEFINE)) { \
      RNCRETURN (result); \
    } \
  }

#define RNCENDDEF(NCIDINT) \
  { int result = nc_enddef(NCIDINT); \
    if ((result != NC_NOERR) && (result != NC_ENOTINDEFINE)) { \
      RNCRETURN (result); \
    } \
  }

#define RUTRETURN(STATUS) \
  SET_VECTOR_ELT(retlist, 0, ScalarInteger(STATUS)); \
  if (STATUS != 0) { \
    SET_VECTOR_ELT(retlist, 1, mkString(R_ut_strerror(STATUS))); \
  } \
  UNPROTECT(1); \
  return(retlist);


/*=============================================================================*\
 *  Reusable internal functions
\*=============================================================================*/

/* Convert netcdf type code to string label.
 */
static const char *
R_nc_type2str (int ncid, nc_type xtype)
{
  static char str[NC_MAX_NAME + 1];
  switch (xtype)
    {
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
	  NC_NOERR)
	{
	  return str;
	}
      else
	{
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
  int typelen;
  typelen = strlen (str);
  *xtype = NC_NAT;
  if (typelen >= 6)
    {
      switch (str[3])
	{
	case 'B':
	  if (strcmp (str, "NC_BYTE") == 0)
	    *xtype = NC_BYTE;
	  break;
	case 'C':
	  if (strcmp (str, "NC_CHAR") == 0)
	    *xtype = NC_CHAR;
	  break;
	case 'D':
	  if (strcmp (str, "NC_DOUBLE") == 0)
	    *xtype = NC_DOUBLE;
	  break;
	case 'F':
	  if (strcmp (str, "NC_FLOAT") == 0)
	    *xtype = NC_FLOAT;
	  break;
	case 'I':
	  switch (str[6])
	    {
	    case '\0':
	      if (strcmp (str, "NC_INT") == 0)
		*xtype = NC_INT;
	      break;
	    case '6':
	      if (strcmp (str, "NC_INT64") == 0)
		*xtype = NC_INT64;
	      break;
	    }
	  break;
	case 'L':
	  if (strcmp (str, "NC_LONG") == 0)
	    *xtype = NC_LONG;
	  break;
	case 'S':
	  switch (str[4])
	    {
	    case 'H':
	      if (strcmp (str, "NC_SHORT") == 0)
		*xtype = NC_SHORT;
	      break;
	    case 'T':
	      if (strcmp (str, "NC_STRING") == 0)
		*xtype = NC_STRING;
	      break;
	    }
	  break;
	case 'U':
	  if (typelen >= 7)
	    {
	      switch (str[7])
		{
		case '\0':
		  if (strcmp (str, "NC_UINT") == 0)
		    *xtype = NC_UINT;
		  break;
		case '6':
		  if (strcmp (str, "NC_UINT64") == 0)
		    *xtype = NC_UINT64;
		  break;
		case 'E':
		  if (strcmp (str, "NC_UBYTE") == 0)
		    *xtype = NC_UBYTE;
		  break;
		case 'R':
		  if (strcmp (str, "NC_USHORT") == 0)
		    *xtype = NC_USHORT;
		  break;
		}
	    }
	  break;
	}
    }

  if (*xtype == NC_NAT)
    {
      /* Try to get id of a user defined type */
      return nc_inq_typeid (ncid, str, xtype);
    }
  else
    {
      return NC_NOERR;
    }
}


/* Private function called by qsort to compare integers */
static int
R_nc_int_cmp (const void *a, const void *b)
{
  const int *ia = (const int *) a;
  const int *ib = (const int *) b;
  return *ia - *ib;
}


/* Convert udunits error code to a string */
static const char *
R_ut_strerror (int errcode)
{
  switch (errcode)
    {
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
  switch (format)
    {
    case NC_FORMAT_CLASSIC:
      return "classic";
    case NC_FORMAT_64BIT_OFFSET:
      return "offset64";
    case NC_FORMAT_CDF5:
      return "cdf5";
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
  int attid;
  if (isNumeric (att))
    {
      return nc_inq_attname (ncid, varid, asInteger (att), attname);
    }
  else if (isString (att))
    {
      strcpy (attname, CHAR (STRING_ELT (att, 0)));
      return NC_NOERR;
    }
  else
    {
      return NC_EINVAL;
    }
}


/* Convert dimension identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
static int
R_nc_dim_id (SEXP dim, int ncid, int *dimid)
{
  if (isNumeric (dim))
    {
      *dimid = asInteger (dim);
      return NC_NOERR;
    }
  else if (isString (dim))
    {
      return nc_inq_dimid (ncid, CHAR (STRING_ELT (dim, 0)), dimid);
    }
  else
    {
      return NC_EINVAL;
    }
}


/* Convert variable identifier from R string or number to an integer.
   Result is a netcdf status value.
 */
static int
R_nc_var_id (SEXP var, int ncid, int *varid)
{
  if (isNumeric (var))
    {
      *varid = asInteger (var);
      return NC_NOERR;
    }
  else if (isString (var))
    {
      return nc_inq_varid (ncid, CHAR (STRING_ELT (var, 0)), varid);
    }
  else
    {
      return NC_EINVAL;
    }
}


/*=============================================================================*\
 *  NetCDF library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_nc_copy_att()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_copy_att (SEXP ncid_in, SEXP varid_in, SEXP globflag_in, SEXP attname,
	       SEXP ncid_out, SEXP varid_out, SEXP globflag_out)
{
  int ncvarid_in, ncvarid_out;
  const char *ncattname;
  ROBJDEF (NOSXP, 0);

  /*-- Check if handling global attributes ------------------------------------*/
  if (INTEGER (globflag_in)[0] == 1)
    ncvarid_in = NC_GLOBAL;
  else
    ncvarid_in = INTEGER (varid_in)[0];

  if (INTEGER (globflag_out)[0] == 1)
    ncvarid_out = NC_GLOBAL;
  else
    ncvarid_out = INTEGER (varid_out)[0];

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid_out)[0]);

  /*-- Copy the attribute -----------------------------------------------------*/
  ncattname = CHAR (STRING_ELT (attname, 0));
  RNCCHECK (nc_copy_att (INTEGER (ncid_in)[0], ncvarid_in, ncattname,
			 INTEGER (ncid_out)[0], ncvarid_out));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_delete_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_delete_att (SEXP ncid, SEXP varid, SEXP globflag, SEXP attname)
{
  int ncvarid;
  const char *ncattname;
  ROBJDEF (NOSXP, 0);

  /*-- Check if it is a global attribute --------------------------------------*/
  if (INTEGER (globflag)[0] == 1)
    ncvarid = NC_GLOBAL;
  else
    ncvarid = INTEGER (varid)[0];

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid)[0]);

  /*-- Delete the attribute ---------------------------------------------------*/
  ncattname = CHAR (STRING_ELT (attname, 0));
  RNCCHECK (nc_del_att (INTEGER (ncid)[0], ncvarid, ncattname));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_att (SEXP ncid, SEXP varid, SEXP name, SEXP numflag, SEXP globflag)
{
  int ncvarid;
  const char *ncattname;
  char *cvalue;
  size_t attlen;
  ROBJDEF (NOSXP, 0);

  /*-- Check if it is a global attribute --------------------------------------*/
  if (INTEGER (globflag)[0] == 1)
    ncvarid = NC_GLOBAL;
  else
    ncvarid = INTEGER (varid)[0];

  /*-- Get the attribute's length ---------------------------------------------*/
  ncattname = CHAR (STRING_ELT (name, 0));
  RNCCHECK (nc_inq_attlen (INTEGER (ncid)[0], ncvarid, ncattname, &attlen));

  /*-- Get the attribute ------------------------------------------------------*/
  if (INTEGER (numflag)[0] == 1)
    {
      RDATADEF (REALSXP, attlen);
      RNCCHECK (nc_get_att_double (INTEGER (ncid)[0], ncvarid, ncattname,
				   REAL (RDATASET)));
    }
  else
    {
      RDATADEF (STRSXP, 1);
      cvalue = (char *) R_alloc (attlen + 1, sizeof (char));
      RNCCHECK (nc_get_att_text
		(INTEGER (ncid)[0], ncvarid, ncattname, cvalue));
      cvalue[attlen + 1] = '\0';
      SET_STRING_ELT (RDATASET, 0, mkChar (cvalue));
    }
  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_att (SEXP ncid, SEXP varid, SEXP attname, SEXP attid,
	      SEXP nameflag, SEXP globflag)
{
  int ncvarid, ncattid;
  const char *atttype, *ncattname;
  char namebuf[NC_MAX_NAME + 1];
  size_t ncattlen;
  nc_type xtype;
  ROBJDEF (VECSXP, 4);

  /*-- Check if it is a global attribute --------------------------------------*/
  if (INTEGER (globflag)[0] == 1)
    ncvarid = NC_GLOBAL;
  else
    ncvarid = INTEGER (varid)[0];

  /*-- Get the attribute name or ID -------------------------------------------*/
  if (INTEGER (nameflag)[0] == 1)
    {
      ncattname = CHAR (STRING_ELT (attname, 0));
      RNCCHECK (nc_inq_attid (INTEGER (ncid)[0], ncvarid,
			      CHAR (STRING_ELT (attname, 0)), &ncattid));
    }
  else
    {
      ncattid = INTEGER (attid)[0];
      RNCCHECK (nc_inq_attname
		(INTEGER (ncid)[0], ncvarid, ncattid, namebuf));
      ncattname = (const char *) namebuf;
    }

  /*-- Inquire the attribute --------------------------------------------------*/
  RNCCHECK (nc_inq_att
	    (INTEGER (ncid)[0], ncvarid, ncattname, &xtype, &ncattlen));

  /*-- Convert nc_type to char ------------------------------------------------*/
  atttype = R_nc_type2str (INTEGER (ncid)[0], xtype);

  /*-- Returning the list -----------------------------------------------------*/
  SET_VECTOR_ELT (RDATASET, 0, ScalarInteger (ncattid));
  SET_VECTOR_ELT (RDATASET, 1, mkString (ncattname));
  SET_VECTOR_ELT (RDATASET, 2, mkString (atttype));
  SET_VECTOR_ELT (RDATASET, 3, ScalarInteger ((int) ncattlen));
  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_att (SEXP ncid, SEXP varid, SEXP name, SEXP type, SEXP attlen,
	      SEXP numflag, SEXP globflag, SEXP value)
{
  int ncvarid, ncattlen;
  const char *ncattname;
  nc_type xtype;
  ROBJDEF (NOSXP, 0);

  /*-- Check if it is a global attribute --------------------------------------*/
  if (INTEGER (globflag)[0] == 1)
    ncvarid = NC_GLOBAL;
  else
    ncvarid = INTEGER (varid)[0];

  /*-- Convert char to nc_type ------------------------------------------------*/
  RNCCHECK (R_nc_str2type
	    (INTEGER (ncid)[0], CHAR (STRING_ELT (type, 0)), &xtype));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid)[0]);

  /*-- Create the attribute ---------------------------------------------------*/
  ncattlen = INTEGER (attlen)[0];
  ncattname = CHAR (STRING_ELT (name, 0));
  if (INTEGER (numflag)[0] == 1)
    {
      RNCCHECK (nc_put_att_double
		(INTEGER (ncid)[0], ncvarid, ncattname, xtype, ncattlen,
		 REAL (value)));
    }
  else
    {
      ncattlen = strlen (CHAR (STRING_ELT (value, 0)));
      RNCCHECK (nc_put_att_text (INTEGER (ncid)[0], ncvarid, ncattname,
				 ncattlen, CHAR (STRING_ELT (value, 0))));
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_att (SEXP ncid, SEXP varid, SEXP globflag, SEXP attname,
		 SEXP newname)
{
  int ncvarid;
  const char *ncattname, *ncnewname;
  ROBJDEF (NOSXP, 0);

  /*-- Check if it is a global attribute --------------------------------------*/
  if (INTEGER (globflag)[0] == 1)
    ncvarid = NC_GLOBAL;
  else
    ncvarid = INTEGER (varid)[0];

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid)[0]);

  /*-- Rename the attribute ---------------------------------------------------*/
  ncattname = CHAR (STRING_ELT (attname, 0));
  ncnewname = CHAR (STRING_ELT (newname, 0));
  RNCCHECK (nc_rename_att (INTEGER (ncid)[0], ncvarid, ncattname, ncnewname));

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
  if (!fileid)
    {
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
  if (INTEGER (clobber)[0] == 0)
    cmode = NC_NOCLOBBER;
  else
    cmode = NC_CLOBBER;

  /*-- Determine which buffer scheme shall be used ----------------------------*/
  if (INTEGER (share)[0] != 0)
    cmode = cmode | NC_SHARE;

  /*-- Determine the fillmode -------------------------------------------------*/
  if (INTEGER (prefill)[0] == 0)
    fillmode = NC_NOFILL;
  else
    fillmode = NC_FILL;

  /*-- Set file format --------------------------------------------------------*/
  switch (INTEGER (format)[0])
    {
    case 2:
      cmode = cmode | NC_64BIT_OFFSET;
      break;
    case 3:
      cmode = cmode | NC_NETCDF4 | NC_CLASSIC_MODEL;
      break;
    case 4:
      cmode = cmode | NC_NETCDF4;
      break;
    default:
      /* Use default, which is netcdf classic */
      break;
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
R_nc_def_dim (SEXP ncid, SEXP dimname, SEXP size, SEXP unlimp)
{
  ROBJDEF (INTSXP, 1);

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid)[0]);

  /*-- Create the dimension ---------------------------------------------------*/
  if (INTEGER (unlimp)[0] == 1)
    {
      RNCCHECK (nc_def_dim (INTEGER (ncid)[0], CHAR (STRING_ELT (dimname, 0)),
			    NC_UNLIMITED, INTEGER (RDATASET)));
    }
  else
    {
      RNCCHECK (nc_def_dim (INTEGER (ncid)[0], CHAR (STRING_ELT (dimname, 0)),
			    INTEGER (size)[0], INTEGER (RDATASET)));
    }

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
  if (status != NC_NOERR)
    {
      return status;
    }

  if (format == NC_FORMAT_NETCDF4)
    {
      status = nc_inq_dimids (ncid, &ndims, NULL, 1);
      if (status != NC_NOERR)
	{
	  return (status);
	}

      /* At most, all visible dimensions could be unlimited */
      *unlimids = (int *) R_alloc (ndims, sizeof (int));
      tmpdims = (int *) R_alloc (ndims, sizeof (int));

      /* Get unlimited dimensions in this group and (optionally) its ancestors */
      do
	{
	  status = nc_inq_unlimdims (ncid, &ntmp, tmpdims);
	  if (status != NC_NOERR)
	    {
	      return status;
	    }
	  if ((ntmp + *nunlim) <= ndims)
	    {
	      memcpy (*unlimids + *nunlim * sizeof (int), tmpdims,
		      ntmp * sizeof (int));
	      *nunlim += ntmp;
	    }
	  else
	    {
	      /* Avoid a segfault in case nc_inq_unlimdims starts checking ancestors */
	      return NC_ENOMEM;
	    }
	}
      while (ancestors && nc_inq_grp_parent (ncid, &ncid) == NC_NOERR);

    }
  else
    {
      *unlimids = (int *) R_alloc (1, sizeof (int));
      status = nc_inq_unlimdim (ncid, *unlimids);
      if (status == NC_NOERR && **unlimids != -1)
	{
	  *nunlim = 1;
	}
    }

  return status;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dim (SEXP ncid, SEXP dimid, SEXP dimname, SEXP nameflag)
{
  int nunlim, *unlimids, isunlim, ncdimid, ii;
  size_t ncdimlen;
  char ncdimname[NC_MAX_NAME + 1];
  ROBJDEF (VECSXP, 4);

  /*-- Get the dimension ID if necessary --------------------------------------*/
  if (INTEGER (nameflag)[0] != 0)
    {
      RNCCHECK (nc_inq_dimid
		(INTEGER (ncid)[0], CHAR (STRING_ELT (dimname, 0)),
		 &ncdimid));
    }
  else
    {
      ncdimid = INTEGER (dimid)[0];
    }

  /*-- Inquire the dimension --------------------------------------------------*/
  RNCCHECK (nc_inq_dim (INTEGER (ncid)[0], ncdimid, ncdimname, &ncdimlen));

  /*-- Check if it is an unlimited dimension ---------------------------------*/
  RNCCHECK (R_nc_unlimdims (INTEGER (ncid)[0], &nunlim, &unlimids, 1));

  isunlim = 0;
  for (ii = 0; ii < nunlim; ii++)
    {
      if (unlimids[ii] == ncdimid)
	{
	  isunlim = 1;
	  break;
	}
    }

  /*-- Returning the list -----------------------------------------------------*/
  SET_VECTOR_ELT (RDATASET, 0, ScalarInteger (ncdimid));
  SET_VECTOR_ELT (RDATASET, 1, mkString (ncdimname));
  SET_VECTOR_ELT (RDATASET, 2, ScalarInteger (ncdimlen));
  SET_VECTOR_ELT (RDATASET, 3, ScalarInteger (isunlim));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_dim()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_dim (SEXP ncid, SEXP dimid, SEXP dimname, SEXP nameflag,
		 SEXP newname)
{
  int ncdimid;
  const char *ncdimname, *ncnewname;
  ROBJDEF (NOSXP, 0);

  ncdimid = INTEGER (dimid)[0];
  ncdimname = CHAR (STRING_ELT (dimname, 0));
  ncnewname = CHAR (STRING_ELT (newname, 0));

  /*-- Get the dimension ID if necessary --------------------------------------*/
  if (INTEGER (nameflag)[0] == 1)
    {
      RNCCHECK (nc_inq_dimid (INTEGER (ncid)[0], ncdimname, &ncdimid));
    }

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid)[0]);

  /*-- Rename the dimension ---------------------------------------------------*/
  RNCCHECK (nc_rename_dim (INTEGER (ncid)[0], ncdimid, ncnewname));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_file (SEXP ncid)
{
  int ndims, nvars, ngatts, unlimdimid, format;
  ROBJDEF (VECSXP, 5);

  /*-- Inquire about the NetCDF dataset ---------------------------------------*/
  RNCCHECK (nc_inq (INTEGER (ncid)[0], &ndims, &nvars, &ngatts, &unlimdimid));

  /*-- Inquire about the NetCDF format ----------------------------------------*/
  RNCCHECK (nc_inq_format (INTEGER (ncid)[0], &format));

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
  if (INTEGER (write)[0] == 0)
    omode = NC_NOWRITE;
  else
    omode = NC_WRITE;

  if (INTEGER (share)[0] != 0)
    omode = omode | NC_SHARE;

  /*-- Determine the fillmode -------------------------------------------------*/
  if (INTEGER (prefill)[0] == 0)
    fillmode = NC_NOFILL;
  else
    fillmode = NC_FILL;

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
  if (INTEGER (write)[0] != 0)
    {
      RNCCHECK (nc_set_fill (ncid, fillmode, &old_fillmode));
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_sync()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_sync (SEXP ncid)
{
  ROBJDEF (NOSXP, 0);

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  RNCENDDEF (INTEGER (ncid)[0]);

  /*-- Sync the file ----------------------------------------------------------*/
  RNCCHECK (nc_sync (INTEGER (ncid)[0]));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_var (SEXP ncid, SEXP varname, SEXP type, SEXP ndims, SEXP dimids)
{
  nc_type xtype;
  ROBJDEF (INTSXP, 1);

  /*-- Convert char to nc_type ------------------------------------------------*/
  RNCCHECK (R_nc_str2type
	    (INTEGER (ncid)[0], CHAR (STRING_ELT (type, 0)), &xtype));

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid)[0]);

  /*-- Define the variable ----------------------------------------------------*/
  RNCCHECK (nc_def_var
	    (INTEGER (ncid)[0], CHAR (STRING_ELT (varname, 0)), xtype,
	     INTEGER (ndims)[0], INTEGER (dimids), INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_vara_double()                                                     *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_vara_double (SEXP ncid, SEXP varid, SEXP start,
		      SEXP count, SEXP ndims)
{
  int i;
  size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS], varsize;
  ROBJDEF (NOSXP, 0);

  /*-- Copy dims from int to size_t, calculate total array size ---------------*/
  varsize = 1;
  for (i = 0; i < INTEGER (ndims)[0]; i++)
    {
      s_start[i] = (size_t) INTEGER (start)[i];
      s_count[i] = (size_t) INTEGER (count)[i];
      varsize *= s_count[i];
    }

  RDATADEF (REALSXP, varsize);

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  RNCENDDEF (INTEGER (ncid)[0]);

  /*-- Read variable from file ------------------------------------------------*/
  if (varsize > 0)
    {
      /* Some netcdf versions cannot handle zero-sized arrays */
      RNCCHECK (nc_get_vara_double (INTEGER (ncid)[0], INTEGER (varid)[0],
				    s_start, s_count, REAL (RDATASET)));
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_vara_text()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_get_vara_text (SEXP ncid, SEXP varid, SEXP start,
		    SEXP count, SEXP ndims, SEXP rawchar)
{
  int i;
  char *data, *tx_str;
  size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
  size_t tx_len, tx_num, varsize;
  ROBJDEF (NOSXP, 0);

  /*-- Copy dims from int to size_t, calculate number and length of strings ---*/
  for (i = 0; i < INTEGER (ndims)[0]; i++)
    {
      s_start[i] = (size_t) INTEGER (start)[i];
      s_count[i] = (size_t) INTEGER (count)[i];
    }

  if (INTEGER (ndims)[0] > 0)
    {
      tx_num = 1;
      for (i = 0; i < INTEGER (ndims)[0] - 1; i++)
	{
	  tx_num *= s_count[i];
	}
      tx_len = s_count[INTEGER (ndims)[0] - 1];
    }
  else
    {
      tx_num = 1;
      tx_len = 1;
    }
  varsize = tx_num * tx_len;

  /*-- Create output object ---------------------------------------------------*/
  if (INTEGER (rawchar)[0] > 0)
    {
      RDATADEF (RAWSXP, varsize);
    }
  else
    {
      RDATADEF (STRSXP, tx_num);
    }

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  RNCENDDEF (INTEGER (ncid)[0]);

  /*-- Read variable from file ------------------------------------------------*/
  if (INTEGER (rawchar)[0] > 0)
    {
      data = (char *) RAW (RDATASET);
    }
  else
    {
      data = (char *) R_alloc (varsize, sizeof (char));
    }

  if (varsize > 0)
    {
      /* Some netcdf versions cannot handle zero-sized arrays */
      RNCCHECK (nc_get_vara_text (INTEGER (ncid)[0], INTEGER (varid)[0],
				  s_start, s_count, data));
    }

  /*-- Copy from C to R character vector (if specified) -----------------------*/
  if (INTEGER (rawchar)[0] <= 0)
    {
      tx_str = (char *) R_alloc (tx_len + 1, sizeof (char));
      tx_str[tx_len] = '\0';	/* Final null character is never modified */
      for (i = 0; i < tx_num; i++)
	{
	  strncpy (tx_str, data + i * tx_len, tx_len);
	  SET_STRING_ELT (RDATASET, i, mkChar (tx_str));
	}
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_var (SEXP ncid, SEXP varid, SEXP varname, SEXP nameflag)
{
  int ncvarid, ndims, natts, i, *dimids;
  const char *vartype;
  char ncvarname[NC_MAX_NAME + 1];
  nc_type xtype;
  ROBJDEF (VECSXP, 6);

  /*-- Get the variable ID if necessary ---------------------------------------*/
  if (INTEGER (nameflag)[0] == 1)
    {
      RNCCHECK (nc_inq_varid
		(INTEGER (ncid)[0], CHAR (STRING_ELT (varname, 0)),
		 &ncvarid));
    }
  else
    {
      ncvarid = INTEGER (varid)[0];
    }

  /*-- Get the number of dimensions -------------------------------------------*/
  RNCCHECK (nc_inq_varndims (INTEGER (ncid)[0], ncvarid, &ndims));

  if (ndims == 0)
    {
      dimids = NULL;
    }
  else
    {
      SET_VECTOR_ELT (RDATASET, 4, allocVector (INTSXP, ndims));
      dimids = INTEGER (VECTOR_ELT (RDATASET, 4));
    }

  /*-- Inquire the variable ---------------------------------------------------*/
  RNCCHECK (nc_inq_var (INTEGER (ncid)[0], ncvarid, ncvarname, &xtype, &ndims,
			dimids, &natts));

  /*-- Convert nc_type to char ------------------------------------------------*/
  vartype = R_nc_type2str (INTEGER (ncid)[0], xtype);

  /*-- Returning the list -----------------------------------------------------*/
  SET_VECTOR_ELT (RDATASET, 0, ScalarInteger (ncvarid));
  SET_VECTOR_ELT (RDATASET, 1, mkString (ncvarname));
  SET_VECTOR_ELT (RDATASET, 2, mkString (vartype));
  SET_VECTOR_ELT (RDATASET, 3, ScalarInteger (ndims));
  /* List item 4 is already defined by nc_inq_var */
  SET_VECTOR_ELT (RDATASET, 5, ScalarInteger (natts));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_vara_double()                                                     *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_vara_double (SEXP ncid, SEXP varid, SEXP start,
		      SEXP count, SEXP ndims, SEXP data)
{
  int i;
  size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS], varsize;
  ROBJDEF (NOSXP, 0);

  /*-- Copy dims from int to size_t -------------------------------------------*/
  varsize = 1;
  for (i = 0; i < INTEGER (ndims)[0]; i++)
    {
      s_start[i] = (size_t) INTEGER (start)[i];
      s_count[i] = (size_t) INTEGER (count)[i];
      varsize *= s_count[i];
    }

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  RNCENDDEF (INTEGER (ncid)[0]);

  /*-- Put the var ------------------------------------------------------------*/
  if (varsize > 0)
    {
      /* Some netcdf versions cannot handle zero-sized arrays */
      RNCCHECK (nc_put_vara_double (INTEGER (ncid)[0], INTEGER (varid)[0],
				    s_start, s_count, REAL (data)));
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_vara_text()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_put_vara_text (SEXP ncid, SEXP varid, SEXP start,
		    SEXP count, SEXP ndims, SEXP rawchar, SEXP data)
{
  int i;
  char *ncdata;
  size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
  size_t tx_len, tx_num, varsize;
  ROBJDEF (NOSXP, 0);

  /*-- Copy dims from int to size_t, calculate number and length of strings ---*/
  for (i = 0; i < INTEGER (ndims)[0]; i++)
    {
      s_start[i] = (size_t) INTEGER (start)[i];
      s_count[i] = (size_t) INTEGER (count)[i];
    }

  if (INTEGER (ndims)[0] > 0)
    {
      tx_num = 1;
      for (i = 0; i < INTEGER (ndims)[0] - 1; i++)
	{
	  tx_num *= s_count[i];
	}
      tx_len = s_count[INTEGER (ndims)[0] - 1];
    }
  else
    {
      tx_num = 1;
      tx_len = 1;
    }
  varsize = tx_num * tx_len;

  /*-- Enter data mode (if necessary) -----------------------------------------*/
  RNCENDDEF (INTEGER (ncid)[0]);

  /*-- Prepare output array ---------------------------------------------------*/
  if (INTEGER (rawchar)[0] > 0)
    {
      ncdata = (char *) RAW (data);
    }
  else
    {
      ncdata = (char *) R_alloc (varsize, sizeof (char));
      for (i = 0; i < tx_num; i++)
	{
	  strncpy (ncdata + i * tx_len, CHAR (STRING_ELT (data, i)), tx_len);
	}
    }

  /*-- Write variable to file -------------------------------------------------*/
  if (varsize > 0)
    {
      /* Some netcdf versions cannot handle zero-sized arrays */
      RNCCHECK (nc_put_vara_text (INTEGER (ncid)[0], INTEGER (varid)[0],
				  s_start, s_count, ncdata));
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_var()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_rename_var (SEXP ncid, SEXP varid, SEXP varname, SEXP nameflag,
		 SEXP newname)
{
  int ncvarid;
  const char *ncvarname, *ncnewname;
  ROBJDEF (NOSXP, 0);

  ncvarname = CHAR (STRING_ELT (varname, 0));
  ncnewname = CHAR (STRING_ELT (newname, 0));
  ncvarid = INTEGER (varid)[0];

  /*-- Get the variable ID if necessary ---------------------------------------*/
  if (INTEGER (nameflag)[0] == 1)
    {
      RNCCHECK (nc_inq_varid (INTEGER (ncid)[0], ncvarname, &ncvarid));
    }

  /*-- Enter define mode ------------------------------------------------------*/
  RNCREDEF (INTEGER (ncid)[0]);

  /*-- Rename the variable ----------------------------------------------------*/
  RNCCHECK (nc_rename_var (INTEGER (ncid)[0], ncvarid, ncnewname));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_grp()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_def_grp (SEXP ncid, SEXP grpname)
{
  ROBJDEF (INTSXP, 1);

  /* Enter define mode */
  RNCREDEF (INTEGER (ncid)[0]);

  /* Define the group */
  RNCCHECK (nc_def_grp (INTEGER (ncid)[0], CHAR (STRING_ELT (grpname, 0)),
			INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_parent()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_parent (SEXP ncid)
{
  ROBJDEF (INTSXP, 1);

  /* Get parent group */
  RNCCHECK (nc_inq_grp_parent (INTEGER (ncid)[0], INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_natts()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_natts (SEXP ncid)
{
  ROBJDEF (INTSXP, 1);

  /* Get number of attributes in group */
  RNCCHECK (nc_inq_natts (INTEGER (ncid)[0], INTEGER (RDATASET)));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grpname()                                                         *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grpname (SEXP ncid, SEXP full)
{
  size_t namelen;
  char *name;
  ROBJDEF (STRSXP, 1);

  if (INTEGER (full)[0])
    {
      RNCCHECK (nc_inq_grpname_full (INTEGER (ncid)[0], &namelen, NULL));

      name = (char *) R_alloc (namelen + 1, sizeof (char));
      RNCCHECK (nc_inq_grpname_full (INTEGER (ncid)[0], NULL, name));

    }
  else
    {
      name = (char *) R_alloc (NC_MAX_NAME + 1, sizeof (char));
      RNCCHECK (nc_inq_grpname (INTEGER (ncid)[0], name));

    }

  SET_STRING_ELT (RDATASET, 0, mkChar (name));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_ncid()                                                        *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_inq_grp_ncid (SEXP ncid, SEXP grpname, SEXP full)
{
  ROBJDEF (INTSXP, 1);

  if (INTEGER (full)[0])
    {
      RNCCHECK (nc_inq_grp_full_ncid (INTEGER (ncid)[0],
				      CHAR (STRING_ELT (grpname, 0)),
				      INTEGER (RDATASET)));
    }
  else
    {
      RNCCHECK (nc_inq_grp_ncid (INTEGER (ncid)[0],
				 CHAR (STRING_ELT (grpname, 0)),
				 INTEGER (RDATASET)));
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  Get lists of ncids for components of a group                               *
\*-----------------------------------------------------------------------------*/

/* Template function returning a list of ncids for a group */
#define INQGRPIDS(RFUN, NCFUN) \
SEXP RFUN (SEXP ncid) \
{ \
  int    count; \
  ROBJDEF(NOSXP,0); \
  RNCCHECK(NCFUN(INTEGER(ncid)[0], &count, NULL)); \
  RDATADEF(INTSXP,count); \
  RNCCHECK(NCFUN(INTEGER(ncid)[0], NULL, INTEGER(RDATASET))); \
  RNCRETURN(NC_NOERR); \
}

INQGRPIDS (R_nc_inq_grps, nc_inq_grps);
INQGRPIDS (R_nc_inq_typeids, nc_inq_typeids);
INQGRPIDS (R_nc_inq_varids, nc_inq_varids);


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dimids()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_dimids (SEXP ncid, SEXP ancestors)
{
  int count;
  ROBJDEF (NOSXP, 0);

  RNCCHECK (nc_inq_dimids (INTEGER (ncid)[0], &count, NULL,
			   INTEGER (ancestors)[0]));
  RDATADEF (INTSXP, count);
  RNCCHECK (nc_inq_dimids (INTEGER (ncid)[0], NULL, INTEGER (RDATASET),
			   INTEGER (ancestors)[0]));

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_unlimids()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP
R_nc_inq_unlimids (SEXP ncid, SEXP ancestors)
{
  int nunlim, *unlimids;
  ROBJDEF (NOSXP, 0);

  RNCCHECK (R_nc_unlimdims (INTEGER (ncid)[0], &nunlim, &unlimids,
			    INTEGER (ancestors)[0]));

  RDATADEF (INTSXP, nunlim);

  /* Sort the results for ease of presentation and searching */
  if (nunlim > 1)
    {
      qsort (unlimids, nunlim, sizeof (int), R_nc_int_cmp);
    }

  /* Copy temporary results to output structure */
  if (nunlim > 0)
    {
      memcpy (INTEGER (RDATASET), unlimids, nunlim * sizeof (int));
    }

  RNCRETURN (NC_NOERR);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_grp()                                                          *
\*-----------------------------------------------------------------------------*/
SEXP
R_nc_rename_grp (SEXP ncid, SEXP grpname)
{
  ROBJDEF (NOSXP, 0);

#if defined HAVE_DECL_NC_RENAME_GRP && HAVE_DECL_NC_RENAME_GRP
  /* Enter define mode */
  RNCREDEF (INTEGER (ncid)[0]);

  /* Rename the group */
  RNCCHECK (nc_rename_grp
	    (INTEGER (ncid)[0], CHAR (STRING_ELT (grpname, 0))));

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
R_ut_calendar (SEXP unitstring, SEXP unitcount, SEXP values)
{
  int year, month, day, hour, minute, count, i, status;
  float second;
  double utvalue;
  char strerror[64];
  utUnit utunit;
  ROBJDEF (REALSXP, INTEGER (unitcount)[0] * 6);

  /*-- Scan unitstring --------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
  utIni (&utunit);
#endif

  status = utScan (CHAR (STRING_ELT (unitstring, 0)), &utunit);
  if (status != 0)
    {
      goto cleanup;
    }

  /*-- Check if unit is time and has origin -----------------------------------*/
  if (!utIsTime (&utunit))
    {
      status = UT_ENOTTIME;
      goto cleanup;
    }

  if (!utHasOrigin (&utunit))
    {
      status = UT_EINVALID;
      goto cleanup;
    }

  /*-- Convert values ---------------------------------------------------------*/
  count = (int) INTEGER (unitcount)[0];
  for (i = 0; i < count; i++)
    {
      utvalue = (double) REAL (values)[i];
      status = utCalendar (utvalue, &utunit, &year, &month, &day,
			   &hour, &minute, &second);
      if (status != 0)
	{
	  goto cleanup;
	}

      REAL (RDATASET)[i + 0 * count] = (double) year;
      REAL (RDATASET)[i + 1 * count] = (double) month;
      REAL (RDATASET)[i + 2 * count] = (double) day;
      REAL (RDATASET)[i + 3 * count] = (double) hour;
      REAL (RDATASET)[i + 4 * count] = (double) minute;
      REAL (RDATASET)[i + 5 * count] = (double) second;
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
R_ut_inv_calendar (SEXP unitstring, SEXP unitcount, SEXP values)
{
  int year, month, day, hour, minute, count, i, status;
  float second;
  double utvalue;
  char strerror[64];
  utUnit utunit;
  ROBJDEF (NOSXP, 0);

  /*-- Create output object and initialize return values ----------------------*/
  count = (int) INTEGER (unitcount)[0];
  count = count / 6;
  RDATADEF (REALSXP, count);

  /*-- Scan unitstring --------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
  utIni (&utunit);
#endif

  status = utScan (CHAR (STRING_ELT (unitstring, 0)), &utunit);
  if (status != 0)
    {
      goto cleanup;
    }

  /*-- Check if unit is time and has origin -----------------------------------*/
  if (!utIsTime (&utunit))
    {
      status = UT_ENOTTIME;
      goto cleanup;
    }

  if (!utHasOrigin (&utunit))
    {
      status = UT_EINVALID;
      goto cleanup;
    }

  /*-- Convert values ---------------------------------------------------------*/
  for (i = 0; i < count; i++)
    {
      year = (int) REAL (values)[i + 0 * count];
      month = (int) REAL (values)[i + 1 * count];
      day = (int) REAL (values)[i + 2 * count];
      hour = (int) REAL (values)[i + 3 * count];
      minute = (int) REAL (values)[i + 4 * count];
      second = (double) REAL (values)[i + 5 * count];

      status = utInvCalendar (year, month, day, hour, minute, second,
			      &utunit, &utvalue);
      if (status != 0)
	{
	  goto cleanup;
	}

      REAL (RDATASET)[i] = utvalue;
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
