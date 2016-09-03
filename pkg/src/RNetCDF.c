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
  SET_VECTOR_ELT(retlist, 0, allocVector(INTSXP, 1)); \
  SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP, 1)); \
  RDATADEF(RTYPE,RLEN);

#define RDATASET VECTOR_ELT(retlist,2)

#define RRETURN(STATUS) \
  if (STATUS == E_UNSUPPORTED) { \
    SET_VECTOR_ELT(retlist, 1, \
      mkString("Operation requires RNetCDF built with newer netcdf library")); \
  } else if (STATUS != NC_NOERR) { \
    SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(STATUS))); \
  } \
  INTEGER(VECTOR_ELT(retlist, 0))[0] = STATUS; \
  UNPROTECT(1); \
  return(retlist);

#define RUTRETURN(STATUS) \
  if (STATUS != 0) { \
    SET_VECTOR_ELT(retlist, 1, mkString(R_ut_strerror(STATUS))); \
  } \
  INTEGER(VECTOR_ELT(retlist, 0))[0] = STATUS; \
  UNPROTECT(1); \
  return(retlist);


/*=============================================================================*\
 *  Reusable internal functions
\*=============================================================================*/

/* Convert netcdf type code to string label.
   Argument str is assumed to be pre-allocated.
   Returns NC_NOERR if ok, NC_EBADTYPE otherwise.
 */
static int R_nc_type2str(int ncid, nc_type xtype, char *str) {
  switch (xtype) {
    case NC_BYTE:
      strcpy(str, "NC_BYTE");
      break;
    case NC_UBYTE:
      strcpy(str, "NC_UBYTE");
      break;
    case NC_CHAR:
      strcpy(str, "NC_CHAR");
      break;
    case NC_SHORT:
      strcpy(str, "NC_SHORT");
      break;
    case NC_USHORT:
      strcpy(str, "NC_USHORT");
      break;
    case NC_INT:
      strcpy(str, "NC_INT");
      break;
    case NC_UINT:
      strcpy(str, "NC_UINT");
      break;
    case NC_INT64:
      strcpy(str, "NC_INT64");
      break;
    case NC_UINT64:
      strcpy(str, "NC_UINT64");
      break;
    case NC_FLOAT:
      strcpy(str, "NC_FLOAT");
      break;
    case NC_DOUBLE:
      strcpy(str, "NC_DOUBLE");
      break;
    case NC_STRING:
      strcpy(str, "NC_STRING");
      break;
    default:
      /* Try to get name of a user defined type */
      return nc_inq_user_type(ncid, xtype, str, NULL, NULL, NULL, NULL);
  }
  return NC_NOERR;
}


/* Convert netcdf string label to type code.
   Return NC_NOERR if ok, NC_EBADTYPE otherwise.
 */
static int R_nc_str2type(int ncid, const char *str, nc_type *xtype) {
  int typelen; 
  typelen = strlen(str);
  *xtype = NC_NAT;
  if (typelen >= 6) {
    switch (str[3]) {
      case 'B':
	if (strcmp(str, "NC_BYTE") == 0)
	  *xtype = NC_BYTE;
	break;
      case 'C':
	if (strcmp(str, "NC_CHAR") == 0)
	  *xtype = NC_CHAR;
	break;
      case 'D':
	if (strcmp(str, "NC_DOUBLE") == 0)
	  *xtype = NC_DOUBLE;
	break;
      case 'F':
	if (strcmp(str, "NC_FLOAT") == 0)
	  *xtype = NC_FLOAT;
	break;
      case 'I':
	switch (str[6]) {
	  case '\0':
	    if (strcmp(str, "NC_INT") == 0)
	      *xtype = NC_INT;
	    break;
	  case '6':
	    if (strcmp(str, "NC_INT64") == 0)
	      *xtype = NC_INT64;
	    break;
	 }
	 break;
       case 'L':
	 if (strcmp(str, "NC_LONG") == 0)
	   *xtype = NC_LONG;
	 break;
       case 'S':
	 switch (str[4]) {
	   case 'H':
	     if (strcmp(str, "NC_SHORT") == 0)
	       *xtype = NC_SHORT;
	     break;
	   case 'T':
	     if (strcmp(str, "NC_STRING") == 0)
	       *xtype = NC_STRING;
	     break;
	 }
         break;
      case 'U':
	if (typelen >= 7) {
	  switch (str[7]) {
	    case '\0':
	      if (strcmp(str, "NC_UINT") == 0)
		*xtype = NC_UINT;
	      break;
	    case '6':
	      if (strcmp(str, "NC_UINT64") == 0)
		*xtype = NC_UINT64;
	      break;
	    case 'E':
	      if (strcmp(str, "NC_UBYTE") == 0)
		*xtype = NC_UBYTE;
	      break;
	    case 'R':
	      if (strcmp(str, "NC_USHORT") == 0)
		*xtype = NC_USHORT;
	      break;
	  }
	}
        break;
    }
  }

  if (*xtype == NC_NAT) {
    /* Try to get id of a user defined type */
    return nc_inq_typeid(ncid, str, xtype);
  } else {
    return NC_NOERR;
  }
}


/* Private function called by qsort to compare integers */
static int R_nc_int_cmp(const void *a, const void *b)
{
   const int *ia = (const int *)a; 
   const int *ib = (const int *)b;
   return *ia  - *ib; 
}


/* Convert udunits error code to a string */
static const char* R_ut_strerror (int errcode)
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


/*=============================================================================*\
 *  NetCDF library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_nc_copy_att()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_copy_att (SEXP ncid_in, SEXP varid_in, SEXP globflag_in, SEXP attname,
                    SEXP ncid_out, SEXP varid_out, SEXP globflag_out)
{
    int  ncvarid_in, ncvarid_out, status;
    char ncattname[NC_MAX_NAME+1];
    ROBJDEF(NOSXP, 0);

    /*-- Check if handling global attributes ----------------------------------*/
    if(INTEGER(globflag_in)[0] == 1)
        ncvarid_in = NC_GLOBAL;
    else
        ncvarid_in = INTEGER(varid_in)[0];

    if(INTEGER(globflag_out)[0] == 1)
        ncvarid_out = NC_GLOBAL;
    else
        ncvarid_out = INTEGER(varid_out)[0];

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid_out)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        RRETURN(status);
    }

    /*-- Copy the attribute ---------------------------------------------------*/
    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));
    status = nc_copy_att(INTEGER(ncid_in)[0], ncvarid_in, ncattname,
                         INTEGER(ncid_out)[0], ncvarid_out);
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_delete_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_delete_att (SEXP ncid, SEXP varid, SEXP globflag, SEXP attname)
{
    int  ncvarid, status;
    char ncattname[NC_MAX_NAME+1];
    ROBJDEF(NOSXP,0);
    
    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        RRETURN(status);
    }

    /*-- Delete the attribute -------------------------------------------------*/
    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));
    status = nc_del_att(INTEGER(ncid)[0], ncvarid, ncattname);
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_get_att (SEXP ncid, SEXP varid, SEXP name, 
                   SEXP numflag, SEXP globflag)
{
    int    ncvarid, status;
    char   ncattname[NC_MAX_NAME+1], *cvalue;    
    size_t attlen;
    ROBJDEF(NOSXP,0);

    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Get the attribute's length -------------------------------------------*/
    strcpy(ncattname, CHAR(STRING_ELT(name, 0)));
    status = nc_inq_attlen(INTEGER(ncid)[0], ncvarid, ncattname, &attlen);
    if(status != NC_NOERR) {
        RRETURN(status);
    }
    
    /*-- Get the attribute ----------------------------------------------------*/
    if(INTEGER(numflag)[0] == 1) {
        RDATADEF(REALSXP, attlen);
        status = nc_get_att_double(INTEGER(ncid)[0], ncvarid, ncattname, REAL(RDATASET));
	if(status != NC_NOERR) {
            RRETURN(status);
	}
    
    } else {
        RDATADEF(STRSXP, 1);
        cvalue = (char *) R_alloc(attlen+1, sizeof(char));
	status = nc_get_att_text(INTEGER(ncid)[0], ncvarid, ncattname, cvalue);
	cvalue[attlen+1] = '\0';
	if(status != NC_NOERR) {
            RRETURN(status);
	}
        SET_STRING_ELT(RDATASET, 0, mkChar(cvalue));
    }
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_att (SEXP ncid, SEXP varid, SEXP attname, SEXP attid,
                   SEXP nameflag, SEXP globflag)
{
    int     ncvarid, ncattid, status;
    char    atttype[NC_MAX_NAME+1], ncattname[NC_MAX_NAME+1];
    size_t  ncattlen;
    nc_type xtype;
    ROBJDEF(VECSXP,4);

    ncattid    = INTEGER(attid)[0];
    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));

    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Get the attribute ID if necessary ------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_attid(INTEGER(ncid)[0], ncvarid, ncattname, &ncattid);
        if(status != NC_NOERR) {
            RRETURN(status);
	}
    }

    /*-- Get the attribute name if necessary ----------------------------------*/
    if(INTEGER(nameflag)[0] == 0) {
 	status = nc_inq_attname(INTEGER(ncid)[0], ncvarid, ncattid, ncattname);
	if(status != NC_NOERR) {
            RRETURN(status);
	}
    }

    /*-- Inquire the attribute ------------------------------------------------*/
    status = nc_inq_att(INTEGER(ncid)[0], ncvarid, ncattname, &xtype, 
        &ncattlen);
    if(status != NC_NOERR) {
        RRETURN(status);
    }

    /*-- Convert nc_type to char ----------------------------------------------*/
    status = R_nc_type2str(INTEGER(ncid)[0], xtype, atttype);
    if(status != NC_NOERR) {
        RRETURN(status);
    }
 
    /*-- Returning the list ---------------------------------------------------*/
    SET_VECTOR_ELT(RDATASET, 0, allocVector(INTSXP, 1));
    SET_VECTOR_ELT(RDATASET, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(RDATASET, 2, allocVector(STRSXP, 1));
    SET_VECTOR_ELT(RDATASET, 3, allocVector(INTSXP, 1));
    
    INTEGER(VECTOR_ELT(RDATASET, 0))[0] = ncattid;
    SET_VECTOR_ELT(RDATASET, 1, mkString(ncattname));
    SET_VECTOR_ELT(RDATASET, 2, mkString(atttype));
    INTEGER(VECTOR_ELT(RDATASET, 3))[0] = (int) ncattlen;

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_put_att (SEXP ncid, SEXP varid, SEXP name, SEXP type, SEXP attlen,
                   SEXP numflag, SEXP globflag, SEXP value)
{
    int     ncvarid, ncattlen, status;
    char    ncattname[NC_MAX_NAME+1];
    nc_type xtype;
    ROBJDEF(NOSXP,0);

    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Convert char to nc_type ----------------------------------------------*/
    status = R_nc_str2type(INTEGER(ncid)[0], CHAR(STRING_ELT(type, 0)), &xtype);
    if (status != NC_NOERR) {
        RRETURN(status);
    }

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        RRETURN(status);
    }

    /*-- Create the attribute -------------------------------------------------*/
    ncattlen  = INTEGER(attlen)[0];
    strcpy(ncattname, CHAR(STRING_ELT(name, 0)));
    if(INTEGER(numflag)[0] == 1) {
        status = nc_put_att_double(INTEGER(ncid)[0], ncvarid, ncattname, xtype,
	    ncattlen, REAL(value));
    } else {
        ncattlen = strlen(CHAR(STRING_ELT(value, 0)));
	status = nc_put_att_text(INTEGER(ncid)[0], ncvarid, ncattname,
	    ncattlen, CHAR(STRING_ELT(value, 0)));
    }
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_rename_att (SEXP ncid, SEXP varid, SEXP globflag, SEXP attname,
                      SEXP newname)
{
    int  ncvarid, status;
    char ncattname[NC_MAX_NAME+1], ncnewname[NC_MAX_NAME+1];
    ROBJDEF(NOSXP,0);
    
    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        RRETURN(status);
    }

    /*-- Rename the attribute -------------------------------------------------*/
    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));
    strcpy(ncnewname, CHAR(STRING_ELT(newname, 0)));
    status = nc_rename_att(INTEGER(ncid)[0], ncvarid, ncattname, ncnewname);
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_close()                                                               *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_close (SEXP ptr)
{
    int  status, *fileid;
    ROBJDEF(NOSXP,0);

    fileid = R_ExternalPtrAddr(ptr);
    if(!fileid) {
      RRETURN(NC_NOERR);
    }

    status = nc_close(*fileid);
    if (status == NC_NOERR) {
      R_Free(fileid);
      R_ClearExternalPtr(ptr);
    }
    RRETURN(status);
}

/* Private function used as finalizer during garbage collection.
   It is required to have no return value. */
static void R_nc_finalizer(SEXP ptr)
{
  R_nc_close(ptr);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_create()                                                              *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_create (SEXP filename, SEXP clobber, SEXP share, SEXP prefill,
                  SEXP format)
{
    int  cmode, fillmode, old_fillmode, ncid, status, *fileid;
    SEXP Rptr;
    ROBJDEF(INTSXP,1);

    /*-- Determine the cmode --------------------------------------------------*/
    if(INTEGER(clobber)[0] == 0)
        cmode = NC_NOCLOBBER;
    else
        cmode = NC_CLOBBER;

    /*-- Determine which buffer scheme shall be used --------------------------*/
    if(INTEGER(share)[0] != 0)
        cmode = cmode | NC_SHARE;

    /*-- Determine the fillmode -----------------------------------------------*/
    if(INTEGER(prefill)[0] == 0)
        fillmode = NC_NOFILL;
    else
        fillmode = NC_FILL;

    /*-- Set file format ------------------------------------------------------*/
    switch (INTEGER(format)[0])
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

    /*-- Create the file ------------------------------------------------------*/
    status = nc_create(R_ExpandFileName(CHAR(STRING_ELT(filename, 0))), 
        cmode, &ncid);
    if (status != NC_NOERR) {
        RRETURN(status);
    }
    INTEGER(RDATASET)[0] = ncid;

    /*-- Arrange for file to be closed if handle is garbage collected ---------*/
    fileid = R_Calloc(1, int);
    *fileid = ncid;
    Rptr = R_MakeExternalPtr(fileid, R_NilValue, R_NilValue);
    PROTECT(Rptr);
    R_RegisterCFinalizerEx(Rptr, &R_nc_finalizer, TRUE);
    setAttrib(RDATASET, install("handle_ptr"), Rptr);
    UNPROTECT(1);

    /*-- Set the fill mode ----------------------------------------------------*/
    status = nc_set_fill(ncid, fillmode, &old_fillmode);

    RRETURN(status); 
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_def_dim (SEXP ncid, SEXP dimname, SEXP size, SEXP unlimp)
{
    int  status;
    ROBJDEF(INTSXP,1);

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
      RRETURN(status);
    }

    /*-- Create the dimension -------------------------------------------------*/
    if(INTEGER(unlimp)[0] == 1) {
      status = nc_def_dim(INTEGER(ncid)[0], CHAR(STRING_ELT(dimname, 0)), 
	         NC_UNLIMITED, INTEGER(RDATASET));
    } else {
      status = nc_def_dim(INTEGER(ncid)[0], CHAR(STRING_ELT(dimname, 0)), 
	         INTEGER(size)[0], INTEGER(RDATASET));
    }

    RRETURN(status);
}


/* Private function to find all unlimited dimensions visible in a file or group.
   The netcdf4 function nc_inq_unlimdims does not check ancestors of a group.
   Returns netcdf status. If no error occurs, nunlim and unlimids are set.
 */
static int R_nc_unlimdims (int ncid, int *nunlim, int **unlimids, int ancestors) {
  int status, format, ndims, ntmp, *tmpdims;

  *nunlim = 0;

  status = nc_inq_format(ncid, &format);
  if (status != NC_NOERR) {
    return status;
  }

  if (format == NC_FORMAT_NETCDF4) {
    status = nc_inq_dimids(ncid, &ndims, NULL, 1);
    if(status != NC_NOERR) {
      return(status);
    }

    /* At most, all visible dimensions could be unlimited */
    *unlimids = (int *) R_alloc(ndims, sizeof(int));
    tmpdims = (int *) R_alloc(ndims, sizeof(int));

    /* Get unlimited dimensions in this group and (optionally) its ancestors */
    do {
      status = nc_inq_unlimdims(ncid, &ntmp, tmpdims);
      if (status != NC_NOERR) {
	return status;
      }
      if ((ntmp + *nunlim) <= ndims) {
        memcpy(*unlimids + *nunlim*sizeof(int), tmpdims, ntmp*sizeof(int));
        *nunlim += ntmp;
      } else {
        /* Avoid a segfault in case nc_inq_unlimdims starts checking ancestors */
        return NC_ENOMEM;
      }
    } while (ancestors && nc_inq_grp_parent(ncid, &ncid) == NC_NOERR);

  } else {
    *unlimids = (int *) R_alloc(1, sizeof(int));
    status = nc_inq_unlimdim(ncid, *unlimids);
    if (status == NC_NOERR && **unlimids != -1) {
      *nunlim = 1;
    }
  }

  return status;
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_dim (SEXP ncid, SEXP dimid, SEXP dimname, SEXP nameflag)
{
    int    nunlim, *unlimids, isunlim, ncdimid, status, ii;
    size_t ncdimlen;
    char   ncdimname[NC_MAX_NAME+1];
    ROBJDEF(VECSXP, 4);

    /*-- Get the dimension ID if necessary ------------------------------------*/
    if(INTEGER(nameflag)[0] != 0) {
      status = nc_inq_dimid(INTEGER(ncid)[0], CHAR(STRING_ELT(dimname, 0)), &ncdimid);
      if(status != NC_NOERR) {
	RRETURN(status);
      }
    } else {
      ncdimid = INTEGER(dimid)[0];
    }

    /*-- Inquire the dimension ------------------------------------------------*/
    status = nc_inq_dim(INTEGER(ncid)[0], ncdimid, ncdimname, &ncdimlen);
    if(status != NC_NOERR) {
      RRETURN(status);
    }

    /*-- Check if it is an unlimited dimension -------------------------------*/
    status = R_nc_unlimdims (INTEGER(ncid)[0], &nunlim, &unlimids, 1);
    if (status != NC_NOERR) {
      RRETURN(status);
    }

    isunlim = 0;
    for ( ii=0; ii<nunlim; ii++ ) {
      if (unlimids[ii] == ncdimid) {
        isunlim = 1;
        break;
      }
    }

    /*-- Returning the list ---------------------------------------------------*/
    SET_VECTOR_ELT(RDATASET, 0, allocVector(INTSXP, 1));
    SET_VECTOR_ELT(RDATASET, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(RDATASET, 2, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(RDATASET, 3, allocVector(INTSXP, 1));

    INTEGER(VECTOR_ELT(RDATASET, 0))[0] = ncdimid;
    SET_VECTOR_ELT (RDATASET, 1, mkString(ncdimname));
    REAL(VECTOR_ELT(RDATASET, 2))[0] = (double)ncdimlen;
    INTEGER(VECTOR_ELT(RDATASET, 3))[0] = isunlim;

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_dim()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_rename_dim (SEXP ncid, SEXP dimid, SEXP dimname, SEXP nameflag,
                      SEXP newname)
{
    int  ncdimid, status;
    char ncdimname[NC_MAX_NAME+1], ncnewname[NC_MAX_NAME+1];
    ROBJDEF(NOSXP,0);

    ncdimid   = INTEGER(dimid)[0];
    strcpy(ncdimname, CHAR(STRING_ELT(dimname, 0)));
    strcpy(ncnewname, CHAR(STRING_ELT(newname, 0)));
    
    /*-- Get the dimension ID if necessary ------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_dimid(INTEGER(ncid)[0], ncdimname, &ncdimid);
	if(status != NC_NOERR) {
            RRETURN(status);
	}
    }

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        RRETURN(status);
    }

    /*-- Rename the dimension -------------------------------------------------*/
    status = nc_rename_dim(INTEGER(ncid)[0], ncdimid, ncnewname);
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_file (SEXP ncid)
{
    int  ndims, nvars, ngatts, unlimdimid, status;
    ROBJDEF(VECSXP,4);

    /*-- Inquire about the NetCDF dataset -------------------------------------*/
    status = nc_inq(INTEGER(ncid)[0], &ndims, &nvars, &ngatts, &unlimdimid);
    if(status != NC_NOERR) {
      RRETURN(status);
    }

    /*-- Returning the list ---------------------------------------------------*/
    SET_VECTOR_ELT(RDATASET, 0, allocVector(INTSXP, 1));
    SET_VECTOR_ELT(RDATASET, 1, allocVector(INTSXP, 1));
    SET_VECTOR_ELT(RDATASET, 2, allocVector(INTSXP, 1));
    SET_VECTOR_ELT(RDATASET, 3, allocVector(INTSXP, 1));

    INTEGER(VECTOR_ELT(RDATASET, 0))[0] = ndims;
    INTEGER(VECTOR_ELT(RDATASET, 1))[0] = nvars;
    INTEGER(VECTOR_ELT(RDATASET, 2))[0] = ngatts;
    INTEGER(VECTOR_ELT(RDATASET, 3))[0] = unlimdimid;

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_open()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill)
{
    int  ncid, omode, fillmode, old_fillmode, status, *fileid;
    SEXP Rptr;
    ROBJDEF(INTSXP,1);

    /*-- Determine the omode --------------------------------------------------*/
    if(INTEGER(write)[0] == 0)
        omode = NC_NOWRITE;
    else
        omode = NC_WRITE;

    if(INTEGER(share)[0] != 0)
        omode = omode | NC_SHARE;

    /*-- Determine the fillmode -----------------------------------------------*/
    if(INTEGER(prefill)[0] == 0)
        fillmode = NC_NOFILL;
    else
        fillmode = NC_FILL;

    /*-- Open the file --------------------------------------------------------*/
    status = nc_open(R_ExpandFileName(CHAR(STRING_ELT(filename, 0))),
        omode, &ncid);
    if (status != NC_NOERR) {
      RRETURN(status);
    }
    INTEGER(RDATASET)[0] = ncid;

    /*-- Arrange for file to be closed if handle is garbage collected ---------*/
    fileid = R_Calloc(1, int);
    *fileid = ncid;
    Rptr = R_MakeExternalPtr(fileid, R_NilValue, R_NilValue);
    PROTECT(Rptr);
    R_RegisterCFinalizerEx(Rptr, &R_nc_finalizer, TRUE);
    setAttrib(RDATASET, install("handle_ptr"), Rptr);
    UNPROTECT(1);

    /*-- Set the fill mode ----------------------------------------------------*/
    if(INTEGER(write)[0] != 0) {
        status = nc_set_fill(ncid, fillmode, &old_fillmode);
    }

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_sync()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_sync (SEXP ncid)
{
    int  status;
    ROBJDEF(NOSXP,0);

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        RRETURN(status);
    }

    /*-- Sync the file --------------------------------------------------------*/
    status = nc_sync(INTEGER(ncid)[0]);
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_def_var (SEXP ncid, SEXP varname, SEXP type, SEXP ndims, SEXP dimids)
{
    int     status;
    nc_type xtype;
    ROBJDEF(INTSXP,1);

    /*-- Convert char to nc_type ----------------------------------------------*/
    status = R_nc_str2type(INTEGER(ncid)[0], CHAR(STRING_ELT(type, 0)), &xtype);
    if (status != NC_NOERR) {
      RRETURN(status);
    }
    
    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
      RRETURN(status);
    }

    /*-- Define the variable --------------------------------------------------*/
    status = nc_def_var(INTEGER(ncid)[0], CHAR(STRING_ELT(varname, 0)), xtype,
        INTEGER(ndims)[0], INTEGER(dimids), INTEGER(RDATASET));

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_vara_double()                                                     *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_get_vara_double (SEXP ncid, SEXP varid, SEXP start, 
                           SEXP count, SEXP ndims)
{
    int    i, status;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS], varsize;
    ROBJDEF(NOSXP,0);

    /*-- Copy dims from int to size_t, calculate total array size -------------*/
    varsize = 1;
    for(i=0; i<INTEGER(ndims)[0]; i++) {
	s_start[i] = (size_t)INTEGER(start)[i];
	s_count[i] = (size_t)INTEGER(count)[i];
	varsize *= s_count[i];
    }

    RDATADEF(REALSXP,varsize);	

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
      RRETURN(status);
    }

    /*-- Read variable from file ----------------------------------------------*/
    if (varsize > 0) {
      /* Some netcdf versions cannot handle zero-sized arrays */
      status = nc_get_vara_double(INTEGER(ncid)[0], INTEGER(varid)[0],
        s_start, s_count, REAL(RDATASET));
    } else {
      status = NC_NOERR;
    }

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_vara_text()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_get_vara_text (SEXP ncid, SEXP varid, SEXP start, 
                         SEXP count, SEXP ndims, SEXP rawchar)
{
    int    i, status;
    char   *data, *tx_str;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
    size_t tx_len, tx_num, varsize;
    ROBJDEF(NOSXP,0);

    /*-- Copy dims from int to size_t, calculate number and length of strings -*/
    for(i=0; i<INTEGER(ndims)[0]; i++) {
	s_start[i] = (size_t)INTEGER(start)[i];
	s_count[i] = (size_t)INTEGER(count)[i];
    }

    if(INTEGER(ndims)[0] > 0) {
        tx_num = 1;
	for(i=0; i<INTEGER(ndims)[0]-1; i++) {
            tx_num *= s_count[i];
	}
        tx_len = s_count[INTEGER(ndims)[0]-1];
    } else {
        tx_num = 1;
        tx_len = 1;
    }
    varsize = tx_num*tx_len;

    /*-- Create output object -------------------------------------------------*/
    if (INTEGER(rawchar)[0] > 0) {
      RDATADEF(RAWSXP,varsize);
    } else {
      RDATADEF(STRSXP,tx_num);
    }

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
      RRETURN(status);
    }

    /*-- Read variable from file ----------------------------------------------*/
    if (INTEGER(rawchar)[0] > 0) {
      data = (char *) RAW(RDATASET);
    } else {
      data = (char *) R_alloc(varsize, sizeof(char));
    }

    if (varsize > 0) {
      /* Some netcdf versions cannot handle zero-sized arrays */
      status = nc_get_vara_text(INTEGER(ncid)[0], INTEGER(varid)[0],
        s_start, s_count, data);
    } else {
      status = NC_NOERR;
    }

    /*-- Copy from C to R character vector (if specified) ---------------------*/
    if (status == NC_NOERR && INTEGER(rawchar)[0] <= 0) {
      tx_str = (char *) R_alloc(tx_len+1, sizeof(char));
      tx_str[tx_len] = '\0'; /* Final null character is never modified */
      for(i=0; i<tx_num; i++) {
        strncpy(tx_str, data+i*tx_len, tx_len);
	SET_STRING_ELT(RDATASET, i, mkChar(tx_str));
      }
    }

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_var (SEXP ncid, SEXP varid, SEXP varname, SEXP nameflag)
{
    int     ncvarid, ndims, natts, i, status, *dimids;
    char    ncvarname[NC_MAX_NAME+1], vartype[NC_MAX_NAME+1];
    nc_type xtype;
    ROBJDEF(VECSXP,6);

    ncvarid = INTEGER(varid)[0];
    strcpy(ncvarname, CHAR(STRING_ELT(varname, 0)));

    /*-- Get the variable ID if necessary -------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_varid(INTEGER(ncid)[0], ncvarname, &ncvarid);
        if(status != NC_NOERR) {
            RRETURN(status);
	}
    }

    /*-- Get the number of dimensions -----------------------------------------*/
    status = nc_inq_varndims(INTEGER(ncid)[0], ncvarid, &ndims);
    if(status != NC_NOERR) {
        RRETURN(status);
    }

    if(ndims == 0) {
        dimids = NULL;
    } else {
    	SET_VECTOR_ELT(RDATASET, 4, allocVector(INTSXP, ndims));
	dimids = INTEGER(VECTOR_ELT(RDATASET, 4));
    }

    /*-- Inquire the variable -------------------------------------------------*/
    status = nc_inq_var(INTEGER(ncid)[0], ncvarid, ncvarname, &xtype, &ndims,
        dimids, &natts);
    if (status != NC_NOERR) {
        RRETURN(status);
    }

    /*-- Convert nc_type to char ----------------------------------------------*/
    status = R_nc_type2str(INTEGER(ncid)[0], xtype, vartype);
    if (status != NC_NOERR) {
        RRETURN(status);
    }

    /*-- Returning the list ---------------------------------------------------*/
    SET_VECTOR_ELT(RDATASET, 0, allocVector(INTSXP, 1));
    SET_VECTOR_ELT(RDATASET, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(RDATASET, 2, allocVector(STRSXP, 1));
    SET_VECTOR_ELT(RDATASET, 3, allocVector(INTSXP, 1));
    /* List item 4 is already allocated */
    SET_VECTOR_ELT(RDATASET, 5, allocVector(INTSXP, 1));
    
    INTEGER(VECTOR_ELT(RDATASET, 0))[0] = ncvarid;
    SET_VECTOR_ELT (RDATASET, 1, mkString(ncvarname));
    SET_VECTOR_ELT (RDATASET, 2, mkString(vartype));
    INTEGER(VECTOR_ELT(RDATASET, 3))[0] = ndims;
    /* List item 4 is defined by nc_inq_var above */
    INTEGER(VECTOR_ELT(RDATASET, 5))[0] = natts;

    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_vara_double()                                                     *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_put_vara_double (SEXP ncid, SEXP varid, SEXP start, 
                           SEXP count, SEXP ndims, SEXP data)
{
    int    i, status;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS], varsize;
    ROBJDEF(NOSXP,0);

    /*-- Copy dims from int to size_t -----------------------------------------*/
    varsize = 1;
    for(i=0; i<INTEGER(ndims)[0]; i++) {
	s_start[i] = (size_t)INTEGER(start)[i];
	s_count[i] = (size_t)INTEGER(count)[i];
        varsize *= s_count[i];
    }

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        RRETURN(status);
    }

    /*-- Put the var ----------------------------------------------------------*/
    if (varsize > 0) {
      /* Some netcdf versions cannot handle zero-sized arrays */
      status = nc_put_vara_double(INTEGER(ncid)[0], INTEGER(varid)[0],
        s_start, s_count, REAL(data));
    } else {
      status = NC_NOERR;
    }
    RRETURN(status);

}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_vara_text()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_put_vara_text (SEXP ncid, SEXP varid, SEXP start, 
                         SEXP count, SEXP ndims, SEXP rawchar, SEXP data)
{
    int    i, status;
    char   *ncdata;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
    size_t tx_len, tx_num, varsize;
    ROBJDEF(NOSXP,0);

    /*-- Copy dims from int to size_t, calculate number and length of strings -*/
    for(i=0; i<INTEGER(ndims)[0]; i++) {
	s_start[i] = (size_t)INTEGER(start)[i];
	s_count[i] = (size_t)INTEGER(count)[i];
    }

    if (INTEGER(ndims)[0] > 0) {
	tx_num = 1;
	for(i=0; i<INTEGER(ndims)[0]-1; i++) {
	    tx_num *= s_count[i];
	}
	tx_len = s_count[INTEGER(ndims)[0]-1];
    } else {
	tx_num = 1;
	tx_len = 1;
    }
    varsize = tx_num*tx_len;

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        RRETURN(status);
    }

    /*-- Prepare output array -------------------------------------------------*/
    if (INTEGER(rawchar)[0] > 0) {
      ncdata = (char *) RAW(data);
    } else {
      ncdata = (char *) R_alloc(varsize, sizeof(char));
      for(i=0; i<tx_num; i++) {
	  strncpy(ncdata+i*tx_len, CHAR(STRING_ELT(data, i)), tx_len);
      }
    }
 
    /*-- Write variable to file -----------------------------------------------*/
    if (varsize > 0) {
      /* Some netcdf versions cannot handle zero-sized arrays */
      status = nc_put_vara_text(INTEGER(ncid)[0], INTEGER(varid)[0],
	  s_start, s_count, ncdata);
    } else {
      status = NC_NOERR;
    }
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_var()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_rename_var (SEXP ncid, SEXP varid, SEXP varname, SEXP nameflag,
                      SEXP newname)
{
    int  ncvarid, status;
    char ncvarname[NC_MAX_NAME+1], ncnewname[NC_MAX_NAME+1];
    ROBJDEF(NOSXP,0);

    strcpy(ncvarname, CHAR(STRING_ELT(varname, 0)));
    strcpy(ncnewname, CHAR(STRING_ELT(newname, 0)));
    ncvarid = INTEGER(varid)[0];
   
    /*-- Get the variable ID if necessary -------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_varid(INTEGER(ncid)[0], ncvarname, &ncvarid);
	if(status != NC_NOERR) {
            RRETURN(status);
	}
    }

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        RRETURN(status);
    }

    /*-- Rename the variable --------------------------------------------------*/
    status = nc_rename_var(INTEGER(ncid)[0], ncvarid, ncnewname);
    RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_grp()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_def_grp (SEXP ncid, SEXP grpname)
{
  int     status;
  ROBJDEF(INTSXP,1);

  /* Enter define mode */
  status = nc_redef(INTEGER(ncid)[0]);
  if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
    RRETURN(status);
  }

  /* Define the group */
  status = nc_def_grp(INTEGER(ncid)[0], CHAR(STRING_ELT(grpname, 0)),
	     INTEGER(RDATASET));
  RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_parent()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP R_nc_inq_grp_parent (SEXP ncid)
{
  int     status;
  ROBJDEF(INTSXP,1);

  /* Get parent group */
  status = nc_inq_grp_parent (INTEGER(ncid)[0], 
	     INTEGER(RDATASET));
  RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_natts()                                                      *
\*-----------------------------------------------------------------------------*/
SEXP R_nc_inq_natts (SEXP ncid)
{
  int     status;
  ROBJDEF(INTSXP,1);

  /* Get number of attributes in group */
  status = nc_inq_natts(INTEGER(ncid)[0], INTEGER(RDATASET));

  RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grpname()                                                         *
\*-----------------------------------------------------------------------------*/
SEXP R_nc_inq_grpname (SEXP ncid, SEXP full)
{
  int     status;
  size_t  namelen;
  char    *name; 
  ROBJDEF(STRSXP,1);

  if (INTEGER(full)[0]) {
    status = nc_inq_grpname_full(INTEGER(ncid)[0],  &namelen, NULL);
    if (status != NC_NOERR) {
      RRETURN(status);
    }

    name = (char *) R_alloc(namelen+1, sizeof(char));
    status = nc_inq_grpname_full(INTEGER(ncid)[0], NULL, name);

  } else {
    name = (char *) R_alloc(NC_MAX_NAME+1, sizeof(char));
    status = nc_inq_grpname(INTEGER(ncid)[0], name);

  }

  if (status == NC_NOERR) {
    SET_STRING_ELT(RDATASET, 0, mkChar(name));
  }
  RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_grp_ncid()                                                        *
\*-----------------------------------------------------------------------------*/
SEXP R_nc_inq_grp_ncid (SEXP ncid, SEXP grpname, SEXP full)
{
  int     status;
  ROBJDEF(INTSXP,1);

  if (INTEGER(full)[0]) {
    status = nc_inq_grp_full_ncid(INTEGER(ncid)[0],
	       CHAR(STRING_ELT(grpname, 0)), INTEGER(RDATASET));
  } else {
    status = nc_inq_grp_ncid(INTEGER(ncid)[0],
	       CHAR(STRING_ELT(grpname, 0)), INTEGER(RDATASET));
  }
  RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  Get lists of ncids for components of a group                               *
\*-----------------------------------------------------------------------------*/

/* Template function returning a list of ncids for a group */
#define INQGRPIDS(RFUN, NCFUN) \
SEXP RFUN (SEXP ncid) \
{ \
  int    status, count; \
  ROBJDEF(NOSXP,0); \
  status = NCFUN(INTEGER(ncid)[0], &count, NULL); \
  if(status != NC_NOERR) { \
    RRETURN(status); \
  } \
  RDATADEF(INTSXP,count); \
  status = NCFUN(INTEGER(ncid)[0], NULL, INTEGER(RDATASET)); \
  RRETURN(status); \
}

INQGRPIDS(R_nc_inq_grps, nc_inq_grps);
INQGRPIDS(R_nc_inq_typeids, nc_inq_typeids);
INQGRPIDS(R_nc_inq_varids, nc_inq_varids);


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dimids()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_dimids(SEXP ncid, SEXP ancestors)
{
  int    status, count;
  ROBJDEF(NOSXP,0);

  status = nc_inq_dimids(INTEGER(ncid)[0], &count, NULL, INTEGER(ancestors)[0]);
  if(status != NC_NOERR) {
    RRETURN(status);
  }
  RDATADEF(INTSXP,count);
  status = nc_inq_dimids(INTEGER(ncid)[0], NULL, INTEGER(RDATASET), INTEGER(ancestors)[0]);
  RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_unlimids()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_unlimids(SEXP ncid, SEXP ancestors)
{
  int    status, nunlim, *unlimids;
  ROBJDEF(NOSXP,0);

  status = R_nc_unlimdims (INTEGER(ncid)[0], &nunlim, &unlimids, INTEGER(ancestors)[0]);
  if (status != NC_NOERR) {
    RRETURN(status);
  }

  RDATADEF(INTSXP, nunlim);

  /* Sort the results for ease of presentation and searching */
  if (nunlim > 1) {
    qsort(unlimids, nunlim, sizeof(int), R_nc_int_cmp);
  }

  /* Copy temporary results to output structure */
  if (nunlim > 0) {
    memcpy(INTEGER(RDATASET), unlimids, nunlim*sizeof(int));
  }

  RRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_grp()                                                          *
\*-----------------------------------------------------------------------------*/
SEXP R_nc_rename_grp (SEXP ncid, SEXP grpname)
{
  int     status;
  ROBJDEF(NOSXP,0);

#if defined HAVE_DECL_NC_RENAME_GRP && HAVE_DECL_NC_RENAME_GRP
  /* Enter define mode */
  status = nc_redef(INTEGER(ncid)[0]);
  if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
    RRETURN(status);
  }

  /* Rename the group */
  status = nc_rename_grp(INTEGER(ncid)[0], CHAR(STRING_ELT(grpname, 0)));

#else
  status = E_UNSUPPORTED;
#endif

  RRETURN(status);
}


/*=============================================================================*\
 *  Udunits library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_ut_calendar()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP R_ut_calendar (SEXP unitstring, SEXP unitcount, SEXP values)
{
    int    year, month, day, hour, minute, count, i, status;
    float  second;
    double utvalue;
    char   strerror[64];
    utUnit utunit;
    ROBJDEF(REALSXP,INTEGER(unitcount)[0]*6);

    /*-- Scan unitstring ------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
    utIni(&utunit);
#endif

    status = utScan(CHAR(STRING_ELT(unitstring, 0)), &utunit);
    if(status != 0) {
        goto cleanup;
    }
  
    /*-- Check if unit is time and has origin ---------------------------------*/
    if ( !utIsTime(&utunit) ) {
        status = UT_ENOTTIME;
        goto cleanup;
    }

    if ( !utHasOrigin(&utunit) ) {
        status = UT_EINVALID;
        goto cleanup;
    }

    /*-- Convert values -------------------------------------------------------*/
    count = (int)INTEGER(unitcount)[0];
    for(i=0; i<count; i++) {
        utvalue = (double)REAL(values)[i];
	status  = utCalendar(utvalue, &utunit, &year, &month, &day,
	    &hour, &minute, &second);
        if (status != 0) {
          goto cleanup;
        }

	REAL(RDATASET)[i+0*count] = (double)year;
	REAL(RDATASET)[i+1*count] = (double)month;
	REAL(RDATASET)[i+2*count] = (double)day;
	REAL(RDATASET)[i+3*count] = (double)hour;
	REAL(RDATASET)[i+4*count] = (double)minute;
	REAL(RDATASET)[i+5*count] = (double)second;
    }

    /*-- Returning the list ---------------------------------------------------*/
cleanup:
#ifdef HAVE_LIBUDUNITS2
    utFree(&utunit);
#endif
    RUTRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_ut_init()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP R_ut_init (SEXP path)
{
    int   status;
    ROBJDEF(NOSXP,0);

    /*-- Avoid "overriding default" messages from UDUNITS-2 (1/2) -------------*/
#ifdef HAVE_LIBUDUNITS2
    ut_set_error_message_handler(ut_ignore);
#endif

    /*-- Initialize udunits library -------------------------------------------*/
    status = utInit(R_ExpandFileName(CHAR(STRING_ELT(path, 0))));

    /*-- Avoid "overriding default" messages from UDUNITS-2 (2/2) -------------*/
#ifdef HAVE_LIBUDUNITS2
    ut_set_error_message_handler(ut_write_to_stderr);
#endif

    /*-- Returning the list ---------------------------------------------------*/
    RUTRETURN(status);
}


/*-----------------------------------------------------------------------------*\
 *  R_ut_inv_calendar()                                                        *
\*-----------------------------------------------------------------------------*/

SEXP R_ut_inv_calendar (SEXP unitstring, SEXP unitcount, SEXP values)
{
    int    year, month, day, hour, minute, count, i, status;
    float  second;
    double utvalue;
    char   strerror[64];
    utUnit utunit;
    ROBJDEF(NOSXP,0);

    /*-- Create output object and initialize return values --------------------*/
    count = (int)INTEGER(unitcount)[0];
    count = count/6;
    RDATADEF(REALSXP,count);
    
    /*-- Scan unitstring ------------------------------------------------------*/
#ifdef HAVE_LIBUDUNITS2
    utIni(&utunit);
#endif

    status = utScan(CHAR(STRING_ELT(unitstring, 0)), &utunit);
    if(status != 0) {
        goto cleanup;
    }

    /*-- Check if unit is time and has origin ---------------------------------*/
    if ( !utIsTime(&utunit) ) {
        status = UT_ENOTTIME;
        goto cleanup;
    }

    if ( !utHasOrigin(&utunit) ) {
        status = UT_EINVALID;
        goto cleanup;
    }
  
    /*-- Convert values -------------------------------------------------------*/
    for(i=0; i<count; i++) {
        year   = (int)REAL(values)[i+0*count];
	month  = (int)REAL(values)[i+1*count];
	day    = (int)REAL(values)[i+2*count];
	hour   = (int)REAL(values)[i+3*count];
	minute = (int)REAL(values)[i+4*count];
	second = (double)REAL(values)[i+5*count];

        status = utInvCalendar(year, month, day, hour, minute, second,
	    &utunit, &utvalue);
        if (status != 0) {
          goto cleanup;
        }

        REAL(RDATASET)[i] = utvalue;
    }

    /*-- Returning the list ---------------------------------------------------*/
cleanup:
#ifdef HAVE_LIBUDUNITS2
    utFree(&utunit);
#endif
    RUTRETURN(status);
}

/*=============================================================================*/
 
/*=============================================================================*\
 *  SCRATCH                                                                    *
\*=============================================================================*/

