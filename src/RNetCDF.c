/*=============================================================================*\
 *									       *
 *  Name:       RNetCDF.c						       *
 *									       *
 *  Version:    1.6.1-2							       *
 *									       *
 *  Purpose:    NetCDF interface for R.					       *
 *									       *
 *  Author:     Pavel Michna (michna@giub.unibe.ch)			       *
 *              Milton Woods (m.woods@bom.gov.au)                              *
 *									       *
 *  Copyright:  (C) 2004-2012 Pavel Michna                                     *
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
 *									       *
\*=============================================================================*/


/*=============================================================================*\
 *  Includes								       *
\*=============================================================================*/

#include <stdio.h>
#include <string.h>

#include <netcdf.h>
#include <udunits.h>

#include <R.h>
#include <Rinternals.h>


/*=============================================================================*\
 *  NetCDF library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_nc_copy_att()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_copy_att (SEXP ncid_in, SEXP varid_in, SEXP globflag_in, SEXP attname,
                    SEXP ncid_out, SEXP varid_out, SEXP globflag_out)
{
    int  ncvarid_in, ncvarid_out, status, errstatus;
    int  enddef = 0;               /* Keep for possible future use as argument */
    char ncattname[NC_MAX_NAME];
    SEXP retlist, retlistnames;
    
    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status"));
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));

    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    
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
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Copy the attribute ---------------------------------------------------*/
    status = nc_copy_att(INTEGER(ncid_in)[0], ncvarid_in, ncattname,
                         INTEGER(ncid_out)[0], ncvarid_out);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
        errstatus = status;
    }
 
    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid_out)[0]);

	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }
    
    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;	 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_delete_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_delete_att (SEXP ncid, SEXP varid, SEXP globflag, SEXP attname)
{
    int  ncvarid, status, errstatus;
    int  enddef = 0;               /* Keep for possible future use as argument */
    char ncattname[NC_MAX_NAME];
    SEXP retlist, retlistnames;
    
    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));

    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    
    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Delete the attribute -------------------------------------------------*/
    status = nc_del_att(INTEGER(ncid)[0], ncvarid, ncattname);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
        errstatus = status;
    }
 
    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid)[0]);

	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;	 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_get_att (SEXP ncid, SEXP varid, SEXP name, 
                   SEXP numflag, SEXP globflag)
{
    int    ncvarid, i, status;
    char   ncattname[NC_MAX_NAME], *cvalue;    
    double *dvalue;    
    size_t attlen;
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("value")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    strcpy(ncattname, CHAR(STRING_ELT(name, 0)));

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Get the attribute's length -------------------------------------------*/
    status = nc_inq_attlen(INTEGER(ncid)[0], ncvarid, ncattname, &attlen);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 1))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }
    
    /*-- Get the attribute ----------------------------------------------------*/
    if(INTEGER(numflag)[0] == 1) {
        dvalue = Calloc((int)attlen, double); 
        status = nc_get_att_double(INTEGER(ncid)[0], ncvarid, ncattname, dvalue);
	if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	    REAL(VECTOR_ELT(retlist, 1))[0] = status;
	    UNPROTECT(2);
	    Free(dvalue);
	    return(retlist);
	}
    
        SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, (int)attlen));
        for(i=0; i<attlen; i++)
            REAL(VECTOR_ELT(retlist, 2))[i] = (double)dvalue[i];
        Free(dvalue);
    } else {
        cvalue = Calloc((int)attlen+1, char);
	status = nc_get_att_text(INTEGER(ncid)[0], ncvarid, ncattname, cvalue);
	cvalue[(int)attlen] = '\0';
	if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
            REAL(VECTOR_ELT(retlist, 1))[0] = status;
	    UNPROTECT(2);
	    Free(cvalue);
	    return(retlist);
	}
        
	SET_VECTOR_ELT(retlist, 2, allocVector(STRSXP, 1));
        SET_VECTOR_ELT(retlist, 2, mkString(cvalue));
        Free(cvalue);
    }
    
    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = status;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_att (SEXP ncid, SEXP varid, SEXP attname, SEXP attid,
                   SEXP nameflag, SEXP globflag)
{
    int     ncvarid, ncattid, attlen, status;
    char    atttype[NC_MAX_NAME], ncattname[NC_MAX_NAME];
    size_t  ncattlen;
    nc_type xtype;
    SEXP    retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 6));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 3, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 4, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 5, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 6)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("id")); 
    SET_STRING_ELT(retlistnames, 3, mkChar("name")); 
    SET_STRING_ELT(retlistnames, 4, mkChar("type")); 
    SET_STRING_ELT(retlistnames, 5, mkChar("length")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    ncattid    = INTEGER(attid)[0];
    strcpy(atttype,   "UNKNOWN");
    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));

    attlen     = -1;
    status     = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncattid;
    SET_VECTOR_ELT (retlist, 3, mkString(ncattname));
    SET_VECTOR_ELT (retlist, 4, mkString(atttype));
    REAL(VECTOR_ELT(retlist, 5))[0] = (double)attlen;
    
    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Get the attribute ID if necessary ------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_attid(INTEGER(ncid)[0], ncvarid, ncattname, &ncattid);
        if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	    REAL(VECTOR_ELT(retlist, 0))[0] = status;
	    UNPROTECT(2);
	    return(retlist);
	}
    }

    /*-- Get the attribute name if necessary ----------------------------------*/
    if(INTEGER(nameflag)[0] == 0) {
 	status = nc_inq_attname(INTEGER(ncid)[0], ncvarid, ncattid, ncattname);
	if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	    REAL(VECTOR_ELT(retlist, 0))[0] = status;
	    UNPROTECT(2);
	    return(retlist);
	}
    }

    /*-- Inquire the attribute ------------------------------------------------*/
    status = nc_inq_att(INTEGER(ncid)[0], ncvarid, ncattname, &xtype, 
        &ncattlen);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    attlen = (int)ncattlen;

    /*-- Convert nc_type to char ----------------------------------------------*/
    if      (xtype == NC_BYTE  )
        strcpy(atttype, "NC_BYTE"  );
    else if (xtype == NC_CHAR  )
        strcpy(atttype, "NC_CHAR"  );
    else if (xtype == NC_SHORT )
        strcpy(atttype, "NC_SHORT" );
    else if (xtype == NC_INT   )
        strcpy(atttype, "NC_INT"   );
    else if (xtype == NC_FLOAT )
        strcpy(atttype, "NC_FLOAT" );
    else if (xtype == NC_DOUBLE)
        strcpy(atttype, "NC_DOUBLE");
    else {
        strcpy(atttype, "UNKNOWN"  );
        SET_VECTOR_ELT (retlist, 1, mkString("Unknown NC_TYPE"));
	status = -1;
    }
    
    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = status;
    REAL(VECTOR_ELT(retlist, 2))[0] = ncattid;
    SET_VECTOR_ELT (retlist, 3, mkString(ncattname));
    SET_VECTOR_ELT (retlist, 4, mkString(atttype));
    REAL(VECTOR_ELT(retlist, 5))[0] = attlen;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_att()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_put_att (SEXP ncid, SEXP varid, SEXP name, SEXP type, SEXP attlen,
                   SEXP numflag, SEXP globflag, SEXP value)
{
    int     ncvarid, ncattlen, status, errstatus;
    int     enddef = 0;            /* Keep for possible future use as argument */
    char    ncattname[NC_MAX_NAME];
    nc_type xtype;
    SEXP    retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    ncattlen  = INTEGER(attlen)[0];
    strcpy(ncattname, CHAR(STRING_ELT(name, 0)));

    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Convert char to nc_type ----------------------------------------------*/
    if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_BYTE"  ) == 0)
        xtype = NC_BYTE;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_SHORT" ) == 0)
        xtype = NC_SHORT;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_INT"   ) == 0)
        xtype = NC_INT;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_FLOAT" ) == 0)
        xtype = NC_FLOAT;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_DOUBLE") == 0)
	xtype = NC_DOUBLE;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_CHAR"  ) == 0)
	xtype = NC_CHAR;
    else {
        SET_VECTOR_ELT (retlist, 1, mkString("Unknown NC_TYPE"));
	REAL(VECTOR_ELT(retlist, 0))[0] = -1;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Create the attribute -------------------------------------------------*/
    if(INTEGER(numflag)[0] == 1) {
        status = nc_put_att_double(INTEGER(ncid)[0], ncvarid, ncattname, xtype,
	    ncattlen, REAL(value));
	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
	    errstatus = status;
	}
    } else {
        ncattlen = strlen(CHAR(STRING_ELT(value, 0)));
	status = nc_put_att_text(INTEGER(ncid)[0], ncvarid, ncattname,
	    ncattlen, CHAR(STRING_ELT(value, 0)));
	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }
    
    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid)[0]);

	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;	 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_att()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_rename_att (SEXP ncid, SEXP varid, SEXP globflag, SEXP attname,
                      SEXP newname)
{
    int  ncvarid, status, errstatus;
    int  enddef = 0;               /* Keep for possible future use as argument */
    char ncattname[NC_MAX_NAME], ncnewname[NC_MAX_NAME];
    SEXP retlist, retlistnames;
    
    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    strcpy(ncattname, CHAR(STRING_ELT(attname, 0)));
    strcpy(ncnewname, CHAR(STRING_ELT(newname, 0)));

    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    
    /*-- Check if it is a global attribute ------------------------------------*/
    if(INTEGER(globflag)[0] == 1)
        ncvarid = NC_GLOBAL;
    else
        ncvarid = INTEGER(varid)[0];

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Rename the attribute -------------------------------------------------*/
    status = nc_rename_att(INTEGER(ncid)[0], ncvarid, ncattname, ncnewname);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
        errstatus = status;
    }

    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid)[0]);

	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }
    
    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;	 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_close()                                                               *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_close (SEXP ncid)
{
    int  status;
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Close the file -------------------------------------------------------*/
    status = nc_close(INTEGER(ncid)[0]);
    if(status != NC_NOERR)
	SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_create()                                                              *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_create (SEXP filename, SEXP clobber, SEXP large, SEXP share,
                  SEXP prefill)
{
    int  cmode, fillmode, old_fillmode, ncid, status;
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("ncid")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    ncid   = -1;
    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncid;

    /*-- Determine the cmode --------------------------------------------------*/
    if(INTEGER(clobber)[0] == 0)
        cmode = NC_NOCLOBBER;
    else
        cmode = NC_CLOBBER;

    /*-- Determine if using classic format or 64-bit offset -------------------*/
    if(INTEGER(large)[0] != 0)
        cmode = cmode | NC_64BIT_OFFSET;

    /*-- Determine which buffer scheme shall be used --------------------------*/
    if(INTEGER(share)[0] != 0)
        cmode = cmode | NC_SHARE;

    /*-- Determine the fillmode -----------------------------------------------*/
    if(INTEGER(prefill)[0] == 0)
        fillmode = NC_NOFILL;
    else
        fillmode = NC_FILL;

    /*-- Create the file ------------------------------------------------------*/
    status = nc_create(R_ExpandFileName(CHAR(STRING_ELT(filename, 0))), 
        cmode, &ncid);

    /*-- Set the fill mode ----------------------------------------------------*/
    if(status == NC_NOERR)
        status = nc_set_fill(ncid, fillmode, &old_fillmode);

    /*-- Returning the list ---------------------------------------------------*/
    if(status != NC_NOERR)
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncid;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_def_dim (SEXP ncid, SEXP dimname, SEXP size, SEXP unlimp)
{
    int  dimid, status, errstatus;
    int  enddef = 0;               /* Keep for possible future use as argument */
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("dimid")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    dimid     = -1;
    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)dimid;

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
	SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
        UNPROTECT(2);
	return(retlist);
    }

    /*-- Create the dimension -------------------------------------------------*/
    if(INTEGER(unlimp)[0] == 1) {
	status = nc_def_dim(INTEGER(ncid)[0], CHAR(STRING_ELT(dimname, 0)), 
	    NC_UNLIMITED, &dimid);
	if(status != NC_NOERR) {
	    SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
        }
    } else {
	status = nc_def_dim(INTEGER(ncid)[0], CHAR(STRING_ELT(dimname, 0)), 
	    INTEGER(size)[0], &dimid);
	if(status != NC_NOERR) {
	    SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
        }
    }

    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid)[0]);
    
	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)dimid;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_dim()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_dim (SEXP ncid, SEXP dimid, SEXP dimname, SEXP nameflag)
{
    int    unlimdimid, unlimp, ncdimid, dimlen, status;
    size_t ncdimlen;
    char   ncdimname[NC_MAX_NAME];
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 6));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 3, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 4, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 5, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 6)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("id")); 
    SET_STRING_ELT(retlistnames, 3, mkChar("name")); 
    SET_STRING_ELT(retlistnames, 4, mkChar("length")); 
    SET_STRING_ELT(retlistnames, 5, mkChar("unlim")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    strcpy(ncdimname, CHAR(STRING_ELT(dimname, 0)));

    ncdimid  = INTEGER(dimid)[0];
    dimlen   = -1;
    ncdimlen = (size_t)dimlen;
    unlimp   = -1;
    status   = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncdimid;
    SET_VECTOR_ELT (retlist, 3, mkString(ncdimname));
    REAL(VECTOR_ELT(retlist, 4))[0] = (double)dimlen;
    REAL(VECTOR_ELT(retlist, 5))[0] = (double)unlimp;

    /*-- Get the dimension ID if necessary ------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_dimid(INTEGER(ncid)[0], ncdimname, &ncdimid);
        if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	    REAL(VECTOR_ELT(retlist, 0))[0] = status;
	    UNPROTECT(2);
	    return(retlist);
	}
    }

    /*-- Inquire the dimension ------------------------------------------------*/
    status = nc_inq_dim(INTEGER(ncid)[0], ncdimid, ncdimname, &ncdimlen);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Check if it is the unlimited dimension -------------------------------*/
    status = nc_inq_unlimdim(INTEGER(ncid)[0], &unlimdimid);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Converting from size_t to int and setting unlimdim-flag --------------*/
    dimlen = (int)ncdimlen;
    
    if(unlimdimid == ncdimid)
        unlimp = 1;
    else
        unlimp = 0;

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = status;
    REAL(VECTOR_ELT(retlist, 2))[0] = ncdimid;
    SET_VECTOR_ELT (retlist, 3, mkString(ncdimname));
    REAL(VECTOR_ELT(retlist, 4))[0] = dimlen;
    REAL(VECTOR_ELT(retlist, 5))[0] = unlimp;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_dim()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_rename_dim (SEXP ncid, SEXP dimid, SEXP dimname, SEXP nameflag,
                      SEXP newname)
{
    int  ncdimid, status, errstatus;
    int  enddef = 0;               /* Keep for possible future use as argument */
    char ncdimname[NC_MAX_NAME], ncnewname[NC_MAX_NAME];
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    ncdimid   = INTEGER(dimid)[0];
    strcpy(ncdimname, CHAR(STRING_ELT(dimname, 0)));
    strcpy(ncnewname, CHAR(STRING_ELT(newname, 0)));
    
    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Get the dimension ID if necessary ------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_dimid(INTEGER(ncid)[0], ncdimname, &ncdimid);
	if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	    REAL(VECTOR_ELT(retlist, 0))[0] = status;
	    UNPROTECT(2);
	    return(retlist);
	}
    }

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Rename the dimension -------------------------------------------------*/
    status = nc_rename_dim(INTEGER(ncid)[0], ncdimid, ncnewname);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
        errstatus = status;
    }
 
    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid)[0]);

	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;	 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_file()                                                            *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_file (SEXP ncid)
{
    int  ndims, nvars, ngatts, unlimdimid, status;
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 6));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 3, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 4, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 5, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 6)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("ndims")); 
    SET_STRING_ELT(retlistnames, 3, mkChar("nvars")); 
    SET_STRING_ELT(retlistnames, 4, mkChar("ngatts")); 
    SET_STRING_ELT(retlistnames, 5, mkChar("unlimdimid")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    ndims      = -1;
    nvars      = -1;
    ngatts     = -1;
    unlimdimid = -1;
    status     = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ndims;
    REAL(VECTOR_ELT(retlist, 3))[0] = (double)nvars;
    REAL(VECTOR_ELT(retlist, 4))[0] = (double)ngatts;
    REAL(VECTOR_ELT(retlist, 5))[0] = (double)unlimdimid;
    
    /*-- Inquire about the NetCDF dataset -------------------------------------*/
    status = nc_inq(INTEGER(ncid)[0], &ndims, &nvars, &ngatts, &unlimdimid);
    if(status != NC_NOERR)
	SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ndims;	 
    REAL(VECTOR_ELT(retlist, 3))[0] = (double)nvars;	 
    REAL(VECTOR_ELT(retlist, 4))[0] = (double)ngatts;	 
    REAL(VECTOR_ELT(retlist, 5))[0] = (double)unlimdimid; 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_open()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill)
{
    int  ncid, omode, fillmode, old_fillmode, status;
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("ncid")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    ncid   = -1;
    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncid;
    
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
    
    /*-- Set the fill mode ----------------------------------------------------*/
    if((status == NC_NOERR) && (INTEGER(write)[0] != 0))
        status = nc_set_fill(ncid, fillmode, &old_fillmode);

    /*-- Returning the list ---------------------------------------------------*/
    if(status != NC_NOERR)
	SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncid;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_sync()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_sync (SEXP ncid)
{
    int  status;
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Sync the file --------------------------------------------------------*/
    status = nc_sync(INTEGER(ncid)[0]);
    if(status != NC_NOERR)
	SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_def_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_def_var (SEXP ncid, SEXP varname, SEXP type, SEXP ndims, SEXP dimids)
{
    int     varid, status, errstatus;
    int     enddef = 0;            /* Keep for possible future use as argument */
    nc_type xtype;
    SEXP    retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("varid")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    varid     = -1;
    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)varid;

    /*-- Convert char to nc_type ----------------------------------------------*/
    if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_BYTE"  ) == 0)
        xtype = NC_BYTE;
    else if(strcmp(CHAR(STRING_ELT(type ,0)), "NC_SHORT" ) == 0)
        xtype = NC_SHORT;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_INT"   ) == 0)
        xtype = NC_INT;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_FLOAT" ) == 0)
        xtype = NC_FLOAT;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_DOUBLE") == 0)
	xtype = NC_DOUBLE;
    else if(strcmp(CHAR(STRING_ELT(type, 0)), "NC_CHAR"  ) == 0)
	xtype = NC_CHAR;
    else {
        SET_VECTOR_ELT (retlist, 1, mkString("Unknown NC_TYPE"));
        REAL(VECTOR_ELT(retlist, 0))[0] = -1;
	UNPROTECT(2);
	return(retlist);
    }
    
    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Define the variable --------------------------------------------------*/
    status = nc_def_var(INTEGER(ncid)[0], CHAR(STRING_ELT(varname, 0)), xtype,
        INTEGER(ndims)[0], INTEGER(dimids), &varid);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
        errstatus = status;
    }

    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid)[0]);

	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)varid;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_vara_double()                                                     *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_get_vara_double (SEXP ncid, SEXP varid, SEXP start, 
                           SEXP count, SEXP varsize)
{
    int    ndims, i, status;
    double *data;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, INTEGER(varsize)[0]));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("data")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    data = Calloc(INTEGER(varsize)[0], double);

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Get ndims for this var -----------------------------------------------*/
    status = nc_inq_varndims(INTEGER(ncid)[0], INTEGER(varid)[0], &ndims);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
        UNPROTECT(2);
	Free(data);
	return(retlist);
    }

    /*-- Copy over from int to size_t, handle scalar variables ----------------*/
    if(ndims > 0) {
	for(i=0; i<ndims; i++) {
	    s_start[i] = (size_t)INTEGER(start)[i];
	    s_count[i] = (size_t)INTEGER(count)[i];
	}
    }
    else {
        s_start[0] = 0;
	s_count[0] = 1;
    }
		
    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	Free(data);
	return(retlist);
    }

    /*-- Get the var ----------------------------------------------------------*/
    status = nc_get_vara_double(INTEGER(ncid)[0], INTEGER(varid)[0],
        s_start, s_count, data);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
        UNPROTECT(2);
	Free(data);
	return(retlist);
    }
   
    /*-- Copy from C to R object ----------------------------------------------*/
    for(i=0; i<INTEGER(varsize)[0]; i++)
        REAL(VECTOR_ELT(retlist, 2))[i] = (double)data[i];

    Free(data);

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = status;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_get_vara_text()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_get_vara_text (SEXP ncid, SEXP varid, SEXP start, 
                         SEXP count, SEXP varsize)
{
    int    ndims, tx_len, tx_num, i, j, status;
    char   *data, *tx_str;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    tx_len = INTEGER(varsize)[1];
    tx_num = INTEGER(varsize)[0]/tx_len;
    
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(STRSXP,  tx_num));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("data")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    data = Calloc(INTEGER(varsize)[0], char);                 /*-- Is zeroed --*/

    /*-- Get ndims for this var -----------------------------------------------*/
    status = nc_inq_varndims(INTEGER(ncid)[0], INTEGER(varid)[0], &ndims);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
        UNPROTECT(2);
	Free(data);
	return(retlist);
    }

    /*-- Copy over from int to size_t, handle scalar variables ----------------*/
    if(ndims > 0) {
	for(i=0; i<ndims; i++) {
	    s_start[i] = (size_t)INTEGER(start)[i];
	    s_count[i] = (size_t)INTEGER(count)[i];
	}
    }
    else {
        s_start[0] = 0;
	s_count[0] = 1;
    }

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	Free(data);
	return(retlist);
    }

    /*-- Get the var ----------------------------------------------------------*/
    status = nc_get_vara_text(INTEGER(ncid)[0], INTEGER(varid)[0],
        s_start, s_count, data);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
        UNPROTECT(2);
	Free(data);
	return(retlist);
    }
   
    /*-- Copy from C to R object ----------------------------------------------*/
    tx_str = Calloc(tx_len+1, char);                    /*-- String handling --*/
    for(i=0; i<tx_num; i++) {
        for(j=0; j<tx_len; j++)
            tx_str[j] = data[i*tx_len+j];
	tx_str[j+1] = '\0';                             /*-- String handling --*/
	SET_STRING_ELT(VECTOR_ELT(retlist, 2), i, mkChar(tx_str));
    }

    Free(data);
    Free(tx_str);

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_inq_var()                                                             *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_inq_var (SEXP ncid, SEXP varid, SEXP varname, SEXP nameflag)
{
    int     ncvarid, ndimsp, nattsp, i, status, *dimids;
    char    ncvarname[NC_MAX_NAME], vartype[NC_MAX_NAME];
    nc_type xtype;
    SEXP    retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 8));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 3, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 4, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 5, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 7, allocVector(REALSXP, 1));

    PROTECT(retlistnames = allocVector(STRSXP, 8)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("id")); 
    SET_STRING_ELT(retlistnames, 3, mkChar("name")); 
    SET_STRING_ELT(retlistnames, 4, mkChar("type")); 
    SET_STRING_ELT(retlistnames, 5, mkChar("ndims")); 
    SET_STRING_ELT(retlistnames, 6, mkChar("dimids")); 
    SET_STRING_ELT(retlistnames, 7, mkChar("natts")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    ncvarid = INTEGER(varid)[0];
    strcpy(ncvarname, CHAR(STRING_ELT(varname, 0)));
    strcpy(vartype,   "UNKNOWN");

    ndimsp  = -1;
    dimids  = NULL;
    nattsp  = -1;
    status  = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncvarid;
    SET_VECTOR_ELT (retlist, 3, mkString(ncvarname));
    SET_VECTOR_ELT (retlist, 4, mkString(vartype));
    REAL(VECTOR_ELT(retlist, 5))[0] = (double)ndimsp;
    REAL(VECTOR_ELT(retlist, 7))[0] = (double)nattsp;

    /*-- Get the variable ID if necessary -------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_varid(INTEGER(ncid)[0], ncvarname, &ncvarid);
        if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	    REAL(VECTOR_ELT(retlist, 0))[0] = status;
	    UNPROTECT(2);
	    return(retlist);
	}
    }

    /*-- Get the number of dimensions -----------------------------------------*/
    status = nc_inq_varndims(INTEGER(ncid)[0], ncvarid, &ndimsp);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    if(ndimsp == 0) {
    	SET_VECTOR_ELT(retlist, 6, allocVector(REALSXP, 1));
	dimids = Calloc(1, int);
    }
    else {
    	SET_VECTOR_ELT(retlist, 6, allocVector(REALSXP, ndimsp));
	dimids = Calloc(ndimsp, int);
    }

    /*-- Inquire the variable -------------------------------------------------*/
    status = nc_inq_var(INTEGER(ncid)[0], ncvarid, ncvarname, &xtype, &ndimsp,
        dimids, &nattsp);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	Free(dimids);
	return(retlist);
    }

    /*-- Convert nc_type to char ----------------------------------------------*/
    if      (xtype == NC_BYTE  )
        strcpy(vartype, "NC_BYTE"  );
    else if (xtype == NC_CHAR  )
        strcpy(vartype, "NC_CHAR"  );
    else if (xtype == NC_SHORT )
        strcpy(vartype, "NC_SHORT" );
    else if (xtype == NC_INT   )
        strcpy(vartype, "NC_INT"   );
    else if (xtype == NC_FLOAT )
        strcpy(vartype, "NC_FLOAT" );
    else if (xtype == NC_DOUBLE)
        strcpy(vartype, "NC_DOUBLE");
    else {
        strcpy(vartype, "UNKNOWN"  );
        SET_VECTOR_ELT(retlist, 1, mkString("Unknown NC_TYPE"));
    }

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = status;
    REAL(VECTOR_ELT(retlist, 2))[0] = ncvarid;
    SET_VECTOR_ELT (retlist, 3, mkString(ncvarname));
    SET_VECTOR_ELT (retlist, 4, mkString(vartype));
    REAL(VECTOR_ELT(retlist, 5))[0] = ndimsp;
    for(i=0; i<ndimsp; i++)
        REAL(VECTOR_ELT(retlist, 6))[i] = (double)dimids[i];
    REAL(VECTOR_ELT(retlist, 7))[0] = nattsp;
    Free(dimids);
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_vara_double()                                                     *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_put_vara_double (SEXP ncid, SEXP varid, SEXP start, 
                           SEXP count, SEXP data)
{
    int    ndims, i, status;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Get ndims for this var -----------------------------------------------*/
    status = nc_inq_varndims(INTEGER(ncid)[0], INTEGER(varid)[0], &ndims);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;	 
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Copy over from int to size_t, handle scalar variables ----------------*/
    if(ndims > 0) {
	for(i=0; i<ndims; i++) {
	    s_start[i] = (size_t)INTEGER(start)[i];
	    s_count[i] = (size_t)INTEGER(count)[i];
	}
    }
    else {
        s_start[0] = 0;
	s_count[0] = 1;
    }

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Put the var ----------------------------------------------------------*/
    status = nc_put_vara_double(INTEGER(ncid)[0], INTEGER(varid)[0],
        s_start, s_count, REAL(data));
    if(status != NC_NOERR)
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_put_vara_text()                                                       *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_put_vara_text (SEXP ncid, SEXP varid, SEXP start, 
                         SEXP count, SEXP data, SEXP varsize)
{
    int    ndims, tx_num, tx_len, i, j, status;
    char   *ncdata, *tx_str;
    size_t s_start[MAX_NC_DIMS], s_count[MAX_NC_DIMS];
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    tx_len = INTEGER(varsize)[1];
    tx_num = INTEGER(varsize)[0]/tx_len;
    
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));

    /*-- Get ndims for this var -----------------------------------------------*/
    status = nc_inq_varndims(INTEGER(ncid)[0], INTEGER(varid)[0], &ndims);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;	 
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Copy over from int to size_t, handle scalar variables ----------------*/
    if(ndims > 0) {
	for(i=0; i<ndims; i++) {
	    s_start[i] = (size_t)INTEGER(start)[i];
	    s_count[i] = (size_t)INTEGER(count)[i];
	}
    }
    else {
        s_start[0] = 0;
	s_count[0] = 1;
    }

    /*-- Copy from R to C object ----------------------------------------------*/
    ncdata = Calloc(tx_len*tx_num, char);                     /*-- Is zeroed --*/
    tx_str = Calloc(tx_len, char);                            /*-- Is zeroed --*/
        
    for(i=0; i<tx_num; i++) {
        strcpy(tx_str, CHAR(STRING_ELT(data, i)));
	for(j=0; j<tx_len; j++) {
            ncdata[i*tx_len+j] = tx_str[j];
	    tx_str[j] = '\0';
	}
    }

    /*-- Enter data mode (if necessary) ---------------------------------------*/
    status = nc_enddef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_ENOTINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	Free(ncdata);
	Free(tx_str);
	return(retlist);
    }

    /*-- Put the var ----------------------------------------------------------*/
    status = nc_put_vara_text(INTEGER(ncid)[0], INTEGER(varid)[0],
        s_start, s_count, ncdata);
    if(status != NC_NOERR)
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

    Free(ncdata);
    Free(tx_str);

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_nc_rename_var()                                                          *
\*-----------------------------------------------------------------------------*/

SEXP R_nc_rename_var (SEXP ncid, SEXP varid, SEXP varname, SEXP nameflag,
                      SEXP newname)
{
    int  ncvarid, status, errstatus;
    int  enddef = 0;               /* Keep for possible future use as argument */
    char ncvarname[NC_MAX_NAME], ncnewname[NC_MAX_NAME];
    SEXP retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    strcpy(ncvarname, CHAR(STRING_ELT(varname, 0)));
    strcpy(ncnewname, CHAR(STRING_ELT(newname, 0)));

    ncvarid   = INTEGER(varid)[0];
    status    = -1;
    errstatus = 0;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    
    /*-- Get the variable ID if necessary -------------------------------------*/
    if(INTEGER(nameflag)[0] == 1) {
 	status = nc_inq_varid(INTEGER(ncid)[0], ncvarname, &ncvarid);
	if(status != NC_NOERR) {
            SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	    REAL(VECTOR_ELT(retlist, 0))[0] = status;
	    UNPROTECT(2);
	    return(retlist);
	}
    }

    /*-- Enter define mode ----------------------------------------------------*/
    status = nc_redef(INTEGER(ncid)[0]);
    if((status != NC_NOERR) && (status != NC_EINDEFINE)) {
        SET_VECTOR_ELT (retlist, 1, mkString(nc_strerror(status)));
	REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);
    }

    /*-- Rename the variable --------------------------------------------------*/
    status = nc_rename_var(INTEGER(ncid)[0], ncvarid, ncnewname);
    if(status != NC_NOERR) {
        SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
        errstatus = status;
    }
 
    /*-- Leave define mode ----------------------------------------------------*/
    if(enddef != 0) {
        status = nc_enddef(INTEGER(ncid)[0]);

	if(status != NC_NOERR) {
            SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));
            errstatus = status;
	}
    }

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)errstatus;	 
    UNPROTECT(2);
    return(retlist);
}


/*=============================================================================*\
 *  Udunits library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_ut_strerror()                                                            *
\*-----------------------------------------------------------------------------*/

void R_ut_strerror (int errcode, char* strerror)
{
    if     (errcode == UT_EOF     )
        strcpy(strerror, "end-of-file encountered (udunits)");
    else if(errcode == UT_ENOFILE )
        strcpy(strerror, "no units-file (udunits)");
    else if(errcode == UT_ESYNTAX )
        strcpy(strerror, "syntax error (udunits)");
    else if(errcode == UT_EUNKNOWN)
        strcpy(strerror, "unknown specification (udunits)");
    else if(errcode == UT_EIO     )
        strcpy(strerror, "I/O error (udunits)");
    else if(errcode == UT_EINVALID)
        strcpy(strerror, "invalid unit-structure (udunits)");
    else if(errcode == UT_ENOINIT )
        strcpy(strerror, "package not initialized (udunits)");
    else if(errcode == UT_ECONVERT)
        strcpy(strerror, "two units are not convertable (udunits)");
    else if(errcode == UT_EALLOC  )
        strcpy(strerror, "memory allocation failure (udunits)");
    else if(errcode == UT_ENOROOM )
        strcpy(strerror, "insufficient room supplied (udunits)");
    else if(errcode == UT_ENOTTIME)
        strcpy(strerror, "not a unit of time (udunits)");
    else
        strcpy(strerror, "unknown error (udunits)");
}


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
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocMatrix(REALSXP, INTEGER(unitcount)[0], 6));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status"));
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
    SET_STRING_ELT(retlistnames, 2, mkChar("value")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    
    /*-- Scan unitstring ------------------------------------------------------*/
    status = utScan(CHAR(STRING_ELT(unitstring, 0)), &utunit);
    if(status != 0) {
        R_ut_strerror(status, strerror);
        SET_VECTOR_ELT (retlist, 1, mkString(strerror));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);        
    }
  
    /*-- Check if unit is time and has origin ---------------------------------*/
    status = utIsTime(&utunit);    
    if(status == 0) {
        R_ut_strerror(UT_ENOTTIME, strerror);
        SET_VECTOR_ELT (retlist, 1, mkString(strerror));
	REAL(VECTOR_ELT(retlist, 0))[0] = UT_ENOTTIME;
	UNPROTECT(2);
	return(retlist);        
    }

    status = utHasOrigin(&utunit);    
    if(status == 0) {
        R_ut_strerror(UT_EINVALID, strerror);
        SET_VECTOR_ELT (retlist, 1, mkString(strerror));
        REAL(VECTOR_ELT(retlist, 0))[0] = UT_EINVALID;
	UNPROTECT(2);
	return(retlist);        
    }

    /*-- Convert values -------------------------------------------------------*/
    count = (int)INTEGER(unitcount)[0];
    for(i=0; i<count; i++) {
        utvalue = (double)REAL(values)[i];
	status  = utCalendar(utvalue, &utunit, &year, &month, &day,
	    &hour, &minute, &second);

	REAL(VECTOR_ELT(retlist, 2))[i+0*count] = (double)year;
	REAL(VECTOR_ELT(retlist, 2))[i+1*count] = (double)month;
	REAL(VECTOR_ELT(retlist, 2))[i+2*count] = (double)day;
	REAL(VECTOR_ELT(retlist, 2))[i+3*count] = (double)hour;
	REAL(VECTOR_ELT(retlist, 2))[i+4*count] = (double)minute;
	REAL(VECTOR_ELT(retlist, 2))[i+5*count] = (double)second;
    }

    /*-- Returning the list ---------------------------------------------------*/
    if(status != 0) {
        R_ut_strerror(status, strerror);
        SET_VECTOR_ELT(retlist, 1, mkString(strerror));
    }
    
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	     
    UNPROTECT(2);
    return(retlist);
}


/*-----------------------------------------------------------------------------*\
 *  R_ut_init()                                                                *
\*-----------------------------------------------------------------------------*/

SEXP R_ut_init (SEXP path)
{
    int   status;
    char  strerror[64];
    SEXP  retlist, retlistnames;

    /*-- Avoid "overriding default" messages from UDUNITS-2 (1/2) -------------*/
    #ifdef UT_UNITS2_H_INCLUDED
        ut_system* unitSystem;

        ut_set_error_message_handler(ut_ignore);
        unitSystem = ut_read_xml(NULL);
    #endif

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

    PROTECT(retlistnames = allocVector(STRSXP, 2)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
   
    /*-- Initialize udunits library -------------------------------------------*/
    status = utInit(R_ExpandFileName(CHAR(STRING_ELT(path, 0))));
    if(status != 0) {
        R_ut_strerror(status, strerror);
        SET_VECTOR_ELT(retlist, 1, mkString(strerror));
    }

    /*-- Avoid "overriding default" messages from UDUNITS-2 (2/2) -------------*/
    #ifdef UT_UNITS2_H_INCLUDED
        ut_set_error_message_handler(ut_write_to_stderr);
    #endif

    /*-- Returning the list ---------------------------------------------------*/
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
    UNPROTECT(2);
    return(retlist);
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
    SEXP   retlist, retlistnames;

    /*-- Create output object and initialize return values --------------------*/
    count = (int)INTEGER(unitcount)[0];
    count = count/6;
    
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, count));

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status")); 
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
    SET_STRING_ELT(retlistnames, 2, mkChar("value")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    status = -1;
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	 
    SET_VECTOR_ELT (retlist, 1, mkString(""));
    
    /*-- Scan unitstring ------------------------------------------------------*/
    status = utScan(CHAR(STRING_ELT(unitstring, 0)), &utunit);
    if(status != 0) {
        R_ut_strerror(status, strerror);
        SET_VECTOR_ELT (retlist, 1, mkString(strerror));
        REAL(VECTOR_ELT(retlist, 0))[0] = status;
	UNPROTECT(2);
	return(retlist);        
    }

    /*-- Check if unit is time and has origin ---------------------------------*/
    status = utIsTime(&utunit);    
    if(status == 0) {
        R_ut_strerror(UT_ENOTTIME, strerror);
        SET_VECTOR_ELT (retlist, 1, mkString(strerror));
        REAL(VECTOR_ELT(retlist, 0))[0] = UT_ENOTTIME;
	UNPROTECT(2);
	return(retlist);        
    }

    status = utHasOrigin(&utunit);    
    if(status == 0) {
        R_ut_strerror(UT_EINVALID, strerror);
        SET_VECTOR_ELT (retlist, 1, mkString(strerror));
        REAL(VECTOR_ELT(retlist, 0))[0] = UT_EINVALID;
	UNPROTECT(2);
	return(retlist);        
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

        REAL(VECTOR_ELT(retlist, 2))[i] = (double)utvalue;
    }

    /*-- Returning the list ---------------------------------------------------*/
    if(status != 0) {
        R_ut_strerror(status, strerror);
        SET_VECTOR_ELT(retlist, 1, mkString(strerror));
    }
    
    REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;	     
    UNPROTECT(2);
    return(retlist);
}


/*=============================================================================*/
 
/*=============================================================================*\
 *  SCRATCH                                                                    *
\*=============================================================================*/

