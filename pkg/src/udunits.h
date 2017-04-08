/*=============================================================================*\
 *									       *
 *  Name:       udunits.h						       *
 *									       *
 *  Version:    2.0-1							       *
 *									       *
 *  Purpose:    udunits functions for RNetCDF.				       *
 *									       *
 *  Author:     Pavel Michna (michna@giub.unibe.ch)			       *
 *              Milton Woods (m.woods@bom.gov.au)                              *
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


#ifndef RNC_UDUNITS_H_INCLUDED
#define RNC_UDUNITS_H_INCLUDED

SEXP
R_nc_calendar (SEXP unitstring, SEXP values);

SEXP
R_nc_utinit (SEXP path);

SEXP
R_nc_inv_calendar (SEXP unitstring, SEXP values);

#endif /* RNC_UDUNITS_H_INCLUDED */

