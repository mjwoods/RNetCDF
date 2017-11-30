/*=============================================================================*\
 *
 *  Name:       group.h
 *
 *  Version:    2.0-1
 *
 *  Purpose:    NetCDF group functions for RNetCDF.
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


#ifndef RNC_GROUP_H_INCLUDED
#define RNC_GROUP_H_INCLUDED

SEXP
R_nc_def_grp (SEXP nc, SEXP grpname);

SEXP
R_nc_inq_grp_parent (SEXP nc);

SEXP
R_nc_inq_natts (SEXP nc);

SEXP
R_nc_inq_grpname (SEXP nc, SEXP full);

SEXP
R_nc_inq_grp_ncid (SEXP nc, SEXP grpname, SEXP full);

SEXP
R_nc_inq_grps (SEXP nc);

SEXP
R_nc_inq_typeids (SEXP nc);

SEXP
R_nc_inq_varids (SEXP nc);

SEXP
R_nc_inq_dimids (SEXP nc, SEXP ancestors);

SEXP
R_nc_inq_unlimids (SEXP nc);

SEXP
R_nc_rename_grp (SEXP nc, SEXP grpname);

#endif /* RNC_GROUP_H_INCLUDED */
