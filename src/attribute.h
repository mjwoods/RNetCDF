/*=============================================================================*\
 *
 *  Name:       attribute.h
 *
 *  Version:    2.0-1
 *
 *  Purpose:    NetCDF attribute functions for RNetCDF
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


#ifndef RNC_ATTRIBUTE_H_INCLUDED
#define RNC_ATTRIBUTE_H_INCLUDED


SEXP
R_nc_copy_att (SEXP nc_in, SEXP var_in, SEXP att, SEXP nc_out, SEXP var_out);

SEXP
R_nc_delete_att (SEXP nc, SEXP var, SEXP att);

SEXP
R_nc_get_att (SEXP nc, SEXP var, SEXP att, SEXP rawchar, SEXP fitnum);

SEXP
R_nc_inq_att (SEXP nc, SEXP var, SEXP att);

SEXP
R_nc_put_att (SEXP nc, SEXP var, SEXP att, SEXP type, SEXP data);

SEXP
R_nc_rename_att (SEXP nc, SEXP var, SEXP att, SEXP newname);


#endif /* RNC_ATTRIBUTE_H_INCLUDED */

