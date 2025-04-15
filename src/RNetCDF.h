/*=============================================================================*\
 *
 *  Name:       RNetCDF.h
 *
 *  Version:    2.10-2
 *
 *  Purpose:    Declare RNetCDF functions callable from R
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright (C) 2004-2025 Pavel Michna and Milton Woods.
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

#ifndef RNC_RNETCDF_H_INCLUDED
#define RNC_RNETCDF_H_INCLUDED


/* Attributes */

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


/* Datasets */

SEXP
R_nc_close (SEXP ptr);

SEXP
R_nc_create (SEXP filename, SEXP clobber, SEXP share, SEXP prefill,
             SEXP format, SEXP diskless, SEXP persist,
             SEXP mpi_comm, SEXP mpi_info);

SEXP
R_nc_inq_file (SEXP nc);

SEXP
R_nc_open (SEXP filename, SEXP write, SEXP share, SEXP prefill,
           SEXP diskless, SEXP persist, SEXP mpi_comm, SEXP mpi_info);

SEXP
R_nc_sync (SEXP nc);


/* Dimensions */

SEXP
R_nc_def_dim (SEXP nc, SEXP dimname, SEXP size, SEXP unlim);

SEXP
R_nc_inq_dim (SEXP nc, SEXP dim);

SEXP
R_nc_inq_unlimids (SEXP nc);

SEXP
R_nc_rename_dim (SEXP nc, SEXP dim, SEXP newname);


/* Groups */

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
R_nc_rename_grp (SEXP nc, SEXP grpname);


/* Types */

SEXP
R_nc_def_type (SEXP nc, SEXP typename, SEXP class, SEXP size, SEXP basetype,
               SEXP names, SEXP values, SEXP subtypes, SEXP dimsizes);

SEXP
R_nc_inq_type (SEXP nc, SEXP type, SEXP fields);


/* Units */

SEXP
R_nc_calendar (SEXP unitstring, SEXP values);

SEXP
R_nc_utinit (SEXP path);

SEXP
R_nc_inv_calendar (SEXP unitstring, SEXP values);

SEXP
R_nc_utterm (void);


/* Variables */

SEXP
R_nc_def_var (SEXP nc, SEXP varname, SEXP type, SEXP dims,
              SEXP chunking, SEXP chunksizes, SEXP deflate, SEXP shuffle,
              SEXP big_endian, SEXP fletcher32, SEXP filter_id,
              SEXP filter_params);

SEXP
R_nc_get_var (SEXP nc, SEXP var, SEXP start, SEXP count,
              SEXP rawchar, SEXP fitnum, SEXP namode, SEXP unpack,
              SEXP cache_bytes, SEXP cache_slots, SEXP cache_preemption);

SEXP
R_nc_inq_var (SEXP nc, SEXP var);

SEXP
R_nc_par_var (SEXP nc, SEXP var, SEXP access);

SEXP
R_nc_put_var (SEXP nc, SEXP var, SEXP start, SEXP count, SEXP data,
              SEXP namode, SEXP pack,
              SEXP cache_bytes, SEXP cache_slots, SEXP cache_preemption);

SEXP
R_nc_rename_var (SEXP nc, SEXP var, SEXP newname);


#endif  /* RNC_RNETCDF_H_INCLUDED */
