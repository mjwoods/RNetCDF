/*=============================================================================*\
 *
 *  Name:       common.c
 *
 *  Version:    2.6-1
 *
 *  Purpose:    RNetCDF initialisation
 *
 *  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
 *              Milton Woods (miltonjwoods@gmail.com)
 *
 *  Copyright (C) 2004-2022 Pavel Michna and Milton Woods.
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

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "RNetCDF.h"

/* Register native routines */

static const R_CallMethodDef callMethods[]  = {
  {"R_nc_copy_att", (DL_FUNC) &R_nc_copy_att, 5},
  {"R_nc_delete_att", (DL_FUNC) &R_nc_delete_att, 3},
  {"R_nc_get_att", (DL_FUNC) &R_nc_get_att, 5},
  {"R_nc_inq_att", (DL_FUNC) &R_nc_inq_att, 3},
  {"R_nc_put_att", (DL_FUNC) &R_nc_put_att, 5},
  {"R_nc_rename_att", (DL_FUNC) &R_nc_rename_att, 4},
  {"R_nc_close", (DL_FUNC) &R_nc_close, 1},
  {"R_nc_create", (DL_FUNC) &R_nc_create, 9},
  {"R_nc_inq_file", (DL_FUNC) &R_nc_inq_file, 1},
  {"R_nc_open", (DL_FUNC) &R_nc_open, 8},
  {"R_nc_sync", (DL_FUNC) &R_nc_sync, 1},
  {"R_nc_def_dim", (DL_FUNC) &R_nc_def_dim, 4},
  {"R_nc_inq_dim", (DL_FUNC) &R_nc_inq_dim, 2},
  {"R_nc_inq_unlimids", (DL_FUNC) &R_nc_inq_unlimids, 1},
  {"R_nc_rename_dim", (DL_FUNC) &R_nc_rename_dim, 3},
  {"R_nc_def_grp", (DL_FUNC) &R_nc_def_grp, 2},
  {"R_nc_inq_grp_parent", (DL_FUNC) &R_nc_inq_grp_parent, 1},
  {"R_nc_inq_natts", (DL_FUNC) &R_nc_inq_natts, 1},
  {"R_nc_inq_grpname", (DL_FUNC) &R_nc_inq_grpname, 2},
  {"R_nc_inq_grp_ncid", (DL_FUNC) &R_nc_inq_grp_ncid, 3},
  {"R_nc_inq_grps", (DL_FUNC) &R_nc_inq_grps, 1},
  {"R_nc_inq_typeids", (DL_FUNC) &R_nc_inq_typeids, 1},
  {"R_nc_inq_varids", (DL_FUNC) &R_nc_inq_varids, 1},
  {"R_nc_inq_dimids", (DL_FUNC) &R_nc_inq_dimids, 2},
  {"R_nc_rename_grp", (DL_FUNC) &R_nc_rename_grp, 2},
  {"R_nc_def_type", (DL_FUNC) &R_nc_def_type, 9},
  {"R_nc_inq_type", (DL_FUNC) &R_nc_inq_type, 3},
  {"R_nc_calendar", (DL_FUNC) &R_nc_calendar, 2},
  {"R_nc_utinit", (DL_FUNC) &R_nc_utinit, 1},
  {"R_nc_inv_calendar", (DL_FUNC) &R_nc_inv_calendar, 2},
  {"R_nc_utterm", (DL_FUNC) &R_nc_utterm, 0},
  {"R_nc_def_var", (DL_FUNC) &R_nc_def_var, 12},
  {"R_nc_get_var", (DL_FUNC) &R_nc_get_var, 11},
  {"R_nc_inq_var", (DL_FUNC) &R_nc_inq_var, 2},
  {"R_nc_par_var", (DL_FUNC) &R_nc_par_var, 3},
  {"R_nc_put_var", (DL_FUNC) &R_nc_put_var, 10},
  {"R_nc_rename_var", (DL_FUNC) &R_nc_rename_var, 3},
  {NULL, NULL, 0}
};

void R_init_RNetCDF(DllInfo *info)
{
   R_registerRoutines(info, NULL, callMethods, NULL, NULL);
   R_useDynamicSymbols(info, FALSE);
   R_forceSymbols(info, TRUE);
}


