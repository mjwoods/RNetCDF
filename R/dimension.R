#===============================================================================
#
#  Name:       dimension.R
#
#  Purpose:    NetCDF interface for R - dimension functions.
#
#  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
#              Milton Woods (miltonjwoods@gmail.com)
#
#  Copyright (C) 2004-2022 Pavel Michna and Milton Woods.
#
#===============================================================================
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License along
#  with this program; if not, write to the Free Software Foundation, Inc.,
#  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
#===============================================================================


#-------------------------------------------------------------------------------
# dim.def.nc()
#-------------------------------------------------------------------------------

dim.def.nc <- function(ncfile, dimname, dimlength = 1, unlim = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(dimname))
  stopifnot(is.numeric(dimlength))
  stopifnot(is.logical(unlim))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_def_dim, ncfile, dimname, dimlength, unlim)
  
  return(invisible(nc))
}


#-------------------------------------------------------------------------------
# dim.inq.nc()
#-------------------------------------------------------------------------------

dim.inq.nc <- function(ncfile, dimension) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(dimension) || is.numeric(dimension))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_inq_dim, ncfile, dimension)
  
  #-- Return object ----------------------------------------------------------
  names(nc) <- c("id", "name", "length", "unlim")
  return(nc)
}


#-------------------------------------------------------------------------------
# dim.rename.nc()
#-------------------------------------------------------------------------------

dim.rename.nc <- function(ncfile, dimension, newname) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(dimension) || is.numeric(dimension))
  stopifnot(is.character(newname))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_rename_dim, ncfile, dimension, newname)
  
  return(invisible(NULL))
}

