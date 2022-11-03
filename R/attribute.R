#===============================================================================
#
#  Name:       attribute.R
#
#  Purpose:    NetCDF interface for R - attribute functions.
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
# att.copy.nc()
#-------------------------------------------------------------------------------

att.copy.nc <- function(ncfile.in, variable.in, attribute, ncfile.out, variable.out) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile.in, "NetCDF"))
  stopifnot(inherits(ncfile.out, "NetCDF"))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  stopifnot(is.character(variable.in) || is.numeric(variable.in))
  stopifnot(is.character(variable.out) || is.numeric(variable.out))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_copy_att, ncfile.in, variable.in, attribute,
              ncfile.out, variable.out)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# att.delete.nc()
#-------------------------------------------------------------------------------

att.delete.nc <- function(ncfile, variable, attribute) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_delete_att, ncfile, variable, attribute)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# att.get.nc()
#-------------------------------------------------------------------------------

att.get.nc <- function(ncfile, variable, attribute,
                       rawchar = FALSE, fitnum = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  stopifnot(is.logical(rawchar))
  stopifnot(is.logical(fitnum))

  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_get_att, ncfile, variable, attribute, rawchar, fitnum)

  if (inherits(nc, "integer64") &&
      !requireNamespace("bit64", quietly=TRUE)) {
    stop("Package 'bit64' required for class 'integer64'")
  }

  return(nc)
}


#-------------------------------------------------------------------------------
# att.inq.nc()
#-------------------------------------------------------------------------------

att.inq.nc <- function(ncfile, variable, attribute) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_inq_att, ncfile, variable, attribute)
  
  names(nc) <- c("id", "name", "type", "length")
  return(nc)
}


#-------------------------------------------------------------------------------
# att.put.nc()
#-------------------------------------------------------------------------------

att.put.nc <- function(ncfile, variable, name, type, value) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(name))
  stopifnot(is.character(type) || is.numeric(type))
  stopifnot(is.numeric(value) || is.character(value) || is.raw(value) ||
            is.logical(value) || is.list(value) || is.factor(value))

  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_put_att, ncfile, variable, name, type, value)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# att.rename.nc()
#-------------------------------------------------------------------------------

att.rename.nc <- function(ncfile, variable, attribute, newname) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  stopifnot(is.character(newname))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_rename_att, ncfile, variable, attribute, newname)
  
  return(invisible(NULL))
}

