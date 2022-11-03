#===============================================================================
#
#  Name:       type.R
#
#  Purpose:    NetCDF interface for R - type functions.
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
# type.def.nc()
#-------------------------------------------------------------------------------

type.def.nc <- function(ncfile, typename, class, size=NULL, basetype=NULL,
                        names=NULL, values=NULL, subtypes=NULL, dimsizes=NULL) {
  # Check arguments
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(typename))
  stopifnot(is.character(class))
  if (isTRUE(class == "compound")) {
    stopifnot(is.character(names))
    stopifnot(is.character(subtypes) || is.numeric(subtypes))
    stopifnot(is.list(dimsizes))
  } else if (isTRUE(class == "enum")) {
    stopifnot(is.character(basetype) || is.numeric(basetype))
    stopifnot(is.character(names))
    stopifnot(is.numeric(values))
  } else if (isTRUE(class == "opaque")) {
    stopifnot(is.numeric(size))
  } else if (isTRUE(class == "vlen")) {
    stopifnot (is.character(basetype) || is.numeric(basetype))
  } else {
    stop("Unknown class for type definition", call.=FALSE)
  }

  id <- .Call(R_nc_def_type, ncfile, typename, class, size, basetype,
              names, values, subtypes, dimsizes)
  return(invisible(id))
}


#-------------------------------------------------------------------------------
# type.inq.nc()
#-------------------------------------------------------------------------------

type.inq.nc <- function(ncfile, type, fields=TRUE) {
  # Check arguments:
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.numeric(type) || is.character(type))
  stopifnot(is.logical(fields))
  
  result <- .Call(R_nc_inq_type, ncfile, type, fields)

  return(result)
}

