#===============================================================================
#
#  Name:       group.R
#
#  Purpose:    NetCDF interface for R - group functions.
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
# grp.def.nc()
#-------------------------------------------------------------------------------

grp.def.nc <- function(ncid, grpname) {
  # Check arguments:
  stopifnot(inherits(ncid, "NetCDF"))
  stopifnot(is.character(grpname))
  
  # C function call:
  nc <- .Call(R_nc_def_grp, ncid, grpname)
  
  # Return object:
  attributes(nc) <- attributes(ncid)
  return(invisible(nc))
}


#-------------------------------------------------------------------------------
# grp.find() (internal only)
#-------------------------------------------------------------------------------

grp.find <- function(ncid, grpname, full = isTRUE(grepl("/", grpname))) {
  # Check arguments:
  stopifnot(inherits(ncid, "NetCDF"))
  stopifnot(is.character(grpname))
  stopifnot(is.logical(full))
  
  # C function call:
  nc <- .Call(R_nc_inq_grp_ncid, ncid, grpname, full)
  
  # Return object:
  attributes(nc) <- attributes(ncid)
  return(nc)
}


#-------------------------------------------------------------------------------
# grp.inq.nc()
#-------------------------------------------------------------------------------

grp.inq.nc <- function(ncid, grpname = NULL, ancestors = TRUE) {
  # Check arguments:
  stopifnot(inherits(ncid, "NetCDF"))
  stopifnot(is.logical(ancestors))
  stopifnot(is.null(grpname) || is.character(grpname))
  
  # If optional argument is specified, find a group by name:
  if (!is.null(grpname)) {
    ncid <- grp.find(ncid, grpname)
  }
  
  # Initialise output list:
  out <- list()
  out$self <- ncid
  
  # Get parent of group (NULL if none):
  pgrp <- try(.Call(R_nc_inq_grp_parent, ncid), silent = TRUE)
  if (!inherits(pgrp, "try-error")) {
    attributes(pgrp) <- attributes(ncid)
    out$parent <- pgrp
  }
  
  # Get sub-groups of group (empty list if none):
  grpids <- try(.Call(R_nc_inq_grps, ncid), silent = TRUE)
  if (inherits(grpids, "try-error")) {
    out$grps <- list()
  } else {
    out$grps <- lapply(as.list(grpids), function(x) {
      attributes(x) <- attributes(ncid)
      return(x)
    })
  }
  
  # Names of group:
  out$name <- .Call(R_nc_inq_grpname, ncid, FALSE)
  if (isTRUE(ancestors)) {
    out$fullname <- .Call(R_nc_inq_grpname, ncid, TRUE)
  }
  
  # Dimensions visible in group (empty vector if none):
  out$dimids <- .Call(R_nc_inq_dimids, ncid, ancestors)
  
  # Unlimited dimensions visible in group (empty vector if none):
  out$unlimids <- .Call(R_nc_inq_unlimids, ncid)
  
  # Variables in group (empty vector if none):
  out$varids <- .Call(R_nc_inq_varids, ncid)
  
  # Types in group (empty vector if none):
  out$typeids <- .Call(R_nc_inq_typeids, ncid)
  
  # Number of group attributes:
  out$ngatts <- .Call(R_nc_inq_natts, ncid)
  
  return(out)
}


#-------------------------------------------------------------------------------
# grp.rename.nc()
#-------------------------------------------------------------------------------

grp.rename.nc <- function(ncid, newname, oldname = NULL) {
  # Check arguments:
  stopifnot(inherits(ncid, "NetCDF"))
  stopifnot(is.character(newname))
  stopifnot(is.null(oldname) || is.character(oldname))
  
  # If optional argument is specified, find a group by name:
  if (!is.null(oldname)) {
    ncid <- grp.find(ncid, oldname)
  }
  
  # C function call:
  nc <- .Call(R_nc_rename_grp, ncid, newname)
  
  return(invisible(NULL))
}

