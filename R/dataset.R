#===============================================================================
#
#  Name:       dataset.R
#
#  Purpose:    NetCDF interface for R - dataset functions.
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
# close.nc()
#-------------------------------------------------------------------------------

close.nc <- function(con, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(con, "NetCDF"))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_close, attr(con, "handle_ptr"))
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# create.nc()
#-------------------------------------------------------------------------------

create.nc <- function(filename, clobber = TRUE, share = FALSE, prefill = TRUE, 
  format = "classic", large = FALSE, diskless = FALSE, persist = FALSE,
  mpi_comm=NULL, mpi_info=NULL) {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(filename))
  stopifnot(is.logical(clobber))
  stopifnot(is.logical(share))
  stopifnot(is.logical(prefill))
  stopifnot(is.character(format) &&
    format[1] %in% c("classic", "offset64", "data64", "netcdf4", "classic4"))
  stopifnot(is.logical(large))
  stopifnot(is.logical(diskless))
  stopifnot(is.logical(persist))
  stopifnot(is.null(mpi_comm) || is.numeric(mpi_comm))
  stopifnot(is.null(mpi_info) || is.numeric(mpi_info))

  # Handle deprecated argument:
  if (isTRUE(large) && format[1] == "classic") {
    format <- "offset64"
    warning("Argument 'large' is deprecated; please specify 'format' instead")
  }

  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_create, filename, clobber, share, prefill, format,
              diskless, persist, mpi_comm, mpi_info)
  
  attr(nc, "class") <- "NetCDF"
  return(invisible(nc))
}


#-------------------------------------------------------------------------------
# file.inq.nc()
#-------------------------------------------------------------------------------

file.inq.nc <- function(ncfile) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_inq_file, ncfile)
  
  names(nc) <- c("ndims", "nvars", "ngatts", "unlimdimid", "format", "libvers")
  
  return(nc)
}


#-------------------------------------------------------------------------------
# open.nc()
#-------------------------------------------------------------------------------

open.nc <- function(con, write = FALSE, share = FALSE, prefill = TRUE, 
                    diskless = FALSE, persist = FALSE,
                    mpi_comm=NULL, mpi_info=NULL, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(con))
  stopifnot(is.logical(write))
  stopifnot(is.logical(share))
  stopifnot(is.logical(prefill))
  stopifnot(is.logical(diskless))
  stopifnot(is.logical(persist))
  stopifnot(is.null(mpi_comm) || is.numeric(mpi_comm))
  stopifnot(is.null(mpi_info) || is.numeric(mpi_info))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_open, con, write, share, prefill,
              diskless, persist, mpi_comm, mpi_info)
  
  attr(nc, "class") <- "NetCDF"
  return(invisible(nc))
}


#-------------------------------------------------------------------------------
# sync.nc()
#-------------------------------------------------------------------------------

sync.nc <- function(ncfile) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_sync, ncfile)
  
  return(invisible(NULL))
}

