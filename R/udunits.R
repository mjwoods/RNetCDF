#===============================================================================
#
#  Name:       udunits.R
#
#  Purpose:    NetCDF interface for R - udunits functions.
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
# utcal.nc()
#-------------------------------------------------------------------------------

utcal.nc <- function(unitstring, value, type = "n") {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(unitstring))
  stopifnot(is.numeric(value))
  stopifnot(type == "n" || type == "s" || type == "c")
  
  #-- C function call to udunits calendar function -----------------------
  ut <- .Call(R_nc_calendar, unitstring, value)
  
  #-- Return object if no error ------------------------------------------
  if (isTRUE(type == "n")) {
    colnames(ut) <- c("year", "month", "day", "hour", "minute", "second")
    return(ut)
  } else if (isTRUE(type == "s")) {
    x <- sprintf("%02g-%02g-%02g %02g:%02g:%02g",ut[,1],ut[,2],ut[,3],ut[,4],ut[,5],ut[,6])
    return(x)
  } else if (isTRUE(type == "c")) {
    ct <- utinvcal.nc("seconds since 1970-01-01 00:00:00 +00:00", ut)
    attr(ct, "class") <- c("POSIXct","POSIXt")
    attr(ct, "tzone") <- "UTC"
    return(ct)
  }
}


#-------------------------------------------------------------------------------
# utinit.nc()
#-------------------------------------------------------------------------------

utinit.nc <- function(path = "") {
  stopifnot(is.character(path) && length(path) > 0)

  if (nchar(path[1]) == 0) {

    # Check environment for database requested by user:
    envdb <- Sys.getenv("UDUNITS2_XML_PATH", unset=NA)

    if (is.na(envdb)) {
      # Initialise unit system with database packaged in RNetCDF:
      path <- system.file("udunits", "udunits2.xml",
                          package="RNetCDF", mustWork=TRUE)
    } else {
      # Initialise udunits2 library with user-specified database:
      path <- envdb
    }
  }

  ut <- .Call(R_nc_utinit, path)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# utinvcal.nc()
#-------------------------------------------------------------------------------

utinvcal.nc <- function(unitstring, value) {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(unitstring))
  
  if (is.character(value)) {
    stopifnot(isTRUE(all(nchar(value) == 19)))
    value <- cbind(substr(value, 1, 4), substr(value, 6, 7), substr(value, 
      9, 10), substr(value, 12, 13), substr(value, 15, 16), substr(value, 
      18, 19))
    
    value <- matrix(as.numeric(value), ncol = 6)
  } else if (inherits(value, "POSIXct")) {
    value <- utcal.nc("seconds since 1970-01-01 00:00:00 +00:00", as.vector(value), "n") 
  }
  
  stopifnot(is.numeric(value))
  
  #-- C function call --------------------------------------------------------
  ut <- .Call(R_nc_inv_calendar, unitstring, value)
  return(ut)
}


