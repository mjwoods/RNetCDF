#===============================================================================
#
#  Name:       variable.R
#
#  Purpose:    NetCDF interface for R - variable functions.
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
# var.def.nc()
#-------------------------------------------------------------------------------

var.def.nc <- function(ncfile, varname, vartype, dimensions,
                       chunking=NA, chunksizes=NULL,
                       deflate=NA, shuffle=FALSE, big_endian=NA,
                       fletcher32=FALSE, filter_id=integer(0),
                       filter_params=list()) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(varname))
  stopifnot(is.character(vartype) || is.numeric(vartype))

  if (length(dimensions) == 1 && is.na(dimensions)) {
    dimensions <- integer(0)
  }

  stopifnot(is.character(dimensions) || is.numeric(dimensions))
  stopifnot(is.logical(chunking))
  stopifnot(is.null(chunksizes) || is.numeric(chunksizes))
  stopifnot(isTRUE(is.na(deflate)) || is.numeric(deflate))
  stopifnot(is.logical(shuffle))
  stopifnot(is.logical(big_endian))
  stopifnot(is.logical(fletcher32))
  stopifnot(is.numeric(filter_id))
  stopifnot(is.list(filter_params))
  stopifnot(all(sapply(filter_params, FUN=is.numeric)))
  stopifnot(length(filter_id) == length(filter_params))

  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_def_var, ncfile, varname, vartype, dimensions,
              chunking, chunksizes, deflate, shuffle, big_endian,
              fletcher32, filter_id, filter_params)
  
  return(invisible(nc))
}

#-------------------------------------------------------------------------------
# var.get.nc()
#-------------------------------------------------------------------------------

var.get.nc <- function(ncfile, variable, start = NA, count = NA, na.mode = 4, 
  collapse = TRUE, unpack = FALSE, rawchar = FALSE, fitnum = FALSE,
  cache_bytes=NA, cache_slots=NA, cache_preemption=NA) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.numeric(start) || is.logical(start))
  stopifnot(is.numeric(count) || is.logical(count))
  stopifnot(is.logical(collapse))
  stopifnot(is.logical(unpack))
  stopifnot(is.logical(rawchar))
  stopifnot(is.logical(fitnum))
  stopifnot(is.logical(cache_bytes) || is.numeric(cache_bytes))
  stopifnot(is.logical(cache_slots) || is.numeric(cache_slots))
  stopifnot(is.logical(cache_preemption) || is.numeric(cache_preemption))
  
  # Truncate start & count and replace NA as described in the man page:
  varinfo <- var.inq.nc(ncfile, variable)
  ndims <- varinfo$ndims

  if (isTRUE(is.na(start))) {
    start <- rep(1, ndims)
  } else if (length(start) > ndims) {
    start <- start[seq_len(ndims)]
  }
  stopifnot(length(start) == ndims)
  start[is.na(start)] <- 1

  if (isTRUE(is.na(count))) {
    count <- rep(NA, ndims)
  } else if (length(count) > ndims) {
    count <- count[seq_len(ndims)]
  }
  stopifnot(length(count) == ndims)
  for (idim in seq_len(ndims)) {
    if (is.na(count[idim])) {
      diminfo <- dim.inq.nc(ncfile, varinfo$dimids[idim])
      count[idim] <- ( diminfo$length - start[idim] + 1 )
    }
  }

  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_get_var, ncfile, variable, start, count,
              rawchar, fitnum, na.mode, unpack,
              cache_bytes, cache_slots, cache_preemption)

  if (inherits(nc, "integer64") &&
      !requireNamespace("bit64", quietly=TRUE)) {
    stop("Package 'bit64' required for class 'integer64'")
  }

  #-- Collapse singleton dimensions --------------------------------------
  if (isTRUE(collapse) && !is.null(dim(nc))) {
    nc <- drop(nc)
  }

  return(nc)
}


#-------------------------------------------------------------------------------
# var.inq.nc()
#-------------------------------------------------------------------------------

var.inq.nc <- function(ncfile, variable) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_inq_var, ncfile, variable)

  if (length(nc) == 6) {
    names(nc) <- c("id", "name", "type", "ndims", "dimids", "natts")
  } else {
    names(nc) <- c("id", "name", "type", "ndims", "dimids", "natts",
                   "chunksizes", "cache_bytes", "cache_slots",
                   "cache_preemption", "deflate", "shuffle", "big_endian",
                   "fletcher32", "szip_options", "szip_bits",
                   "filter_id", "filter_params")
  }
  
  return(nc)
}


#-------------------------------------------------------------------------------
# var.par.nc()
#-------------------------------------------------------------------------------

var.par.nc <- function(ncfile, variable, access="NC_COLLECTIVE") {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(access) &&
            access[1] %in% c("NC_COLLECTIVE", "NC_INDEPENDENT"))

  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_par_var, ncfile, variable, access)

  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# var.put.nc()
#-------------------------------------------------------------------------------

var.put.nc <- function(ncfile, variable, data, start = NA, count = NA,
  na.mode = 4, pack = FALSE,
  cache_bytes=NA, cache_slots=NA, cache_preemption=NA) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.numeric(data) || is.character(data) || is.raw(data) ||
            is.logical(data) || is.list(data) || is.factor(data))
  stopifnot(is.numeric(start) || is.logical(start))
  stopifnot(is.numeric(count) || is.logical(count))
  stopifnot(is.logical(pack))
  stopifnot(is.logical(cache_bytes) || is.numeric(cache_bytes))
  stopifnot(is.logical(cache_slots) || is.numeric(cache_slots))
  stopifnot(is.logical(cache_preemption) || is.numeric(cache_preemption))
  
  # Determine type and dimensions of variable:
  varinfo <- var.inq.nc(ncfile, variable)
  typeinfo <- type.inq.nc(ncfile, varinfo$type)
  ndims <- varinfo$ndims
  str2char <- is.character(data) && varinfo$type == "NC_CHAR"
  opaque <- is.raw(data) && typeinfo$class == "opaque"
  compound <- is.list(data) && typeinfo$class == "compound"

  # Truncate start & count and replace NA as described in the man page:
  if (isTRUE(is.na(start))) {
    start <- rep(1, ndims)
  } else if (length(start) > ndims) {
    start <- start[seq_len(ndims)]
  }
  stopifnot(length(start) == ndims)
  start[is.na(start)] <- 1

  if (isTRUE(is.na(count))) {
    if (!is.null(dim(data))) {
      count <- dim(data)
    } else if (ndims==0 && length(data)==1) {
      count <- integer(0)
    } else if (compound) {
      # Compound type is stored as an R list,
      # and fields may have different dimensions.
      # Use dimensions from the netcdf variable instead.
      count <- rep(NA, ndims)
    } else {
      count <- length(data)
    }
    if (str2char && ndims > 0) {
      strlen <- dim.inq.nc(ncfile, varinfo$dimids[1])$length
      count <- c(strlen, count)
    } else if (opaque && length(count) > 0) {
      # Opaque items in R have an extra leading dimension
      count <- count[-1]
    }
  } else if (length(count) > ndims) {
    count <- count[seq_len(ndims)]
  }
  stopifnot(length(count) == ndims)
  for (idim in seq_len(ndims)) {
    if (is.na(count[idim])) {
      diminfo <- dim.inq.nc(ncfile, varinfo$dimids[idim])
      count[idim] <- ( diminfo$length - start[idim] + 1 )
    }
  }

  #-- Check that length of data is sufficient --------------------------------#
  if (str2char && ndims > 0) {
    numelem <- prod(count[-1])
  } else if (opaque) {
    numelem <- prod(c(typeinfo$size,count))
  } else if (compound) {
    numelem <- length(typeinfo$offset)
  } else {
    numelem <- prod(count) # Returns 1 if ndims==0 (scalar variable)
  }
  if (length(data) < numelem) {
    stop(paste("Not enough data elements (found ",length(data),
	   ", need ",numelem,")", sep=""), call.=FALSE)
  }

  #-- Warn if strings will be truncated --------------------------------------#
  if (str2char) {
    if (ndims > 0) {
      strlen <- count[1]
    } else {
      strlen <- 1
    }
    if (isTRUE(max(nchar(data,type="bytes")) > strlen)) {
      warning(paste("Strings truncated to length",strlen), call.=FALSE)
    }
  }

  #-- Warn if array data is not conformable with count -----------------------#
  if (!is.null(dim(data))) {
    if (str2char && ndims > 0) {
      count_drop <- count[-1]
    } else if (opaque) {
      count_drop <- c(typeinfo$size,count)
    } else {
      count_drop <- count
    }
    count_drop <- count_drop[count_drop!=1]

    dim_drop <- dim(data)
    dim_drop <- dim_drop[dim_drop!=1]

    if ((length(count_drop) != length(dim_drop)) || 
	any(count_drop != dim_drop)) {
      warning(paste("Data coerced from dimensions (",
		paste(dim(data),collapse=","), ") to dimensions (",
		paste(count,collapse=","), ")", sep=""), call.=FALSE)
    }
  }

  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_put_var, ncfile, variable, start, count, data,
              na.mode, pack,
              cache_bytes, cache_slots, cache_preemption)
 
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# var.rename.nc()
#-------------------------------------------------------------------------------

var.rename.nc <- function(ncfile, variable, newname) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(newname))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call(R_nc_rename_var, ncfile, variable, newname)
  
  return(invisible(NULL))
}


