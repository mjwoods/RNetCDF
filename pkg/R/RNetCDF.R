#===============================================================================
#										
#  Name:       RNetCDF.R							
#										
#  Version:    2.0-1								
#										
#  Purpose:    NetCDF interface for R.						
#										
#  Author:     Pavel Michna (michna@giub.unibe.ch)				
#              Milton Woods (m.woods@bom.gov.au)                                
#										
#  Copyright:  (C) 2004-2016 Pavel Michna					
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
#  You should have received a copy of the GNU General Public License		
#  along with this program; if not, write to the Free Software			
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA	
#										
#===============================================================================
#  Implementation and Revisions 						
#-------------------------------------------------------------------------------
#  Author   Date       Description						
#  ------   ----       -----------						
#  pm       12/06/04   First implementation					
#  pm       09/07/04   Support scalar variables	    		                
#  pm       21/07/04   Changed error handling					
#  pm       28/07/04   Minor modifications					
#  pm       12/09/04   New na.mode=3 and collapse=TRUE/FALSE in var.get.nc()	
#  pm       24/07/06   Handling dates in string form (udunits)           	
#  mw       14/04/08   Added new modes (large, prefill, share)                  
#                      to nc_open and nc_create                                 
#  pm       24/11/10   Added new option enddef to att and dim/var definitions   
#  pm       01/12/10   Removed option enddef, checking in C code for mode       
#  pm       14/02/12   Corrected bug in att.delete.nc                           
#  pm       02/06/12   Added function read.nc()                                 
#  pm       16/07/12   Added packing/unpacking of data (code from mw)           
#  mw       21/08/14   Allow reading of character vector or scalar              
#  mw       05/09/14   Support reading and writing raw character arrays         
#  mw       08/09/14   Handle reading and writing of zero-sized arrays          
#  mw       24/01/16   Support conversion of timestamps to/from POSIXct         
#  mw       24/02/16   Support creation of files in netcdf4 (hdf5) format       
#  mw       21/05/16   Add functions for netcdf4 groups                         
#										
#===============================================================================

# ===============================================================================
# Private utility functions
# ===============================================================================

Cwrap <- function(..., PACKAGE = "RNetCDF", ERRNULL = FALSE) {
  # Invoke C routine and check result for errors.  On success, return
  # object created by C routine, otherwise raise an exception (default) or
  # return NULL if ERRNULL is TRUE.
  nc <- .Call(..., PACKAGE = PACKAGE)
  if (nc[[1]] != 0) {
    if (ERRNULL) {
      return(NULL)
    } else {
      stop(nc[[2]], call. = FALSE)
    }
  }
  return(nc[[3]])
}


# ===============================================================================
# NetCDF library functions
# ===============================================================================

#-------------------------------------------------------------------------------
# att.copy.nc()
#-------------------------------------------------------------------------------

att.copy.nc <- function(ncfile.in, variable.in, attribute, ncfile.out, variable.out) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile.in) == "NetCDF")
  stopifnot(class(ncfile.out) == "NetCDF")
  stopifnot(is.character(attribute) || is.numeric(attribute))
  stopifnot(is.character(variable.in) || is.numeric(variable.in))
  stopifnot(is.character(variable.out) || is.numeric(variable.out))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_copy_att", ncfile.in, variable.in, attribute,
              ncfile.out, variable.out)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# att.delete.nc()
#-------------------------------------------------------------------------------

att.delete.nc <- function(ncfile, variable, attribute) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_delete_att", ncfile, variable, attribute)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# att.get.nc()
#-------------------------------------------------------------------------------

att.get.nc <- function(ncfile, variable, attribute) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_get_att", ncfile, variable, attribute)

  return(nc)
}


#-------------------------------------------------------------------------------
# att.inq.nc()
#-------------------------------------------------------------------------------

att.inq.nc <- function(ncfile, variable, attribute) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_inq_att", ncfile, variable, attribute)
  
  names(nc) <- c("id", "name", "type", "length")
  return(nc)
}


#-------------------------------------------------------------------------------
# att.put.nc()
#-------------------------------------------------------------------------------

att.put.nc <- function(ncfile, variable, name, type, value) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(name))
  stopifnot(is.character(type))
  stopifnot(is.character(value) || is.numeric(value))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_put_att", ncfile, variable, name, type, value)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# att.rename.nc()
#-------------------------------------------------------------------------------

att.rename.nc <- function(ncfile, variable, attribute, newname) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  stopifnot(is.character(newname))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_rename_att", ncfile, variable, attribute, newname)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# close.nc()
#-------------------------------------------------------------------------------

close.nc <- function(con, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(con) == "NetCDF")
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_close", attr(con, "handle_ptr"))
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# create.nc()
#-------------------------------------------------------------------------------

create.nc <- function(filename, clobber = TRUE, share = FALSE, prefill = TRUE, 
  format = "classic", large = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(filename))
  stopifnot(is.logical(clobber))
  stopifnot(is.logical(share))
  stopifnot(is.logical(prefill))
  stopifnot(is.character(format))
  stopifnot(is.logical(large))

  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_create", filename, clobber, share, prefill, format)
  
  attr(nc, "class") <- "NetCDF"
  return(nc)
}


#-------------------------------------------------------------------------------
# dim.def.nc()
#-------------------------------------------------------------------------------

dim.def.nc <- function(ncfile, dimname, dimlength = 1, unlim = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(dimname))
  stopifnot(is.numeric(dimlength))
  stopifnot(is.logical(unlim))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_def_dim", ncfile, dimname, dimlength, unlim)
  
  return(nc)
}


#-------------------------------------------------------------------------------
# dim.inq.nc()
#-------------------------------------------------------------------------------

dim.inq.nc <- function(ncfile, dimension) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(dimension) || is.numeric(dimension))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_inq_dim", ncfile, dimension)
  
  #-- Return object ----------------------------------------------------------
  names(nc) <- c("id", "name", "length", "unlim")
  return(nc)
}


#-------------------------------------------------------------------------------
# dim.rename.nc()
#-------------------------------------------------------------------------------

dim.rename.nc <- function(ncfile, dimension, newname) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(dimension) || is.numeric(dimension))
  stopifnot(is.character(newname))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_rename_dim", ncfile, dimension, newname)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# file.inq.nc()
#-------------------------------------------------------------------------------

file.inq.nc <- function(ncfile) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_inq_file", ncfile)
  
  names(nc) <- c("ndims", "nvars", "ngatts", "unlimdimid", "format")
  
  return(nc)
}


#-------------------------------------------------------------------------------
# open.nc()
#-------------------------------------------------------------------------------

open.nc <- function(con, write = FALSE, share = FALSE, prefill = TRUE, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(con))
  stopifnot(is.logical(write))
  stopifnot(is.logical(share))
  stopifnot(is.logical(prefill))
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_open", con, write, share, prefill)
  
  attr(nc, "class") <- "NetCDF"
  return(nc)
}


#-------------------------------------------------------------------------------
# print.nc()
#-------------------------------------------------------------------------------

# Private function to print metadata of groups recursively:
print_grp <- function(x, level = 0) {
  
  indent <- paste(rep("  ", level), collapse = "")
  
  #-- Inquire about the group ------------------------------------------------
  grpinfo <- try(grp.inq.nc(x, ancestors = FALSE))
  if (class(grpinfo) == "try-error" || is.null(grpinfo)) {
    return(invisible(NULL))
  }
  
  #-- Inquire about all dimensions -------------------------------------------
  if (length(grpinfo$dimids) != 0) {
    cat(indent, "dimensions:\n", sep = "")
    for (id in grpinfo$dimids) {
      diminfo <- dim.inq.nc(x, id)
      if (diminfo$unlim == FALSE) {
        cat(indent, "        ", diminfo$name, " = ", diminfo$length, 
          " ;\n", sep = "")
      } else {
        cat(indent, "        ", diminfo$name, " = UNLIMITED ; // (", 
          diminfo$length, " currently)\n", sep = "")
      }
    }
  }
  
  #-- Inquire about all variables --------------------------------------------
  if (length(grpinfo$varids) != 0) {
    cat(indent, "variables:\n", sep = "")
    for (id in grpinfo$varids) {
      varinfo <- var.inq.nc(x, id)
      vartype <- substring(tolower(varinfo$type), 4)
      cat(indent, "        ", vartype, " ", varinfo$name, sep = "")
      if (varinfo$ndims > 0) {
        cat("(")
        for (jj in seq_len(varinfo$ndims - 1)) {
          cat(dim.inq.nc(x, varinfo$dimids[jj])$name, ", ", sep = "")
        }
        cat(dim.inq.nc(x, varinfo$dimids[varinfo$ndims])$name, sep = "")
        cat(")")
      }
      cat(" ;\n")
      
      #-- Inquire about variable attributes ------------------------------
      if (varinfo$natts != 0) {
        for (jj in 0:(varinfo$natts - 1)) {
          attinfo <- att.inq.nc(x, id, jj)
          cat(indent, rep(" ", 16), varinfo$name, ":", attinfo$name, 
          sep = "")
          if (attinfo$type == "NC_CHAR") {
          cat(" = \"", att.get.nc(x, id, jj), "\" ;\n", sep = "")
          } else {
          cat(" = ", att.get.nc(x, id, jj), " ;\n", sep = "")
          }
        }
      }
    }
  }
  
  #-- Inquire about global attributes ----------------------------------------
  if (grpinfo$ngatts != 0) {
    cat("\n", indent, "// global attributes:\n", sep = "")
    id <- "NC_GLOBAL"
    for (jj in 0:(grpinfo$ngatts - 1)) {
      attinfo <- att.inq.nc(x, id, jj)
      cat(indent, rep(" ", 16), ":", attinfo$name, sep = "")
      if (attinfo$type == "NC_CHAR") {
        cat(" = \"", att.get.nc(x, id, jj), "\" ;\n", sep = "")
      } else {
        cat(" = ", att.get.nc(x, id, jj), " ;\n", sep = "")
      }
    }
  }
  
  #-- Print groups recursively -----------------------------------------------
  if (length(grpinfo$grps) != 0) {
    for (id in grpinfo$grps) {
      subgrpinfo <- grp.inq.nc(id, ancestors = FALSE)
      cat("\n", indent, "group: ", subgrpinfo$name, " {\n", sep = "")
      print_grp(id, level = (level + 1))
      cat(indent, "  } // group ", subgrpinfo$name, "\n", sep = "")
    }
  }
}

print.nc <- function(x, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(x) == "NetCDF")
  
  # Display groups recursively:
  print_grp(x, level = 0)
}


#-------------------------------------------------------------------------------
# sync.nc()
#-------------------------------------------------------------------------------

sync.nc <- function(ncfile) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_sync", ncfile)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# var.def.nc()
#-------------------------------------------------------------------------------

var.def.nc <- function(ncfile, varname, vartype, dimensions) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(varname))
  stopifnot(is.character(vartype))

  if (length(dimensions) == 1 && is.na(dimensions)) {
    dimensions <- integer(0)
  }

  stopifnot(is.character(dimensions) || is.numeric(dimensions))

  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_def_var", ncfile, varname, vartype, dimensions)
  
  return(nc)
}

#-------------------------------------------------------------------------------
# var.get.nc()
#-------------------------------------------------------------------------------

var.get.nc <- function(ncfile, variable, start = NA, count = NA, na.mode = 0, 
  collapse = TRUE, unpack = FALSE, rawchar = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.numeric(start) || is.logical(start))
  stopifnot(is.numeric(count) || is.logical(count))
  stopifnot(is.logical(collapse))
  stopifnot(is.logical(unpack))
  stopifnot(is.logical(rawchar))
  
  stopifnot(isTRUE(na.mode %in% c(0, 1, 2, 3)))
  
  #-- Inquire the variable ---------------------------------------------------
  varinfo <- try(var.inq.nc(ncfile, variable))
  
  if (class(varinfo) == "try-error" || is.null(varinfo)) {
    return(invisible(NULL))
  }
  
  ndims <- varinfo$ndims
  
  #-- Get the varid as integer if necessary ----------------------------------
  ifelse(is.character(variable), varid <- varinfo$id, varid <- variable)
  
  #-- Replace NA by defined limits -------------------------------------------
  if (isTRUE(is.na(start))) {
    start <- rep(NA, ndims)
  }
  if (isTRUE(is.na(count))) {
    count <- rep(NA, ndims)
  }
  
  if (length(start) != ndims || length(count) != ndims) {
    stop("Length of start/count is not ndims", call. = FALSE)
  }
  
  start[is.na(start)] <- 1
  for (idim in seq_len(ndims)) {
    if (is.na(count[idim])) {
      count[idim] <- dim.inq.nc(ncfile, varinfo$dimids[idim])$length
    }
  }
  
  #-- Switch from R to C convention ------------------------------------------
  c.start <- rev(start - 1)
  c.count <- rev(count)
  
  #-- C function calls -------------------------------------------------------
  if (varinfo$type == "NC_CHAR") {
    nc <- Cwrap("R_nc_get_vara_text", as.integer(ncfile), as.integer(varid), 
      as.integer(c.start), as.integer(c.count), as.integer(ndims), 
      as.integer(rawchar))
  } else {
    nc <- Cwrap("R_nc_get_vara_double", as.integer(ncfile), as.integer(varid), 
      as.integer(c.start), as.integer(c.count), as.integer(ndims))
  }
  
  #-- Convert missing value to NA if defined in NetCDF file --------------
  if (is.numeric(nc) && na.mode < 3) {
    tolerance <- 1 * 10^-5  ## Allow rounding error
    na.flag <- 0
    
    missval.flag <- 0
    fillval.flag <- 0
    
    fillval <- try(att.inq.nc(ncfile, varinfo$name, "_FillValue"), silent = TRUE)
    missval <- try(att.inq.nc(ncfile, varinfo$name, "missing_value"), 
      silent = TRUE)
    
    if (!(class(fillval) == "try-error")) {
      if (!is.null(fillval)) {
        fillval.flag <- 1
      }
    }
    if (!(class(missval) == "try-error")) {
      if (!is.null(missval)) {
        missval.flag <- 1
      }
    }
    
    if (na.mode == 0 && missval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "missing_value")
      na.flag <- 1
    }
    if (na.mode == 0 && fillval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "_FillValue")
      na.flag <- 1
    }
    
    if (na.mode == 1 && fillval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "_FillValue")
      na.flag <- 1
    }
    
    if (na.mode == 2 && missval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "missing_value")
      na.flag <- 1
    }
    
    if (na.flag == 1) {
      nc[abs(nc - as.numeric(na.value)) < tolerance] <- NA
    }
  }
  
  #-- Unpack variables if requested (missing values are preserved) -------
  if (unpack && is.numeric(nc)) {
    offset <- try(att.inq.nc(ncfile, varinfo$name, "add_offset"), silent = TRUE)
    scale <- try(att.inq.nc(ncfile, varinfo$name, "scale_factor"), silent = TRUE)
    if ((!inherits(offset, "try-error")) && (!inherits(scale, "try-error"))) {
      add_offset <- att.get.nc(ncfile, varinfo$name, "add_offset")
      scale_factor <- att.get.nc(ncfile, varinfo$name, "scale_factor")
      nc <- nc * scale_factor + add_offset
    }
  }
  
  #-- Set dimensions, collapse degenerate dimensions ---------------------
  if (is.character(nc) && ndims > 0) {
    # Drop string length dimension
    datadim <- count[-1]
  } else {
    datadim <- count
  }
  if (collapse) {
    # Drop singleton dimensions
    datadim <- datadim[datadim != 1]
  }
  if (length(datadim) < 1) {
    # For compatibility with code written for RNetCDF<=1.6.x, scalars and
    # vectors always have a dimension attribute:
    datadim <- length(nc)
  }
  dim(nc) <- datadim
  
  return(nc)
}


#-------------------------------------------------------------------------------
# var.inq.nc()
#-------------------------------------------------------------------------------

var.inq.nc <- function(ncfile, variable) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  
  #-- Look if handle variable by name or ID ----------------------------------
  varid <- -1
  varname <- ""
  
  ifelse(is.character(variable), nameflag <- 1, nameflag <- 0)
  ifelse(is.character(variable), varname <- variable, varid <- variable)
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_inq_var", as.integer(ncfile), as.integer(varid), as.character(varname), 
    as.integer(nameflag))
  
  names(nc) <- c("id", "name", "type", "ndims", "dimids", "natts")
  
  if (nc$ndims > 0) {
    nc$dimids <- nc$dimids[(nc$ndims):1]  ## C to R convention
  } else {
    nc$dimids <- NA
  }
  
  return(nc)
}


#-------------------------------------------------------------------------------
# var.put.nc()
#-------------------------------------------------------------------------------

var.put.nc <- function(ncfile, variable, data, start = NA, count = NA, na.mode = 0, 
  pack = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.numeric(data) || is.character(data) || is.raw(data) || is.logical(data))
  stopifnot(is.numeric(start) || is.logical(start))
  stopifnot(is.numeric(count) || is.logical(count))
  stopifnot(is.logical(pack))
  
  stopifnot(isTRUE(na.mode %in% c(0, 1, 2)))
  
  #-- Inquire the variable ---------------------------------------------------
  varinfo <- try(var.inq.nc(ncfile, variable))
  
  if (class(varinfo) == "try-error" || is.null(varinfo)) {
    return(invisible(NULL))
  }
  
  ndims <- varinfo$ndims
  
  if ((is.character(data) || is.raw(data)) && varinfo$type != "NC_CHAR") {
    stop("R character data can only be written to NC_CHAR variable", 
      call. = FALSE)
  }
  
  #-- Get the varid as integer if necessary ----------------------------------
  ifelse(is.character(variable), varid <- varinfo$id, varid <- variable)
  
  #-- Get correct mode (numeric/character) if data contain only NAs ----------
  if (is.logical(data)) {
    if (varinfo$type == "NC_CHAR") {
      mode(data) <- "character"
    } else {
      mode(data) <- "numeric"
    }
  }
  
  #-- Check length of character strings --------------------------------------
  if (is.character(data)) {
    if (ndims > 0) {
      strlen <- dim.inq.nc(ncfile, varinfo$dimids[1])$length
    } else {
      strlen <- 1
    }
    if (max(nchar(data, type = "bytes")) > strlen) {
      stop("String length exceeds netcdf dimension", call. = FALSE)
    }
  }
  
  #-- Replace NA by dimensions of data ---------------------------------------
  if (any(is.na(start))) {
    start <- rep(1, ndims)
  }
  
  if (any(is.na(count))) {
    if (!is.null(dim(data))) {
      count <- dim(data)
    } else if (ndims > 0) {
      count <- length(data)
    } else {
      count <- integer(0)
    }
    if (is.character(data) && ndims > 0) {
      count <- c(strlen, count)
    }
  }
  
  if (length(start) != ndims || length(count) != ndims) {
    stop("Length of start/count is not ndims", call. = FALSE)
  }
  
  #-- Check that length of data and count match ------------------------------
  if (is.character(data) && ndims > 0) {
    numelem <- prod(count[-1])
  } else {
    numelem <- prod(count)
  }
  if (length(data) != numelem) {
    stop("Mismatch between count and length(data)", call. = FALSE)
  }
  
  #-- Pack variables if requested (missing values are preserved) -------------
  if (pack && is.numeric(data)) {
    offset <- try(att.inq.nc(ncfile, varinfo$name, "add_offset"), silent = TRUE)
    scale <- try(att.inq.nc(ncfile, varinfo$name, "scale_factor"), silent = TRUE)
    if ((!inherits(offset, "try-error")) && (!inherits(scale, "try-error"))) {
      add_offset <- att.get.nc(ncfile, varinfo$name, "add_offset")
      scale_factor <- att.get.nc(ncfile, varinfo$name, "scale_factor")
      data <- (data - add_offset) * (1/scale_factor)
    }
  }
  
  #-- Convert missing value to NA if defined in NetCDF file ------------------
  if (is.numeric(data) && any(is.na(data))) {
    na.flag <- 0
    
    missval.flag <- 0
    fillval.flag <- 0
    
    fillval <- try(att.inq.nc(ncfile, varinfo$name, "_FillValue"), silent = TRUE)
    missval <- try(att.inq.nc(ncfile, varinfo$name, "missing_value"), 
      silent = TRUE)
    
    if (!(class(fillval) == "try-error")) {
      if (!is.null(fillval)) {
        fillval.flag <- 1
      }
    }
    if (!(class(missval) == "try-error")) {
      if (!is.null(missval)) {
        missval.flag <- 1
      }
    }
    
    if (na.mode == 0 && missval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "missing_value")
      na.flag <- 1
    }
    if (na.mode == 0 && fillval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "_FillValue")
      na.flag <- 1
    }
    
    if (na.mode == 1 && fillval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "_FillValue")
      na.flag <- 1
    }
    
    if (na.mode == 2 && missval.flag == 1) {
      na.value <- att.get.nc(ncfile, varinfo$name, "missing_value")
      na.flag <- 1
    }
    
    if (na.flag == 1) {
      data[is.na(data)] <- as.numeric(na.value)
    } else {
      stop("Found NAs but no missing value attribute", call. = FALSE)
    }
  }
  
  #-- Switch from R to C convention ------------------------------------------
  c.start <- rev(start - 1)
  c.count <- rev(count)
  
  #-- C function calls -------------------------------------------------------
  if (is.numeric(data)) {
    if (!is.double(data)) {
      data <- as.double(data)
    }
    nc <- Cwrap("R_nc_put_vara_double", as.integer(ncfile), as.integer(varid), 
      as.integer(c.start), as.integer(c.count), as.integer(ndims), 
      data)
  } else {
    stopifnot(is.character(data) || is.raw(data))
    nc <- Cwrap("R_nc_put_vara_text", as.integer(ncfile), as.integer(varid), 
      as.integer(c.start), as.integer(c.count), as.integer(ndims), 
      as.integer(is.raw(data)), data)
  }
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# var.rename.nc()
#-------------------------------------------------------------------------------

var.rename.nc <- function(ncfile, variable, newname) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(newname))
  
  #-- Look if handle variable by name or ID ----------------------------------
  varid <- -1
  varname <- ""
  
  ifelse(is.character(variable), nameflag <- 1, nameflag <- 0)
  ifelse(is.character(variable), varname <- variable, varid <- variable)
  
  #-- C function call --------------------------------------------------------
  nc <- Cwrap("R_nc_rename_var", as.integer(ncfile), as.integer(varid), 
    as.character(varname), as.integer(nameflag), as.character(newname))
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# grp.def.nc()
#-------------------------------------------------------------------------------

grp.def.nc <- function(ncid, grpname) {
  # Check arguments:
  stopifnot(class(ncid) == "NetCDF")
  stopifnot(is.character(grpname))
  
  # C function call:
  nc <- Cwrap("R_nc_def_grp", as.integer(ncid), grpname, PACKAGE = "RNetCDF")
  
  # Return object:
  attributes(nc) <- attributes(ncid)
  return(nc)
}


#-------------------------------------------------------------------------------
# grp.find() (internal only)
#-------------------------------------------------------------------------------

grp.find <- function(ncid, grpname, full = isTRUE(grepl("/", grpname))) {
  # Check arguments:
  stopifnot(class(ncid) == "NetCDF")
  stopifnot(is.character(grpname))
  stopifnot(is.logical(full))
  
  # C function call:
  nc <- Cwrap("R_nc_inq_grp_ncid", as.integer(ncid), grpname, as.integer(full))
  
  # Return object:
  attributes(nc) <- attributes(ncid)
  return(nc)
}


#-------------------------------------------------------------------------------
# grp.inq.nc()
#-------------------------------------------------------------------------------

grp.inq.nc <- function(ncid, grpname = NULL, ancestors = TRUE) {
  # Check arguments:
  stopifnot(class(ncid) == "NetCDF")
  stopifnot(is.logical(ancestors))
  
  # If optional argument is specified, find a group by name:
  if (is.character(grpname)) {
    ncid <- grp.find(ncid, grpname)
  }
  
  # Initialise output list:
  out <- list()
  out$self <- ncid
  
  # Get parent of group (NULL if none):
  out$parent <- Cwrap("R_nc_inq_grp_parent", as.integer(ncid), ERRNULL = TRUE)
  if (!is.null(out$parent)) {
    attributes(out$parent) <- attributes(ncid)
  }
  
  # Get sub-groups of group (empty list if none):
  grpids <- Cwrap("R_nc_inq_grps", as.integer(ncid), ERRNULL = TRUE)
  if (is.null(grpids)) {
    out$grps <- list()
  } else {
    out$grps <- lapply(as.list(grpids), function(x) {
      attributes(x) <- attributes(ncid)
      return(x)
    })
  }
  
  # Names of group:
  out$name <- Cwrap("R_nc_inq_grpname", as.integer(ncid), as.integer(FALSE))
  if (ancestors) {
    out$fullname <- Cwrap("R_nc_inq_grpname", as.integer(ncid), as.integer(TRUE))
  }
  
  # Dimensions visible in group (empty vector if none):
  out$dimids <- Cwrap("R_nc_inq_dimids", as.integer(ncid), as.integer(ancestors))
  
  # Unlimited dimensions visible in group (empty vector if none):
  out$unlimids <- Cwrap("R_nc_inq_unlimids", as.integer(ncid), as.integer(ancestors))
  
  # Variables in group (empty vector if none):
  out$varids <- Cwrap("R_nc_inq_varids", as.integer(ncid))
  
  # Types in group (empty vector if none):
  out$typeids <- Cwrap("R_nc_inq_typeids", as.integer(ncid))
  
  # Number of group attributes:
  out$ngatts <- Cwrap("R_nc_inq_natts", as.integer(ncid))
  
  return(out)
}


#-------------------------------------------------------------------------------
# grp.rename.nc()
#-------------------------------------------------------------------------------

grp.rename.nc <- function(ncid, newname, oldname = NULL) {
  # Check arguments:
  stopifnot(class(ncid) == "NetCDF")
  stopifnot(is.character(newname))
  
  # If optional argument is specified, find a group by name:
  if (is.character(oldname)) {
    ncid <- grp.find(ncid, oldname)
  }
  
  # C function call:
  nc <- Cwrap("R_nc_rename_grp", as.integer(ncid), newname)
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# read.nc()
#-------------------------------------------------------------------------------

read.nc <- function(ncfile, recursive = FALSE, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.logical(recursive))
  
  #-- Initialise storage -----------------------------------------------------
  inq <- grp.inq.nc(ncfile)
  nvars <- length(inq$varids)
  if (recursive) {
    ngrps <- length(inq$grps)
    nelem <- nvars + ngrps
  } else {
    ngrps <- 0
    nelem <- nvars
  }
  
  elemnames <- character(nelem)
  retlist <- vector("list", nelem)
  
  #-- Read data from each variable -------------------------------------------
  for (ii in seq_len(nvars)) {
    retlist[[ii]] <- var.get.nc(ncfile, inq$varids[ii], ...)
    elemnames[ii] <- var.inq.nc(ncfile, inq$varids[ii])$name
  }
  
  #-- Recursively read each group --------------------------------------------
  for (ii in seq_len(ngrps)) {
    retlist[[nvars + ii]] <- read.nc(inq$grps[[ii]], recursive = TRUE, 
      ...)
    elemnames[nvars + ii] <- grp.inq.nc(inq$grps[[ii]])$name
  }
  
  #-- Set names of list elements ---------------------------------------------
  names(retlist) <- elemnames
  
  return(retlist)
}

# ===============================================================================
# Udunits library functions
# ===============================================================================

#-------------------------------------------------------------------------------
# utcal.nc()
#-------------------------------------------------------------------------------

utcal.nc <- function(unitstring, value, type = "n") {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(unitstring))
  stopifnot(is.numeric(value) && !any(is.na(value)))
  stopifnot(type == "n" || type == "s" || type == "c")
  
  count <- length(value)
  
  #-- C function call to udunits calendar function -----------------------
  ut <- Cwrap("R_ut_calendar", as.character(unitstring), as.integer(count), 
    as.double(value))
  dim(ut) <- c(length(value), 6)
  
  #-- Return object if no error ------------------------------------------
  if (type == "n") {
    colnames(ut) <- c("year", "month", "day", "hour", "minute", "second")
    return(ut)
  } else if (type == "s") {
    x <- apply(ut, 1, function(x) {
      paste(x[1], "-", sprintf("%02g", x[2]), "-", sprintf("%02g", 
        x[3]), " ", sprintf("%02g", x[4]), ":", sprintf("%02g", x[5]), 
        ":", sprintf("%02g", x[6]), sep = "")
    })
    return(x)
  } else if (type == "c") {
    ct <- as.POSIXct(utinvcal.nc("seconds since 1970-01-01 00:00:00 +00:00", 
      ut), tz = "UTC", origin = ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC"))
    return(ct)
  }
}


#-------------------------------------------------------------------------------
# utinit.nc()
#-------------------------------------------------------------------------------

utinit.nc <- function(path = "") {
  ut <- Cwrap("R_ut_init", as.character(path))
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# utinvcal.nc()
#-------------------------------------------------------------------------------

utinvcal.nc <- function(unitstring, value) {
  #-- Check args -------------------------------------------------------------
  stopifnot(is.character(unitstring))
  
  if (is.character(value)) {
    stopifnot(all(nchar(value) == 19))
    value <- cbind(substr(value, 1, 4), substr(value, 6, 7), substr(value, 
      9, 10), substr(value, 12, 13), substr(value, 15, 16), substr(value, 
      18, 19))
    
    value <- matrix(as.numeric(value), ncol = 6)
  } else if (inherits(value, "POSIXct")) {
    value <- utcal.nc("seconds since 1970-01-01 00:00:00 +00:00", as.numeric(value), 
      "n")
  }
  
  stopifnot(is.numeric(value) && !any(is.na(value)))
  
  count <- length(value)
  
  if (is.vector(value) && count%%6 != 0) {
    stop("length(value) not divisible by 6", call. = FALSE)
  }
  
  if (is.matrix(value) && ncol(value) != 6) {
    stop("ncol(value) not 6", call. = FALSE)
  }
  
  #-- C function call --------------------------------------------------------
  ut <- Cwrap("R_ut_inv_calendar", as.character(unitstring), as.integer(count), 
    as.double(value))
  return(ut)
}


# ===============================================================================

# ===============================================================================
# SCRATCH
# ===============================================================================

