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
  nc <- .Call("R_nc_copy_att", ncfile.in, variable.in, attribute,
              ncfile.out, variable.out, PACKAGE="RNetCDF")
  
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
  nc <- .Call("R_nc_delete_att", ncfile, variable, attribute, PACKAGE="RNetCDF")
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# att.get.nc()
#-------------------------------------------------------------------------------

att.get.nc <- function(ncfile, variable, attribute,
                       rawchar = FALSE, fitnum = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.character(attribute) || is.numeric(attribute))
  stopifnot(is.logical(rawchar))
  stopifnot(is.logical(fitnum))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_get_att", ncfile, variable, attribute, rawchar, fitnum,
              PACKAGE="RNetCDF")

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
  nc <- .Call("R_nc_inq_att", ncfile, variable, attribute, PACKAGE="RNetCDF")
  
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
  stopifnot(is.numeric(value) || is.character(value) ||
            is.raw(value) || is.logical(value))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_put_att", ncfile, variable, name, type, value,
              PACKAGE="RNetCDF")
  
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
  nc <- .Call("R_nc_rename_att", ncfile, variable, attribute, newname,
              PACKAGE="RNetCDF")
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# close.nc()
#-------------------------------------------------------------------------------

close.nc <- function(con, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(con) == "NetCDF")
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_close", attr(con, "handle_ptr"), PACKAGE="RNetCDF")
  
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
  nc <- .Call("R_nc_create", filename, clobber, share, prefill, format,
              PACKAGE="RNetCDF")
  
  attr(nc, "class") <- "NetCDF"
  return(invisible(nc))
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
  nc <- .Call("R_nc_def_dim", ncfile, dimname, dimlength, unlim,
              PACKAGE="RNetCDF")
  
  return(invisible(nc))
}


#-------------------------------------------------------------------------------
# dim.inq.nc()
#-------------------------------------------------------------------------------

dim.inq.nc <- function(ncfile, dimension) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(dimension) || is.numeric(dimension))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_inq_dim", ncfile, dimension, PACKAGE="RNetCDF")
  
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
  nc <- .Call("R_nc_rename_dim", ncfile, dimension, newname, PACKAGE="RNetCDF")
  
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# file.inq.nc()
#-------------------------------------------------------------------------------

file.inq.nc <- function(ncfile) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_inq_file", ncfile, PACKAGE="RNetCDF")
  
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
  nc <- .Call("R_nc_open", con, write, share, prefill, PACKAGE="RNetCDF")
  
  attr(nc, "class") <- "NetCDF"
  return(invisible(nc))
}


#-------------------------------------------------------------------------------
# print.nc()
#-------------------------------------------------------------------------------

# Private function to print metadata of groups recursively:
print_grp <- function(x, level = 0) {
  
  indent <- paste(rep("  ", level), collapse = "")
  
  #-- Inquire about the group ------------------------------------------------
  grpinfo <- try(grp.inq.nc(x, ancestors = FALSE), silent = TRUE)
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

  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# sync.nc()
#-------------------------------------------------------------------------------

sync.nc <- function(ncfile) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_sync", ncfile, PACKAGE="RNetCDF")
  
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
  nc <- .Call("R_nc_def_var", ncfile, varname, vartype, dimensions,
              PACKAGE="RNetCDF")
  
  return(invisible(nc))
}

#-------------------------------------------------------------------------------
# var.get.nc()
#-------------------------------------------------------------------------------

var.get.nc <- function(ncfile, variable, start = NA, count = NA, na.mode = 0, 
  collapse = TRUE, unpack = FALSE, rawchar = FALSE, fitnum = FALSE) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  stopifnot(is.numeric(start) || is.logical(start))
  stopifnot(is.numeric(count) || is.logical(count))
  stopifnot(is.logical(collapse))
  stopifnot(is.logical(unpack))
  stopifnot(is.logical(rawchar))
  stopifnot(is.logical(fitnum))
  
  stopifnot(isTRUE(na.mode %in% c(0, 1, 2, 3)))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_get_var", ncfile, variable, start, count, rawchar, fitnum,
              PACKAGE="RNetCDF") 
  
  #-- Convert missing value to NA if defined in NetCDF file --------------
  if (na.mode < 3 && is.numeric(nc)) {
    na.flag <- FALSE
    
    if (na.mode == 0) {
      na.value <- try(att.get.nc(ncfile, variable, "_FillValue"), silent = TRUE)
      na.flag <- !inherits(na.value, "try-error")
      if (!na.flag) {
        na.value <- try(att.get.nc(ncfile, variable, "missing_value"), silent = TRUE)
        na.flag <- !inherits(na.value, "try-error")
      }
    } else if (na.mode == 1) {
      na.value <- try(att.get.nc(ncfile, variable, "_FillValue"), silent = TRUE)
      na.flag <- !inherits(na.value, "try-error")
    } else if (na.mode == 2) {
      na.value <- try(att.get.nc(ncfile, variable, "missing_value"), silent = TRUE)
      na.flag <- !inherits(na.value, "try-error")
    }
    
    if (na.flag && is.numeric(na.value)) {
      nc[nc == na.value] <- NA
    }
  }
  
  #-- Unpack variables if requested (missing values are preserved) -------
  if (unpack && is.numeric(nc)) {
    offset <- try(att.get.nc(ncfile, variable, "add_offset"), silent = TRUE)
    scale <- try(att.get.nc(ncfile, variable, "scale_factor"), silent = TRUE)
    if ((!inherits(offset, "try-error")) && is.numeric(offset) &&
        (!inherits(scale,  "try-error")) && is.numeric(scale)) {
      nc <- nc * scale + offset
    }
  }
  
  #-- Collapse singleton dimensions --------------------------------------
  if (collapse && !is.null(dim(nc))) {
    datadim <- dim(nc)
    keepdim <- (datadim != 1)
    if (any(keepdim)) {
      dim(nc) <- datadim[keepdim]
    }
  }
 
  return(nc)
}


#-------------------------------------------------------------------------------
# var.inq.nc()
#-------------------------------------------------------------------------------

var.inq.nc <- function(ncfile, variable) {
  #-- Check args -------------------------------------------------------------
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(variable) || is.numeric(variable))
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_inq_var", ncfile, variable, PACKAGE="RNetCDF")
  
  names(nc) <- c("id", "name", "type", "ndims", "dimids", "natts")
  
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
  
  #-- Pack variables if requested (missing values are preserved) -------------
  if (pack && is.numeric(data)) {
    offset <- try(att.get.nc(ncfile, variable, "add_offset"), silent = TRUE)
    scale <- try(att.get.nc(ncfile, variable, "scale_factor"), silent = TRUE)
    if ((!inherits(offset, "try-error")) && is.numeric(offset) &&
        (!inherits(scale,  "try-error")) && is.numeric(scale)) {
      data <- (data - offset) * (1/scale)
    }
  }
 
  #-- Convert missing values to a suitable netcdf value ----------------------
  if (na.mode < 3 && is.numeric(data) && any(is.na(data))) {
    na.flag <- FALSE
    
    if (na.mode == 0) {
      na.value <- try(att.get.nc(ncfile, variable, "_FillValue"), silent = TRUE)
      na.flag <- !inherits(na.value, "try-error")
      if (!na.flag) {
        na.value <- try(att.get.nc(ncfile, variable, "missing_value"), silent = TRUE)
        na.flag <- !inherits(na.value, "try-error")
      }
    } else if (na.mode == 1) {
      na.value <- try(att.get.nc(ncfile, variable, "_FillValue"), silent = TRUE)
      na.flag <- !inherits(na.value, "try-error")
    } else if (na.mode == 2) {
      na.value <- try(att.get.nc(ncfile, variable, "missing_value"), silent = TRUE)
      na.flag <- !inherits(na.value, "try-error")
    }

    if (na.flag && is.numeric(na.value)) {
      data[is.na(data)] <- na.value
    } else {
      stop("Found NAs but no applicable missing value attribute", call. = FALSE)
    }
  }
 
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_put_var", ncfile, variable, start, count, data,
              PACKAGE="RNetCDF") 
 
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
  
  #-- C function call --------------------------------------------------------
  nc <- .Call("R_nc_rename_var", ncfile, variable, newname, PACKAGE="RNetCDF")
  
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
  nc <- .Call("R_nc_def_grp", ncid, grpname, PACKAGE="RNetCDF")
  
  # Return object:
  attributes(nc) <- attributes(ncid)
  return(invisible(nc))
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
  nc <- .Call("R_nc_inq_grp_ncid", ncid, grpname, full, PACKAGE="RNetCDF")
  
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
  stopifnot(is.null(grpname) || is.character(grpname))
  
  # If optional argument is specified, find a group by name:
  if (!is.null(grpname)) {
    ncid <- grp.find(ncid, grpname)
  }
  
  # Initialise output list:
  out <- list()
  out$self <- ncid
  
  # Get parent of group (NULL if none):
  pgrp <- try(.Call("R_nc_inq_grp_parent", ncid, PACKAGE="RNetCDF"),
              silent = TRUE)
  if (!inherits(pgrp, "try-error")) {
    attributes(pgrp) <- attributes(ncid)
    out$parent <- pgrp
  }
  
  # Get sub-groups of group (empty list if none):
  grpids <- try(.Call("R_nc_inq_grps", ncid, PACKAGE="RNetCDF"),
                silent = TRUE)
  if (inherits(grpids, "try-error")) {
    out$grps <- list()
  } else {
    out$grps <- lapply(as.list(grpids), function(x) {
      attributes(x) <- attributes(ncid)
      return(x)
    })
  }
  
  # Names of group:
  out$name <- .Call("R_nc_inq_grpname", ncid, FALSE, PACKAGE="RNetCDF")
  if (ancestors) {
    out$fullname <- .Call("R_nc_inq_grpname", ncid, TRUE, PACKAGE="RNetCDF")
  }
  
  # Dimensions visible in group (empty vector if none):
  out$dimids <- .Call("R_nc_inq_dimids", ncid, ancestors, PACKAGE="RNetCDF")
  
  # Unlimited dimensions visible in group (empty vector if none):
  out$unlimids <- .Call("R_nc_inq_unlimids", ncid, PACKAGE="RNetCDF")
  
  # Variables in group (empty vector if none):
  out$varids <- .Call("R_nc_inq_varids", ncid, PACKAGE="RNetCDF")
  
  # Types in group (empty vector if none):
  out$typeids <- .Call("R_nc_inq_typeids", ncid, PACKAGE="RNetCDF")
  
  # Number of group attributes:
  out$ngatts <- .Call("R_nc_inq_natts", ncid, PACKAGE="RNetCDF")
  
  return(out)
}


#-------------------------------------------------------------------------------
# grp.rename.nc()
#-------------------------------------------------------------------------------

grp.rename.nc <- function(ncid, newname, oldname = NULL) {
  # Check arguments:
  stopifnot(class(ncid) == "NetCDF")
  stopifnot(is.character(newname))
  stopifnot(is.null(oldname) || is.character(oldname))
  
  # If optional argument is specified, find a group by name:
  if (!is.null(oldname)) {
    ncid <- grp.find(ncid, oldname)
  }
  
  # C function call:
  nc <- .Call("R_nc_rename_grp", ncid, newname, PACKAGE="RNetCDF")
  
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


#-------------------------------------------------------------------------------
# type.def.nc()
#-------------------------------------------------------------------------------

type.def.nc <- function(ncfile, typename, class, basetype=NULL, size=NULL) {
  # Check arguments
  stopifnot(class(ncfile) == "NetCDF")
  stopifnot(is.character(typename))
  stopifnot(is.character(class))
  if (class == "compound" || class == "opaque") {
    stopifnot(is.numeric(size))
  } else if (class == "enum" || class == "vlen") {
    stopifnot(is.character(basetype) || is.numeric(basetype))
  } else {
    stop("Unknown class for type definition", call.=FALSE)
  }

  id <- .Call("R_nc_def_type", ncfile, typename, class, basetype, size)
  return(invisible(id))
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
  stopifnot(is.numeric(value))
  stopifnot(type == "n" || type == "s" || type == "c")
  
  #-- C function call to udunits calendar function -----------------------
  ut <- .Call("R_nc_calendar", unitstring, value, PACKAGE="RNetCDF")
  
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
  ut <- .Call("R_nc_utinit", as.character(path), PACKAGE="RNetCDF")
  
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
    value <- utcal.nc("seconds since 1970-01-01 00:00:00 +00:00", as.vector(value), "n") 
  }
  
  stopifnot(is.numeric(value))
  
  #-- C function call --------------------------------------------------------
  ut <- .Call("R_nc_inv_calendar", unitstring, value, PACKAGE="RNetCDF") 
  return(ut)
}


# ===============================================================================

# ===============================================================================
# SCRATCH
# ===============================================================================

