#===============================================================================
#
#  Name:       utility.R
#
#  Purpose:    NetCDF interface for R - utility functions.
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
# print.nc()
#-------------------------------------------------------------------------------

# Private function to print attributes,
# given results from var.inq.nc and att.inq.nc:
print_att <- function(grp, attinfo, indent, varinfo=NULL) {
  if (is.null(varinfo)) {
    varid <- "NC_GLOBAL"
    varname <- ""
  } else {
    varid <- varinfo$id
    varname <- varinfo$name
  }
  typeinfo <- type.inq.nc(grp, attinfo$type, fields=FALSE)
  if (attinfo$type == "NC_CHAR" || attinfo$type == "NC_STRING") {
    atttypestr <- attinfo$type
    attvalstr <- paste("\"", att.get.nc(grp, varid, attinfo$id), "\"", 
		       collapse=", ", sep="")
  } else if (typeinfo$class != "builtin") {
    atttypestr <- paste("//", attinfo$type, sep="");
    attvalstr <- "..."
  } else {
    atttypestr <- attinfo$type
    attval <- att.get.nc(grp, varid, attinfo$id,
                         fitnum=requireNamespace("bit64", quietly=TRUE))
    if (inherits(attval, "integer64")) {
      attvalchar <- bit64::as.character.integer64(attval)
    } else {
      attvalchar <- as.character(attval)
    }
    attvalstr <- paste(attvalchar, collapse=", ", sep="")
  }
  tab <- "\t"
  cat(indent, tab, tab, atttypestr, " ",
      varname, ":", attinfo$name,
      " = ", attvalstr, " ;\n", sep="")
}

# Private function to print metadata of groups recursively:
print_grp <- function(x, level = 0) {

  gap <- "  "
  indent <- paste(rep(gap, level), collapse = "")
  tab <- "\t"

  #-- Inquire about the group ------------------------------------------------
  grpinfo <- try(grp.inq.nc(x, ancestors = FALSE), silent = TRUE)
  if (inherits(grpinfo, "try-error") || is.null(grpinfo)) {
    return(invisible(NULL))
  }
  
  #-- Inquire about all dimensions -------------------------------------------
  if (length(grpinfo$dimids) != 0) {
    cat(indent, "dimensions:\n", sep = "")
    for (id in grpinfo$dimids) {
      diminfo <- dim.inq.nc(x, id)
      if (diminfo$unlim == FALSE) {
        cat(indent, tab, diminfo$name, " = ", diminfo$length,
          " ;\n", sep = "")
      } else {
        cat(indent, tab, diminfo$name, " = UNLIMITED ; // (",
          diminfo$length, " currently)\n", sep = "")
      }
    }
  }
  
  #-- Inquire about all types ------------------------------------------------
  if (length(grpinfo$typeids) != 0) {
    cat(indent, "types:\n", sep = "")
    for (id in grpinfo$typeids) {
      typeinfo <- type.inq.nc(x, id, fields=TRUE)
      if (typeinfo$class == "compound") {
        cat(indent, gap, "compound ", typeinfo$name, " {\n", sep="")
        field_names = names(typeinfo$subtype)
        field_types = unname(typeinfo$subtype)
        field_sizes = unname(typeinfo$dimsizes)
        for (item in seq_along(typeinfo$subtype)) {
          cat(indent, gap, gap, field_types[item], " ",
              field_names[item], sep="")
          if (!is.null(field_sizes[[item]])) {
            cat("(", paste(field_sizes[[item]], collapse=","), ")", sep="")
          }
          cat(" ;\n")
        }
        cat(indent, gap, "}; // ", typeinfo$name, "\n", sep="")
      } else if (typeinfo$class == "enum") {
        cat(indent, gap, typeinfo$basetype, " enum ", typeinfo$name,
            " {\n", sep="")
        member_names <- names(typeinfo$value)
        member_values <- unname(typeinfo$value)
        for (item in seq_along(typeinfo$value)) {
          cat(indent, gap, gap, "\"", member_names[item], "\"",
              " = ", member_values[item], ",\n", sep="")
        }
        cat(indent, gap, "} ; // ", typeinfo$name, "\n", sep="")
      } else if (typeinfo$class == "opaque") {
        cat(indent, gap, "opaque(", typeinfo$size, ") ", typeinfo$name,
            " ;\n", sep="")
      } else if (typeinfo$class == "vlen") {
        cat(indent, gap, typeinfo$basetype, "(*) ", typeinfo$name,
            " ;\n", sep="")
      }
    }
  }

  #-- Inquire about all variables --------------------------------------------
  if (length(grpinfo$varids) != 0) {
    cat(indent, "variables:\n", sep = "")
    for (id in grpinfo$varids) {
      varinfo <- var.inq.nc(x, id)
      vartype <- varinfo$type
      cat(indent, tab, vartype, " ", varinfo$name, sep = "")
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
          print_att(x, attinfo, indent, varinfo)
        }
      }
    }
  }
  
  #-- Inquire about global attributes ----------------------------------------
  if (grpinfo$ngatts != 0) {
    if (level == 0) {
      cat("\n", indent, "// global attributes:\n", sep = "")
    } else {
      cat("\n", indent, "// group attributes:\n", sep = "")
    }
    id <- "NC_GLOBAL"
    for (jj in 0:(grpinfo$ngatts - 1)) {
      attinfo <- att.inq.nc(x, id, jj)
      print_att(x, attinfo, indent)
    }
  }

  #-- Print groups recursively -----------------------------------------------
  if (length(grpinfo$grps) != 0) {
    for (id in grpinfo$grps) {
      subgrpinfo <- grp.inq.nc(id, ancestors = FALSE)
      cat("\n", indent, "group: ", subgrpinfo$name, " {\n", sep = "")
      print_grp(id, level = (level + 1))
      cat(indent, gap, "} // group ", subgrpinfo$name, "\n", sep = "")
    }
  }

}

print.nc <- function(x, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(x, "NetCDF"))

  cat("netcdf ", file.inq.nc(x)$format, " {\n", sep="")

  # Display groups recursively:
  print_grp(x, level = 0)

  cat("}\n")

  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# read.nc()
#-------------------------------------------------------------------------------

read.nc <- function(ncfile, recursive = FALSE, ...) {
  #-- Check args -------------------------------------------------------------
  stopifnot(inherits(ncfile, "NetCDF"))
  stopifnot(is.logical(recursive))
  
  #-- Initialise storage -----------------------------------------------------
  inq <- grp.inq.nc(ncfile)
  nvars <- length(inq$varids)
  if (isTRUE(recursive)) {
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

