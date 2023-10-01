\name{grp.inq.nc}

\alias{grp.inq.nc}

\title{Inquire About a NetCDF Group}

\description{Inquire about a NetCDF group.}

\usage{grp.inq.nc(ncid,grpname=NULL,ancestors=TRUE)}

\arguments{
  \item{ncid}{Object of class \code{NetCDF} which points to a NetCDF group (from \code{\link[RNetCDF]{grp.def.nc}}) or dataset (from \code{\link[RNetCDF]{open.nc}}).}
  \item{grpname}{By default, the inquiry relates to the group represented by \code{ncid}. If \code{grpname} is a character string, a group with this name is examined instead. A hierarchical search is performed if \code{grpname} contains \code{"/"}, otherwise only the immediate group of \code{ncid} is searched for a matching group name.}
 \item{ancestors}{If \code{TRUE}, dimensions and names of ancestor groups are examined. Otherwise, only dimensions and names defined in the current group are reported.}
}

\value{
  A list containing the following components:
  \item{self}{Object of class \code{NetCDF} representing the group.}
  \item{parent}{Object of class \code{NetCDF} representing the parent group, or \code{NULL} for the root group.}
  \item{grps}{List of objects of class \code{NetCDF} representing the groups in the group.}
  \item{name}{Name of the NetCDF group.}
  \item{fullname}{Full name of the NetCDF group, with ancestors listed in order from the root group of the dataset and separated by \code{"/"}. Omitted if \code{ancestors} is \code{FALSE}.} 
  \item{dimids}{Vector of dimension identifiers. If \code{ancestors} is \code{TRUE} (default), all visible dimensions in the group and its ancestors are reported, otherwise only dimensions defined in the group of \code{ncid} are shown.}
  \item{unlimids}{Vector of identifiers for unlimited dimensions. If \code{ancestors} is \code{TRUE} (default), all unlimited dimensions in the group and its ancestors are reported, otherwise only unlimited dimensions defined in the group of \code{ncid} are shown.}
  \item{varids}{Vector of identifiers for variables in the group.}
  \item{typeids}{Vector of identifiers for types in the group.}
  \item{ngatts}{Number of group attributes.}
}

\details{This function provides information about the structure of a NetCDF group or dataset. The results allow programs to explore a dataset without prior knowledge of the contents.}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
# Create a new NetCDF dataset:
file1 <- tempfile("grp.inq_", fileext=".nc")
nc <- create.nc(file1, format="netcdf4")

# Define groups in root group.
# (Any names can be used; hierarchical numbers are used here for clarity)
grp11 <- grp.def.nc(nc, "group1.1")
grp12 <- grp.def.nc(nc, "group1.2")

# Define group nested in group1.1:
grp111 <- grp.def.nc(grp11, "group1.1.1")

# Put some attributes in each group.
# (We could also define dimensions, types, and variables).
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Group 1 (root)")
att.put.nc(grp11, "NC_GLOBAL", "title", "NC_CHAR", "Group 1.1")
att.put.nc(grp12, "NC_GLOBAL", "title", "NC_CHAR", "Group 1.2")
att.put.nc(grp111, "NC_GLOBAL", "title", "NC_CHAR", "Group 1.1.1")

## Examine contents of a group directly using its hierarchical name ...

mygrp <- grp.inq.nc(nc, "/group1.1/group1.1.1")
att.get.nc(mygrp$self, "NC_GLOBAL", "title")

## Recursively examine contents of nested groups ...
# (See also print.nc for a visual overview)

get_global_atts <- function(ncid) {
  inq <- grp.inq.nc(ncid)
  atts <- character(inq$ngatts)
  for (ii in seq_len(inq$ngatts)) {
    atts[ii] <- att.get.nc(ncid, "NC_GLOBAL", ii-1)
  }
  ngrps <- length(inq$grps)
  grps <- vector("list", ngrps + 1)
  grps[[1]] <- atts
  for (ii in seq_len(ngrps)) {
    grps[[ii + 1]] <- get_global_atts(inq$grps[[ii]])
  }
  return(grps)
}

get_global_atts(nc)

## Tidy up:
close.nc(nc)
unlink(file1)
}

\keyword{file}
