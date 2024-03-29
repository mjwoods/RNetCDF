\name{grp.rename.nc}

\alias{grp.rename.nc}

\title{Rename a NetCDF Group}

\description{Rename a NetCDF group.}

\usage{grp.rename.nc(ncid, newname, oldname=NULL)}

\arguments{
  \item{ncid}{Object of class \code{NetCDF} which points to a NetCDF group (from \code{\link[RNetCDF]{grp.def.nc}}) or dataset (from \code{\link[RNetCDF]{open.nc}}).}
  \item{newname}{The new group name.}
  \item{oldname}{By default, the rename applies to the group represented by \code{ncid}. If \code{oldname} is a character string, a group with this name is renamed instead. A hierarchical search is performed if \code{oldname} contains \code{"/"}, otherwise only the immediate group of \code{ncid} is searched for a matching group name.}
}

\details{This function renames an existing group in a dataset of \code{"netcdf4"} format that is open for writing. A group cannot be renamed to have the same name as another group, type or variable in the parent group.}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define a group
file1 <- tempfile("grp.rename_", fileext=".nc")
nc <- create.nc(file1, format="netcdf4")

grp <- grp.def.nc(nc, "oldgroup")

##  Rename the group (operation not supported by early versions of the netcdf4 library)
try(grp.rename.nc(grp, "newgroup"))

close.nc(nc)
unlink(file1)
}

\keyword{file}
