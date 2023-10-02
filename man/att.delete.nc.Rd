\name{att.delete.nc}

\alias{att.delete.nc}

\title{Delete a NetCDF Attribute}

\description{Delete a NetCDF attribute.}

\usage{att.delete.nc(ncfile, variable, attribute)}

\arguments{
  \item{ncfile}{Object of class \code{NetCDF} which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the attribute's variable, or \code{"NC_GLOBAL"} for a global attribute.}
  \item{attribute}{The name or ID of the attribute to be deleted.}
}

\details{This function deletes a NetCDF attribute from a NetCDF dataset open for writing.

Valid attribute ID numbers range from 0 to the number of attributes minus 1. The number of attributes of a file, group, or variable can be found using the relevant inquiry function (\code{\link[RNetCDF]{file.inq.nc}}, \code{\link[RNetCDF]{grp.inq.nc}}, or \code{\link[RNetCDF]{var.inq.nc}}).
}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
file1 <- tempfile("att.delete_", fileext=".nc")
nc <- create.nc(file1)

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

##  Put some attributes
att.put.nc(nc, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")

##  Delete these attributes
att.delete.nc(nc, "temperature", "_FillValue")
att.delete.nc(nc, "NC_GLOBAL", "title")

close.nc(nc)
unlink(file1)
}

\keyword{file}
