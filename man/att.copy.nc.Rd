\name{att.copy.nc}

\alias{att.copy.nc}

\title{Copy Attribute from One NetCDF to Another}

\description{Copy attribute from one NetCDF to another.}

\usage{att.copy.nc(ncfile.in, variable.in, attribute, ncfile.out, variable.out)}

\arguments{
  \item{ncfile.in}{Object of class "\code{NetCDF}" which points to the input NetCDF dataset from which the attribute will be copied (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable.in}{ID or name of the variable in the input NetCDF dataset from which the attribute will be copied, or \code{"NC_GLOBAL"} for a global attribute.}
  \item{attribute}{Name or ID of the attribute in the input NetCDF dataset to be copied.}
  \item{ncfile.out}{Object of class "\code{NetCDF}" which points to the output NetCDF dataset to which the attribute will be copied (as returned from \code{\link[RNetCDF]{open.nc}}). It is permissible for the input and output NetCDF object to be the same.}
  \item{variable.out}{ID or name of the variable in the output NetCDF dataset to which the attribute will be copied, or \code{"NC_GLOBAL"} to copy to a global attribute.}
}

\details{This function copies an attribute from one open NetCDF dataset to another. It can also be used to copy an attribute from one variable to another within the same NetCDF dataset.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create two new NetCDF datasets and define two dimensions
file1 <- tempfile("att.copy_", fileext=".nc")
file2 <- tempfile("att.copy_", fileext=".nc")
nc.1 <- create.nc(file1)
nc.2 <- create.nc(file2)

dim.def.nc(nc.1, "station", 5)
dim.def.nc(nc.1, "time", unlim=TRUE)

dim.def.nc(nc.2, "station", 5)
dim.def.nc(nc.2, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc.1, "time", "NC_INT", "time")
var.def.nc(nc.1, "temperature", "NC_DOUBLE", c(0,1))

var.def.nc(nc.2, "time", "NC_INT", "time")
var.def.nc(nc.2, "temperature", "NC_DOUBLE", c(0,1))

##  Put some attributes to the first dataset
att.put.nc(nc.1, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)
att.put.nc(nc.1, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")

##  Copy the attributes to the second dataset
att.copy.nc(nc.1, 1, 0, nc.2, 1)
att.copy.nc(nc.1, "NC_GLOBAL", "title", nc.2, "NC_GLOBAL")

close.nc(nc.1)
close.nc(nc.2)
unlink(file1)
unlink(file2)
}

\keyword{file}
