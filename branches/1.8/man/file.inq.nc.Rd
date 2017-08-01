\name{file.inq.nc}

\alias{file.inq.nc}

\title{Inquire About a NetCDF Dataset}

\description{Inquire about a NetCDF dataset.}

\usage{file.inq.nc(ncfile)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
}

\details{This function returns values for the number of dimensions, the number of variables, the number of global attributes, and the dimension ID of the dimension defined with unlimited length, if any.}

\value{
  A list containing the following components:
  \item{ndims}{Number of dimensions defined for this NetCDF dataset.}
  \item{nvars}{Number of variables defined for this NetCDF dataset.}
  \item{ngatts}{Number of global attributes for this NetCDF dataset.}
  \item{unlimdimid}{ID of the unlimited dimension, if there is one for this NetCDF dataset. Otherwise \code{NA} will be returned.} 
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("foo.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

##  Put some attributes
att.put.nc(nc, "temperature", "missing_value", "NC_DOUBLE", -99999.9)
att.put.nc(nc, "temperature", "long_name", "NC_CHAR", "air temperature")
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")
att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date()))

##  Inquire about the dataset
file.inq.nc(nc)

close.nc(nc)
}

\keyword{file}
