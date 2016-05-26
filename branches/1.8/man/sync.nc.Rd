\name{sync.nc}

\alias{sync.nc}

\title{Synchronize a NetCDF Dataset}

\description{Synchronize an open NetCDF dataset to disk.}

\usage{sync.nc(ncfile)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
}

\details{This function offers a way to synchronize the disk copy of a NetCDF dataset with in-memory buffers. There are two reasons one might want to synchronize after writes: To minimize data loss in case of abnormal termination, or to make data available to other processes for reading immediately after it is written.}

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

##  Define variable values
mytime        <- c(1:2)
mytemperature <- c(0.0, 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9)

##  Put the data
var.put.nc(nc, "time", mytime, 1, length(mytime))
var.put.nc(nc, "temperature", mytemperature, c(1,1), c(5,2))

##  Synchronize to disk
sync.nc(nc)

##  Now the data can be read
var.get.nc(nc, 0)
var.get.nc(nc, "temperature")

close.nc(nc)
}

\keyword{file}
