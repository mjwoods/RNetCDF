\name{read.nc}

\alias{read.nc}

\title{Read a NetCDF Dataset}

\description{Read all data from a NetCDF dataset.}

\usage{
   read.nc(ncfile, unpack=TRUE)
}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{unpack}{Unpack "packed" variables if set to \code{TRUE} (default).}
}

\details{This function reads all variable data from a NetCDF dataset into a single list. The list elements (arrays) have the same names as the variables in the NetCDF dataset.

Packed variables can optionally be returned in an unpacked state (see \code{\link{var.get.nc}} for more information).
}

\value{A list with the list elements containing the variable data of the NetCDF dataset.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("foo.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)
dim.def.nc(nc, "max_string_length", 32)

##  Create three variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))
var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))

##  Put some missing_value attribute for temperature
att.put.nc(nc, "temperature", "missing_value", "NC_DOUBLE", -99999.9)

##  Define variable values
mytime        <- c(1:2)
mytemperature <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9)
myname        <- c("alfa", "bravo", "charlie", "delta", "echo")

##  Put the data
var.put.nc(nc, "time", mytime, 1, length(mytime))
var.put.nc(nc, "temperature", mytemperature, c(1,1), c(5,2))
var.put.nc(nc, "name", myname, c(1,1), c(32,5))

sync.nc(nc)

##  Get the data
read.nc(nc)

close.nc(nc)
}

\keyword{file}
