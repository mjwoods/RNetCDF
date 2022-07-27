\name{read.nc}

\alias{read.nc}

\title{Read a NetCDF Dataset}

\description{Read all data from a NetCDF dataset.}

\usage{
   read.nc(ncfile, recursive=FALSE, ...)
}

\arguments{
  \item{ncfile}{Object of class \code{NetCDF} which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{recursive}{Descend recursively into any groups in the dataset if \code{TRUE}.}
  \item{...}{Optional arguments passed to \code{var.get.nc}.}
}

\value{A list with the list elements containing an array for each variable or a (possibly nested) list for each group in the NetCDF dataset.}

\details{This function reads all variable data from a NetCDF dataset into a list. The list elements (arrays) have the same names as the variables in the NetCDF dataset.

Groups in the dataset may optionally be read recursively and returned as nested lists. Each list has the name of the corresponding group in the dataset.
}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset
file1 <- tempfile("read_", fileext=".nc")
nc <- create.nc(file1, format="netcdf4")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)
dim.def.nc(nc, "max_string_length", 32)

##  Create two coordinate variables
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))

## Create a group to contain the data
# This is not necessary, but shows it can be done.
grp <- grp.def.nc(nc, "data")

## Create a data variable
var.def.nc(grp, "temperature", "NC_DOUBLE", c("station","time"))

##  Put some _FillValue attribute for temperature
att.put.nc(grp, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)

##  Define variable values
mytime        <- c(1:2)
mytemperature <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9)
myname        <- c("alfa", "bravo", "charlie", "delta", "echo")

##  Put the data
var.put.nc(nc, "time", mytime, 1, length(mytime))
var.put.nc(nc, "name", myname, c(1,1), c(32,5))
var.put.nc(grp, "temperature", mytemperature, c(1,1), c(5,2))

sync.nc(nc)

##  Read the dataset, including the contents of any groups
read.nc(nc, recursive=TRUE)

close.nc(nc)
unlink(file1)
}

\keyword{file}
