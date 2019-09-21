\name{var.rename.nc}

\alias{var.rename.nc}

\title{Rename a NetCDF Variable}

\description{Rename a NetCDF variable.}

\usage{var.rename.nc(ncfile, variable, newname)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{Either the ID or the name of the variable to be renamed.}
  \item{newname}{The new variable name.}
}

\details{This function renames an existing variable in a NetCDF dataset open for writing. A variable cannot be renamed to have the same name as another variable.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("var.rename.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

##  Rename these variables
var.rename.nc(nc, 0, "mytime")
var.rename.nc(nc, "temperature", "mytemperature")

close.nc(nc)
}

\keyword{file}
