\name{print.nc}

\alias{print.nc}

\title{Print Summary Information About a NetCDF Dataset}

\description{Print summary information about a NetCDF dataset.}

\usage{print.nc(x, ...)}

\arguments{
  \item{x}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{...}{Arguments passed to or from other methods (not used).}
}

\details{This function prints information about a NetCDF dataset. This includes a list of all dimensions and their length, a list of all variables and their attributes (including their values) and a list of all global attributes (including their values).

The output of this function is almost identical with a \code{"ncdump -h"} call. Because arrays in R have their leftmost subscript varying fastest, the fastest varying dimensions are printed first.
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("print.nc")

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

##  Print summary information about the dataset
print.nc(nc)

close.nc(nc)
}

\keyword{file}
