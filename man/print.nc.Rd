\name{print.nc}

\alias{print.nc}

\title{Print Summary Information About a NetCDF Dataset}

\description{Print summary information about a NetCDF dataset.}

\usage{print.nc(x, ...)}

\arguments{
  \item{x}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{...}{Arguments passed to or from other methods (not used).}
}

\details{This function prints information about the structure of a NetCDF dataset, including lists of all groups, dimensions, user-defined types, variables and attributes.

The output of this function is similar to the \code{ncdump -h} command supplied with the NetCDF C library. One important difference is that array dimensions are shown by \code{print.nc} in the order used by R, where the leftmost subscript varies fastest.
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset
nc <- create.nc("print.nc", format="netcdf4")

## Create a group (just because we can!):
grp <- grp.def.nc(nc, "data")

## Create some dimensions, putting one inside the group:
dim.def.nc(nc, "time", unlim=TRUE)
dim.def.nc(grp, "station", 5)

##  Create two variables, putting one inside the group:
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(grp, "temperature", "NC_DOUBLE", c("station","time"))

##  Put some attributes
att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date()))
att.put.nc(grp, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)
att.put.nc(grp, "temperature", "long_name", "NC_CHAR", "air temperature")
att.put.nc(grp, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")

##  Print summary information about the dataset
print.nc(nc)

close.nc(nc)
}

\keyword{file}
