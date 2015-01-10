\name{dim.rename.nc}

\alias{dim.rename.nc}

\title{Rename a NetCDF Dimension}

\description{Rename a NetCDF dimension.}

\usage{dim.rename.nc(ncfile, dimension, newname)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{dimension}{Either the ID or the name of the dimension to be renamed.}
  \item{newname}{The new dimension name.}
}

\details{This function renames an existing dimension in a NetCDF dataset open for writing. A dimension cannot be renamed to have the same name as another dimension.}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("foo.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Rename the dimensions
dim.rename.nc(nc, 0, "mystation")
dim.rename.nc(nc, "time", "mytime")

close.nc(nc)
}

\keyword{file}
