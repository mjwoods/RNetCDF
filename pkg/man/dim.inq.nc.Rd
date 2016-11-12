\name{dim.inq.nc}

\alias{dim.inq.nc}

\title{Inquire About a NetCDF Dimension}

\description{Inquire about a NetCDF dimension.}

\usage{dim.inq.nc(ncfile, dimension)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{dimension}{Either the ID or the name of the dimension to be inquired.}
}

\value{
  A list containing the following components:
  \item{id}{Dimension ID.}
  \item{name}{Dimension name.}
  \item{length}{Length of dimension. For the unlimited dimension, this is the number of records written so far.}
  \item{unlim}{\code{TRUE} if it is the unlimited dimension, \code{FALSE} otherwise.} 
}

\details{This function returns information about a NetCDF dimension. Information about a dimension include its name, its ID, its length and a flag if it is the unlimited dimension of this NetCDF dataset, if any. The length of the unlimited dimension, if any, is the number of records written so far.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("dim.inq.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Inquire about the dimensions
dim.inq.nc(nc, 0)
dim.inq.nc(nc, "time")

close.nc(nc)
}

\keyword{file}
