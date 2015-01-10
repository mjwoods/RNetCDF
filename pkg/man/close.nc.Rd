\name{close.nc}

\alias{close.nc}

\title{Close a NetCDF Dataset}

\description{Close an open NetCDF dataset.}

\usage{close.nc(con, ...)}

\arguments{
  \item{con}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{...}{Arguments passed to or from other methods (not used).}
}

\details{This function closes an open NetCDF dataset. After an open NetCDF dataset is closed, its NetCDF ID may be reassigned to the next NetCDF dataset that is opened or created. Therefore, the passed object (\code{ncfile}) should be deleted by the user after calling this function.}

\value{}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a void NetCDF dataset
nc <- create.nc("foo.nc")
close.nc(nc)
}

\keyword{file}
