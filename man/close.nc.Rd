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

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a void NetCDF dataset
file1 <- tempfile("close_", fileext=".nc")
nc <- create.nc(file1)
close.nc(nc)
unlink(file1)
}

\keyword{file}
