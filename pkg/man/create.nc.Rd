\name{create.nc}

\alias{create.nc}

\title{Create a NetCDF Dataset}

\description{Create a new NetCDF dataset.}

\usage{create.nc(filename, clobber=TRUE)}

\arguments{
  \item{filename}{Filename for the NetCDF dataset to be created.}
  \item{clobber}{The creation mode. If \code{TRUE} (default), any existing dataset with the same filename will be overwritten. Otherwise set to \code{FALSE}.}
}

\details{This function creates a new NetCDF dataset, returning an object of class "\code{NetCDF}" that can be used in R. A creation mode flag specifies whether to overwrite any existing dataset with the same name.}

\value{Object of class "\code{NetCDF}" which points to the NetCDF dataset.}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a void NetCDF dataset
nc <- create.nc("foo.nc")
close.nc(nc)
}

\keyword{file}
