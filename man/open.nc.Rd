\name{open.nc}

\alias{open.nc}

\title{Open a NetCDF Dataset}

\description{Open an existing NetCDF dataset for reading and (optionally) writing.}

\usage{
   open.nc(con, write=FALSE, ...)
}

\arguments{
  \item{con}{Filename of the NetCDF dataset to be opened.}
  \item{write}{If \code{FALSE} (default), the dataset will be opened read-only. If \code{TRUE}, the dataset will be opened read-write.}
  \item{...}{Arguments passed to or from other methods (not used).}
}

\details{This function opens an existing NetCDF dataset for access. By default, the dataset is opened read-only. If \code{write=TRUE}, then the dataset can be changed. This includes appending or changing data, adding dimensions, variables, and attributes.}

\value{Object of class "\code{NetCDF}" which points to the NetCDF dataset.}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a void NetCDF dataset
nc <- create.nc("foo.nc")
close.nc(nc)

##  Open the NetCDF dataset for writing
nc <- open.nc("foo.nc", write=TRUE)
close.nc(nc)
}

\keyword{file}
