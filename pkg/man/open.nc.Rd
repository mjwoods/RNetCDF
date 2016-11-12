\name{open.nc}

\alias{open.nc}

\title{Open a NetCDF Dataset}

\description{Open an existing NetCDF dataset for reading and (optionally) writing.}

\usage{
   open.nc(con, write=FALSE, share=FALSE, prefill=TRUE, ...)
}

\arguments{
  \item{con}{Filename of the NetCDF dataset to be opened.}
  \item{write}{If \code{FALSE} (default), the dataset will be opened read-only. If \code{TRUE}, the dataset will be opened read-write.}
  \item{share}{The buffer scheme. If \code{FALSE} (default), dataset access is buffered and cached for performance. However, if one or more processes may be reading while another process is writing the dataset, set to \code{FALSE}.}
  \item{prefill}{The prefill mode. If \code{TRUE} (default), newly defined variables are initialised with fill values when they are first accessed. This allows unwritten array elements to be detected when reading, but it also implies duplicate writes if all elements are subsequently written with user-specified data. Enhanced write performance can be obtained by setting \code{prefill=FALSE}.}
  \item{...}{Arguments passed to or from other methods (not used).}
}

\value{Object of class "\code{NetCDF}" which points to the NetCDF dataset, returned invisibly.}

\details{This function opens an existing NetCDF dataset for access. By default, the dataset is opened read-only. If \code{write=TRUE}, then the dataset can be changed. This includes appending or changing data, adding dimensions, variables, and attributes.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a void NetCDF dataset
nc <- create.nc("open.nc")
close.nc(nc)

##  Open the NetCDF dataset for writing
nc <- open.nc("open.nc", write=TRUE)
close.nc(nc)
}

\keyword{file}
