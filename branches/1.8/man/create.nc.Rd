\name{create.nc}

\alias{create.nc}

\title{Create a NetCDF Dataset}

\description{Create a new NetCDF dataset.}

\usage{create.nc(filename, clobber=TRUE, large=FALSE, share=FALSE, prefill=TRUE)}

\arguments{
  \item{filename}{Filename for the NetCDF dataset to be created.}
  \item{clobber}{The creation mode. If \code{TRUE} (default), any existing dataset with the same filename will be overwritten. Otherwise set to \code{FALSE}.}
  \item{large}{The file format. If \code{FALSE} (default), create a NetCDF classic format file, otherwise create a 64-bit offset format file. The 64-bit offset format imposes fewer restrictions on data files larger than 2 GB, but it cannot be read by NetCDF library versions earlier than 3.6.0.}
  \item{share}{The buffer scheme. If \code{FALSE} (default), dataset access is buffered and cached for performance. However, if one or more processes may be reading while another process is writing the dataset, set to \code{FALSE}.}
  \item{prefill}{The prefill mode. If \code{TRUE} (default), newly defined variables are initialised with fill values when they are first accessed. This allows unwritten array elements to be detected when reading, but it also implies duplicate writes if all elements are subsequently written with user-specified data. Enhanced write performance can be obtained by setting \code{prefill=FALSE}.}
}

\details{This function creates a new NetCDF dataset, returning an object of class "\code{NetCDF}" that can be used in R. A creation mode flag specifies whether to overwrite any existing dataset with the same name.}

\value{Object of class "\code{NetCDF}" which points to the NetCDF dataset.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a void NetCDF dataset
nc <- create.nc("foo.nc")
close.nc(nc)
}

\keyword{file}
