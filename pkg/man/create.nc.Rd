\name{create.nc}

\alias{create.nc}

\title{Create a NetCDF Dataset}

\description{Create a new NetCDF dataset.}

\usage{create.nc(filename, clobber=TRUE, share=FALSE, prefill=TRUE,
         format="classic", large=FALSE)}

\arguments{
  \item{filename}{Filename for the NetCDF dataset to be created.}
  \item{clobber}{The creation mode. If \code{TRUE} (default), any existing dataset with the same filename will be overwritten. Otherwise set to \code{FALSE}.}
  \item{share}{The buffer scheme. If \code{FALSE} (default), dataset access is buffered and cached for performance. However, if one or more processes may be reading while another process is writing the dataset, set to \code{FALSE}.}
  \item{prefill}{The prefill mode. If \code{TRUE} (default), newly defined variables are initialised with fill values when they are first accessed. This allows unwritten array elements to be detected when reading, but it also implies duplicate writes if all elements are subsequently written with user-specified data. Enhanced write performance can be obtained by setting \code{prefill=FALSE}.}
  \item{format}{The file format. One of "classic", "offset64", "netcdf4" or "classic4". See below for details.}
  \item{large}{(Deprecated) \code{large=TRUE} is equivalent to \code{format="offset64"}.}
}

\value{Object of class "\code{NetCDF}" which points to the NetCDF dataset, returned invisibly.}

\details{This function creates a new NetCDF dataset, returning an object of class "\code{NetCDF}" that can be used in R.

The file format is specified by the \code{format} argument, which may take the following values:
\describe{
  \item{"classic"}{(default) Original netcdf file format, still widely used and recommended for maximum portability of datasets. Uses a signed 32-bit offset in its internal structures, so files larger than 2GB can only be created under limited conditions.}
  \item{"offset64"}{64-bit offset extension of original format, introduced by netcdf-3.6. Allows larger files and variables than "classic" format, but there remain some restrictions on files larger than 2GB.}
  \item{"netcdf4"}{Netcdf in an HDF5 container, introduced by netcdf-4.0. Allows dataset sizes up to filesystem limits, and extends the feature set of the older formats.}
  \item{"classic4"}{Same file format as "netcdf4", but this options ensures that only classic netcdf data structures are stored in the file for compatibility with older software (when linked with the netcdf4 library).}
}
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create empty NetCDF datasets with different formats
nc <- create.nc("create3.nc")
close.nc(nc)
nc2 <- create.nc("create64.nc",format="offset64")
close.nc(nc2)
nc3 <- create.nc("create4.nc",format="netcdf4")
close.nc(nc3)
}

\keyword{file}
