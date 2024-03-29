\name{var.inq.nc}

\alias{var.inq.nc}

\title{Inquire About a NetCDF Variable}

\description{Inquire about a NetCDF variable.}

\usage{var.inq.nc(ncfile, variable)}

\arguments{
  \item{ncfile}{Object of class \code{NetCDF} which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{Either the ID or the name of the variable to be inquired.}
}

\value{
  A list of named components, some of which are only included for datasets in \code{"netcdf4"} format (as reported by \code{\link[RNetCDF]{file.inq.nc}}).
  \item{id}{Variable ID.}
  \item{name}{Variable name.}
  \item{type}{External NetCDF data type as one of the following labels: \code{NC_BYTE}, \code{NC_UBYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_USHORT}, \code{NC_INT}, \code{NC_UINT}, \code{NC_INT64}, \code{NC_UINT64}, \code{NC_FLOAT}, \code{NC_DOUBLE}, \code{NC_STRING}, or a user-defined type name.}
  \item{ndims}{Number of dimensions the variable was defined as using.}
  \item{dimids}{Vector of dimension IDs corresponding to the variable dimensions (\code{NA} for scalar variables). Order is leftmost varying fastest.}
  \item{natts}{Number of variable attributes assigned to this variable.}

  \item{chunksizes}{(\code{"netcdf4"}) Chunk size expressed as the number of elements along each dimension, in the same order as \code{dimids}. \code{NULL} implies contiguous storage.}
  \item{cache_bytes}{(\code{"netcdf4"}) Size of chunk cache in bytes (\code{NULL} if unsupported).}
  \item{cache_slots}{(\code{"netcdf4"}) The number of slots in the chunk cache (\code{NULL} if unsupported).}
  \item{cache_preemption}{(\code{"netcdf4"}) A value between 0 and 1 (inclusive) that biases the cache scheme towards eviction of chunks that have been fully read (\code{NULL} if unsupported).}
  \item{deflate}{(\code{"netcdf4"}) Integer indicating level of compression, from 0 (minimum) to 9 (maximum), or \code{NA} if compression is not enabled.}
  \item{shuffle}{(\code{"netcdf4"}) \code{TRUE} if byte shuffling is enabled for the variable, \code{FALSE} otherwise.}
  \item{big_endian}{(\code{"netcdf4"}) Byte order of the variable. \code{TRUE} for big-endian, \code{FALSE} for little-endian, \code{NA} for not yet determined, or \code{NULL} if unsupported.}
  \item{fletcher32}{(\code{"netcdf4"}) \code{TRUE} if the fletcher32 checksum is enabled for this variable, \code{FALSE} otherwise.}
  \item{szip_options}{(\code{"netcdf4"}) Integer containing a bitmask of szip options. \code{NA} if szip is not used, or \code{NULL} if unsupported.}
  \item{szip_bits}{(\code{"netcdf4"}) Number of bits per pixel for szip. \code{NA} if szip is not used, or \code{NULL} if unsupported.}
  \item{filter_id}{(\code{"netcdf4"}) Vector of filter IDs associated with the variable, or \code{NULL} if the NetCDF library does not support the multi-filter interface.}
  \item{filter_params}{(\code{"netcdf4"}) List with one element per \code{filter_id}, or \code{NULL} if the NetCDF library does not support the multi-filter interface. Each list member is a vector of \code{numeric} parameters for the corresponding filter. Please see the NetCDF documentation for information about the available filters and their parameters.}
}

\details{This function returns information about a NetCDF variable, including its name, ID, type, number of dimensions, a vector of the dimension IDs, and the number of attributes.}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
file1 <- tempfile("var.inq_", fileext=".nc")
nc <- create.nc(file1)

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

##  Inquire about these variables
var.inq.nc(nc, 0)
var.inq.nc(nc, "temperature")

close.nc(nc)
unlink(file1)
}

\keyword{file}
