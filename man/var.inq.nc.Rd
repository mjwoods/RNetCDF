\name{var.inq.nc}

\alias{var.inq.nc}

\title{Inquire About a NetCDF Variable}

\description{Inquire about a NetCDF variable.}

\usage{var.inq.nc(ncfile, variable)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{Either the ID or the name of the variable to be inquired.}
}

\value{
  A list of named components, some of which are only included for datasets in "netcdf4" format (as indicated by \code{\link[RNetCDF]{file.inq.nc}}).
  \item{id}{Variable ID.}
  \item{name}{Variable name.}
  \item{type}{External NetCDF data type as one of the following labels: \code{NC_BYTE}, \code{NC_UBYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_USHORT}, \code{NC_INT}, \code{NC_UINT}, \code{NC_INT64}, \code{NC_UINT64}, \code{NC_FLOAT}, \code{NC_DOUBLE}, \code{NC_STRING}, or a user-defined type name.}
  \item{ndims}{Number of dimensions the variable was defined as using.}
  \item{dimids}{Vector of dimension IDs corresponding to the variable dimensions (\code{NA} for scalar variables). Order is leftmost varying fastest.}
  \item{natts}{Number of variable attributes assigned to this variable.}

The arguments below apply only to datasets in "netcdf4" format:

  \item{chunksizes}{Chunk size expressed as the number of elements along each dimension, in the same order as \code{dimids}. \code{NULL} implies contiguous storage.}
  \item{cache_bytes}{Size of chunk cache in bytes (\code{NA} if undefined).}
  \item{cache_slots}{The number of slots in the chunk cache (\code{NA} if undefined).}
  \item{cache_preemption}{A value between 0 and 1 (inclusive) that biases the cache scheme towards eviction of chunks that have been fully read (\code{NA} if undefined).}
  \item{deflate}{Integer indicating level of compression, from 0 (minimum) to 9 (maximum), or \code{NA} if compression is not enabled.}
  \item{shuffle}{\code{TRUE} if byte shuffling is enabled for the variable, \code{FALSE} otherwise.}
  \item{big_endian}{Byte order of the variable. \code{TRUE} for big-endian, \code{FALSE} for little-endian, \code{NA} for not yet determined.}
  \item{fletcher32}{\code{TRUE} if the fletcher32 checksum is enabled for this variable, \code{FALSE} otherwise.}
  \item{szip_options}{Integer containing a bitmask of szip options. \code{NA} if szip is not used, or \code{NULL} if the information is not provided by this version of NetCDF.}
  \item{szip_bits}{Number of bits per pixel for szip. \code{NA} if szip is not used, or \code{NULL} if the information is not provided by this version of NetCDF.}
  \item{filter_id}{Identifier of filter associated with the variable. \code{NA} if no filter is defined, or \code{NULL} if the information is not provided by this version of NetCDF.}
  \item{filter_params}{Vector of integer parameters for the filter associated with the variable. \code{NA} if no filter is defined, or \code{NULL} if the information is not provided by this version of NetCDF.}
}

\details{This function returns information about a NetCDF variable. Information about a variable include its name, its ID, its type, its number of dimensions, a vector of the dimension IDs of this variable and the number of attributes.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("var.inq.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

##  Inquire about these variables
var.inq.nc(nc, 0)
var.inq.nc(nc, "temperature")

close.nc(nc)
}

\keyword{file}
