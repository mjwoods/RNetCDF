\name{var.def.nc}

\alias{var.def.nc}

\title{Define a NetCDF Variable}

\description{Define a new NetCDF variable.}

\usage{var.def.nc(ncfile, varname, vartype, dimensions,
                  chunking=NA, chunksizes=NULL, deflate=NA, shuffle=FALSE,
                  big_endian=NA, fletcher32=FALSE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{varname}{Variable name. Must begin with an alphabetic character, followed by zero or more alphanumeric characters including the underscore ("\code{_}"). Case is significant.}
  \item{vartype}{External NetCDF data type as one of the following labels: \code{NC_BYTE}, \code{NC_UBYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_USHORT}, \code{NC_INT}, \code{NC_UINT}, \code{NC_INT64}, \code{NC_UINT64}, \code{NC_FLOAT}, \code{NC_DOUBLE}, \code{NC_STRING}, or a user-defined type name.}
  \item{dimensions}{Vector of \code{ndims} dimension IDs or their names corresponding to the variable dimensions or \code{NA} if a scalar variable should be created. If the ID (or name) of the unlimited dimension is included, it must be last.}
  The following arguments are optional for datasets in "netcdf4" format (and ignored for other formats):
  \item{chunking}{\code{TRUE} selects chunking, \code{FALSE} implies contiguous storage, \code{NA} allows the NetCDF library to choose a storage layout.}
  \item{chunksizes}{Chunk size expressed as the number of elements along each dimension, in the same order as \code{dimensions}. If \code{NULL}, the NetCDF library uses a default chunking strategy, which is intended to give reasonable performance in typical applications. Ignored unless \code{chunking} is \code{TRUE}.}
  \item{deflate}{Integer indicating level of compression, from 0 (minimum) to 9 (maximum), or \code{NA} for no compression.}
  \item{shuffle}{\code{TRUE} to enable byte shuffling, which may improve compression with \code{deflate}.}
  \item{big_endian}{Byte order of the variable. \code{TRUE} for big-endian, \code{FALSE} for little-endian, \code{NA} for native endianness of the platform.}
  \item{fletcher32}{\code{TRUE} to enable the fletcher32 checksum.}
}

\value{NetCDF variable identifier, returned invisibly.}

\details{This function creates a new NetCDF variable. A NetCDF variable has a name, a type, and a shape, which are specified when it is defined. A variable may also have values, which are established later in data mode. 

Ordinarily, the name, type, and shape are fixed when the variable is first defined. The name may be changed, but the type and shape of a variable cannot be changed. However, a variable defined in terms of the unlimited dimension can grow without bound in that dimension. The fastest varying dimension has to be first in \code{dimensions}, the slowest varying dimension last (this is the same way as an array is defined in R; i.e., opposite to the CDL conventions).

A NetCDF variable in an open NetCDF dataset is referred to by a small integer called a variable ID. Variable IDs are 0, 1, 2,..., in the order in which the variables were defined within a NetCDF dataset.

Attributes may be associated with a variable to specify such properties as units.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("var.def.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

close.nc(nc)
}

\keyword{file}
