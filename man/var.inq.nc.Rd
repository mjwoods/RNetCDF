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
  A list containing the following components:
  \item{id}{Variable ID.}
  \item{name}{Variable name.}
  \item{type}{External NetCDF data type as one of the following labels: \code{NC_BYTE}, \code{NC_UBYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_USHORT}, \code{NC_INT}, \code{NC_UINT}, \code{NC_INT64}, \code{NC_UINT64}, \code{NC_FLOAT}, \code{NC_DOUBLE}, \code{NC_STRING}, or a user-defined type name.}
  \item{ndims}{Number of dimensions the variable was defined as using.}
  \item{dimids}{Vector of dimension IDs corresponding to the variable dimensions (\code{NA} for scalar variables). Order is leftmost varying fastest.}
  \item{natts}{Number of variable attributes assigned to this variable.}
  \item{chunksizes}{Chunk size expressed as the number of elements along each dimension. (\code{NULL} for contiguous storage, \code{NA} for scalar variables). Order is the same as \code{dimids}.}
}

\details{This function returns information about a NetCDF variable. Information about a variable include its name, its ID, its type, its number of dimensions, a vector of the dimension IDs of this variable and the number of attributes.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna}

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
