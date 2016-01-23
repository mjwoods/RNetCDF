\name{var.inq.nc}

\alias{var.inq.nc}

\title{Inquire About a NetCDF Variable}

\description{Inquire about a NetCDF variable.}

\usage{var.inq.nc(ncfile, variable)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{Either the ID or the name of the variable to be inquired.}
}

\details{This function returns information about a NetCDF variable. Information about a variable include its name, its ID, its type, its number of dimensions, a vector of the dimension IDs of this variable and the number of attributes. The valid external NetCDF data types are \code{NC_BYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_INT}, \code{NC_FLOAT}, and \code{NC_DOUBLE}.}

\value{
  A list containing the following components:
  \item{id}{Variable ID.}
  \item{name}{Variable name.}
  \item{type}{External NetCDF data type.}
  \item{ndims}{Number of dimensions the variable was defined as using.}
  \item{dimids}{Vector of dimension IDs corresponding to the variable dimensions (\code{NA} for scalar variables). Order is leftmost varying fastest.}
  \item{natts}{Number of variable attributes assigned to this variable.}
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("foo.nc")

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
