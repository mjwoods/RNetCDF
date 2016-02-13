\name{att.put.nc}

\alias{att.put.nc}

\title{Put a NetCDF Attribute}

\description{Put an attribute to a NetCDF dataset.}

\usage{att.put.nc(ncfile, variable, name, type, value)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the variable to which the attribute will be assigned or \code{"NC_GLOBAL"} for a global attribute.}
  \item{name}{Attribute name. Must begin with an alphabetic character, followed by zero or more alphanumeric characters including the underscore ("\code{_}"). Case is significant. Attribute name conventions are assumed by some NetCDF generic applications, e.g., \code{units} as the name for a string attribute that gives the units for a NetCDF variable.}
  \item{type}{One of the set of predefined NetCDF external data types. The valid NetCDF external data types are \code{NC_BYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_INT}, \code{NC_FLOAT}, and \code{NC_DOUBLE}.}
  \item{value}{Attribute value. This can be either a single numeric value or a vector of numeric values, or alternatively a character string.}
}

\details{Names commencing with underscore ("\code{_}") are reserved for use by the NetCDF library. Most generic applications that process NetCDF datasets assume standard attribute conventions and it is strongly recommended that these be followed unless there are good reasons for not doing so.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\note{\code{NC_BYTE} is always interpreted as signed.}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("foo.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

##  Put some attributes
att.put.nc(nc, "temperature", "missing_value", "NC_DOUBLE", -99999.9)
att.put.nc(nc, "temperature", "long_name", "NC_CHAR", "air temperature")
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")
att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date()))

close.nc(nc)
}

\keyword{file}
