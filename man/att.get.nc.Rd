\name{att.get.nc}

\alias{att.get.nc}

\title{Get a NetCDF Attribute}

\description{Get an attribute from a NetCDF dataset.}

\usage{att.get.nc(ncfile, variable, attribute, rawchar=FALSE, fitnum=FALSE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the variable from which the attribute will be read, or \code{"NC_GLOBAL"} for a global attribute.}
  \item{attribute}{Attribute name or ID.}
  \item{rawchar}{This option only relates to NetCDF attributes of type \code{NC_CHAR}. When \code{rawchar} is \code{FALSE} (default), a NetCDF attribute of type \code{NC_CHAR} is converted to a \code{character} string in R. If \code{rawchar} is \code{TRUE}, the bytes of \code{NC_CHAR} data are read into an R \code{raw} vector.}
  \item{fitnum}{By default, all numeric variables are read into R as double precision values. When \code{fitnum==TRUE}, the smallest R type that can exactly represent each external type is used. Types \code{NC_BYTE}, \code{NC_UBYTE}, \code{NC_SHORT}, \code{NC_USHORT} and \code{NC_INT} are read as R integer values, which require half the memory of double precision. \code{NC_UINT} is always read as double precision to avoid integer overflow. Due to the lack of native support for 64-bit integers in R, \code{NC_INT64} and \code{NC_UINT64} are stored as R character vectors in decimal format.}
}

\value{Vector with a data type that depends on the NetCDF variable. For NetCDF variables of type \code{NC_CHAR}, the R type is either \code{character} or \code{raw}, as specified by argument \code{rawchar}. For \code{NC_STRING}, the R type is \code{character}. Numeric variables are read as double precision by default, but the smallest R type that exactly represents each external type is used if \code{fitnum} is \code{TRUE}.}

\details{This function returns the value of the attribute.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\note{\code{NC_BYTE} is always interpreted as signed.}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("att.get.nc")

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

##  Get these attributes
att.get.nc(nc, "temperature", "missing_value")
att.get.nc(nc, "temperature", "long_name")
att.get.nc(nc, "NC_GLOBAL", "title")
att.get.nc(nc, "NC_GLOBAL", "history")

close.nc(nc)
}

\keyword{file}
