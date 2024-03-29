\name{att.inq.nc}

\alias{att.inq.nc}

\title{Inquire About a NetCDF Attribute}

\description{
  Inquire about a NetCDF attribute.
}

\usage{att.inq.nc(ncfile, variable, attribute)}

\arguments{
  \item{ncfile}{Object of class \code{NetCDF} which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{Either the ID or the name of the attribute's variable or \code{"NC_GLOBAL"} for a global attribute.}
  \item{attribute}{Either the ID or the name of the attribute to be inquired.}
}

\value{
  A list containing the following components:
  \item{id}{Attribute ID.}
  \item{name}{Attribute name.}
  \item{type}{External NetCDF data type as one of the following labels: \code{NC_BYTE}, \code{NC_UBYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_USHORT}, \code{NC_INT}, \code{NC_UINT}, \code{NC_INT64}, \code{NC_UINT64}, \code{NC_FLOAT}, \code{NC_DOUBLE}, \code{NC_STRING}, or a user-defined type name.}
  \item{length}{Length of this attribute.}
}

\details{This function returns information about a NetCDF attribute. Information about an attribute include its ID, its name, its type, and its length. In general, attributes are accessed by name rather than by their ID number because the attribute number is more volatile than the name, since it can change when other attributes of the same variable are deleted.

Valid attribute ID numbers range from 0 to the number of attributes minus 1. The number of attributes of a file, group, or variable can be found using the relevant inquiry function (\code{\link[RNetCDF]{file.inq.nc}}, \code{\link[RNetCDF]{grp.inq.nc}}, or \code{\link[RNetCDF]{var.inq.nc}}).
}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
file1 <- tempfile("att.inq_", fileext=".nc")
nc <- create.nc(file1)

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

##  Create two variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))

##  Put some attributes
att.put.nc(nc, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")

##  Inquire about these attributes
att.inq.nc(nc, "temperature", "_FillValue")
att.inq.nc(nc, "NC_GLOBAL", "title")

close.nc(nc)
unlink(file1)
}

\keyword{file}
