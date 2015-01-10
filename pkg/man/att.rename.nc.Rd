\name{att.rename.nc}

\alias{att.rename.nc}

\title{Rename a NetCDF Attribute}

\description{Rename a NetCDF attribute.}

\usage{att.rename.nc(ncfile, variable, attribute, newname)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the attribute's variable, or \code{"NC_GLOBAL"} for a global attribute.}
  \item{attribute}{The current attribute name or ID.}
  \item{newname}{The new name to be assigned to the specified attribute.}
}

\details{This function changes the name of an existing attribute in a NetCDF dataset open for writing. An attribute cannot be renamed to have the same name as another attribute of the same variable.}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

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
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")

##  Rename these attributes
att.rename.nc(nc, "temperature", "missing_value", "my_missing_value")
att.rename.nc(nc, "NC_GLOBAL", "title", "my_title")

close.nc(nc)
}

\keyword{file}
