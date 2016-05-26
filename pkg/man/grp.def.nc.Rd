\name{grp.def.nc}

\alias{grp.def.nc}

\title{Define a NetCDF Group}

\description{Define a NetCDF Group.}

\usage{grp.def.nc(ncid, grpname)}

\arguments{
  \item{ncid}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}) or parent group (as returned by this function).}
  \item{grpname}{Group name. Must begin with an alphabetic character, followed by zero or more alphanumeric characters including the underscore ("\code{_}"). Case is significant.}
}

\details{This function may only be used with files in netcdf4 format. It creates a new NetCDF group, which may be used as a container for other NetCDF objects, including groups, dimensions, variables and attributes.

Most NetCDF object types, including groups, variables and "global" attributes, are visible only in the group where they are defined. However, dimensions are visible in their groups and all child groups.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Milton Woods}

\examples{
##  Create a new NetCDF4 dataset
nc <- create.nc("foo.nc", format="netcdf4")

## Define dimensions, variables and attributes in the root group
dim.def.nc(nc, "station", 5)
var.def.nc(nc, "station", "NC_CHAR", c("station"))
att.put.nc(nc, "NC_GLOBAL", "Description", "NC_CHAR", "Site-based measurements") 

## Define a group
grp <- grp.def.nc(nc, "time_series")

## Define dimensions and variables in the new group
dim.def.nc(grp, "time", unlim=TRUE)
var.def.nc(grp, "time", "NC_INT", "time")
var.def.nc(grp, "temperature", "NC_DOUBLE", c("station","time"))
att.put.nc(nc, "NC_GLOBAL", "Description", "NC_CHAR", "Time-series at sites")

close.nc(nc)
}

\keyword{file}
