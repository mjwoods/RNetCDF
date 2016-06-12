\name{grp.inq.nc}

\alias{grp.inq.nc}

\title{Inquire About a NetCDF Group}

\description{Inquire about a NetCDF group.}

\usage{grp.inq.nc(ncid,grpname=NULL)}

\arguments{
  \item{ncid}{Object of class "\code{NetCDF}" which points to a NetCDF group (from \code{\link[RNetCDF]{grp.def.nc}}) or dataset (from \code{\link[RNetCDF]{open.nc}}).}
  \item{grpname}{By default, the inquiry relates to the group represented by \code{ncid}. If \code{grpname} is a character string, a group with this name is examined instead. A hierarchical search is performed if \code{grpname} contains "/", otherwise only the immediate group of \code{ncid} is searched for a matching group name.}
}

\details{This function provides information about the structure of a NetCDF group or dataset. The results allow programs to explore a dataset without prior knowledge of the contents.}

\value{
  A list containing the following components:
  \item{self}{Object of class \code{NetCDF} representing the group.}
  \item{parent}{Object of class \code{NetCDF} representing the parent group, or \code{NULL} for the root group.}
  \item{grps}{List of objects of class \code{NetCDF} representing the groups in the group.}
  \item{name}{Name of the NetCDF group.}
  \item{fullname}{Full name of the NetCDF group, with ancestors listed in order from the root group of the dataset and separated by "/".} 
  \item{dimids}{Vector of identifiers for dimensions in the group and its ancestors.}
  \item{unlimids}{Vector of identifiers for unlimited dimensions in the group and its ancestors.}
  \item{varids}{Vector of identifiers for variables in the group.}
  \item{typeids}{Vector of identifiers for types in the group.}
  \item{ngatts}{Number of group attributes.}
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

##  Put some attributes
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from Foo")
att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date()))

##  Inquire about the root group
grp.inq.nc(nc)

close.nc(nc)
}

\keyword{file}
