\name{type.inq.nc}

\alias{type.inq.nc}

\title{Inquire About a NetCDF Type}

\description{Inquire about a NetCDF builtin or user-defined data type.}

\usage{type.inq.nc(ncfile, type, fields=TRUE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset or group.}
  \item{type}{ID or name of a NetCDF data type.}
  \item{fields}{Read members of enum types or fields of compound types (default \code{TRUE})}.
}

\value{
  A list containing the following components:
  \item{id}{Type ID.}
  \item{name}{Type name.}
  \item{class}{One of the keywords "builtin", "compound", "enum", "opaque" or "vlen".}
  \item{size}{Size in bytes of a single item of the type (or a single element of a "vlen").}
  \item{basetype}{("enum" or "vlen") Name of the NetCDF type of each element.}

  If \code{fields=TRUE}, the return list includes details about members of enum types or fields of compound types:
  \item{value}{("enum" only) Named vector with numeric values of all members.}
  \item{offset}{("compound" only) Named vector with the offset of each field in bytes from the beginning of the compound type.}
  \item{subtype}{("compound" only) Named vector with the NetCDF type name of each field.}
  \item{dimsizes}{("compound" only) Named list with array dimensions of each field. Dimension lengths are reported in R order (leftmost index varies fastest; opposite to CDL conventions). A \code{NULL} length indicates a scalar.}
}

\details{This function obtains information about a NetCDF data type, which could be builtin or user-defined. The items in the return list depend on the class of the NetCDF type.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Milton Woods}

\seealso{
  \code{\link{grp.inq.nc}} - get a list of NetCDF types defined in a dataset or group.
  \code{\link{type.def.nc}} - define a new NetCDF type.
  \code{\link{type.insert.nc}} - define components of an "enum" or "compound" type.
}

\examples{
##  Create a new NetCDF4 dataset and define types
nc <- create.nc("type.inq.nc", format="netcdf4")

# Define a type of each class:
type.def.nc(nc, "blob", "opaque", size=128)
type.def.nc(nc, "vector", "vlen", basetype="NC_FLOAT")

type.def.nc(nc, "factor", "enum", basetype="NC_INT")
type.insert.nc(nc, "factor", "peanut butter", value=101)
type.insert.nc(nc, "factor", "jelly", value=102)

type.def.nc(nc, "struct", "compound", size=4+8+3*2)
type.insert.nc(nc, "struct", "siteid", offset=0, subtype="NC_INT")
type.insert.nc(nc, "struct", "height", offset=4, subtype="NC_DOUBLE")
type.insert.nc(nc, "struct", "colour", offset=12, subtype="NC_SHORT", dimsizes=c(3))

# Inquire about the types:
typeids <- grp.inq.nc(nc)$typeids

for (typeid in typeids) {
  print(type.inq.nc(nc, typeid))
}

close.nc(nc)
}

\keyword{file}

