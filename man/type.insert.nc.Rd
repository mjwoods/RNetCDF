\name{type.insert.nc}

\alias{type.insert.nc}

\title{Define Components of a NetCDF Type}

\description{Define fields of compound types or members of enumerated types created by \code{type.def.nc}.}

\usage{type.insert.nc(ncfile, type, name, value=NULL, offset=NULL, subtype=NULL, dimsizes=NULL)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{type}{ID or name of a data type created by \code{type.def.nc}.}
  \item{name}{Name of a new field in a compound type or member of an enumerated type.}
  \item{value}{(Enumerated types only) Numeric value of a new member.}
  \item{offset}{(Compound types only) Offset of this field in bytes from the beginning of the compound type. Offsets must be spaced widely enough that fields do not overlap, and they may include optional padding between fields.}
  \item{subtype}{(Compound types only) NetCDF type of this field, specified by NetCDF name or identifier.}
  \item{dimsizes}{(Compound types only) Dimensions of an array field, specified as a vector of lengths in R order (leftmost index varies fastest; opposite to CDL conventions). If \code{dimsizes} is \code{NULL} (default), the field is a scalar.}
}

\details{User-defined types are supported by files in "netcdf4" format. New data types are defined by \code{type.def.nc}, but certain classes of data type required further definition. This function allows new members to be inserted in enumerated types and new fields to be inserted in compound types.

NetCDF errors may occur if a file is closed without defining at least one component of an enumerated or compound type.
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}
\examples{
##  Create a new NetCDF4 dataset and define types
nc <- create.nc("type.insert.nc", format="netcdf4")

# Enum type:
type.def.nc(nc, "factor", "enum", basetype="NC_INT")
type.insert.nc(nc, "factor", "peanut butter", value=101)
type.insert.nc(nc, "factor", "jelly", value=102)

# Compound type:
type.def.nc(nc, "struct", "compound", size=4+8+3*2)
type.insert.nc(nc, "struct", "siteid", offset=0, subtype="NC_INT")
type.insert.nc(nc, "struct", "height", offset=4, subtype="NC_DOUBLE")
type.insert.nc(nc, "struct", "colour", offset=12, subtype="NC_SHORT", dimsizes=c(3))

close.nc(nc)
}

\keyword{file}
