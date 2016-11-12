\name{type.def.nc}

\alias{type.def.nc}

\title{Define a NetCDF Type}

\description{Define complex data structures based on existing NetCDF data types.}

\usage{type.def.nc(ncfile, typename, class, basetype=NULL, size=NULL)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{typename}{Name to identify the new data type. Must begin with an alphabetic character, followed by zero or more alphanumeric characters including the underscore ("\code{_}"). Case is significant.}
  \item{class}{One of the keywords "compound", "enum", "opaque" or "vlen".}
  \item{basetype}{Name or identifier of an existing NetCDF type. Required when \code{class} is "enum" or "vlen", ignored otherwise. Only built-in integer types (e.g. "NC_INT") are allowed for \code{class} "enum".}
  \item{size}{Size in bytes of a single item of the new type. Required when \code{class} is "compound" or "opaque", ignored otherwise.}
}

\value{NetCDF type identifier, returned invisibly.}

\details{User-defined types are supported by files in "netcdf4" format. This function creates a new NetCDF data type, which can be used in definitions of NetCDF variables and attributes.

Several varieties of data type are supported, as specified by argument \code{class}:

  \tabular{ll}{
    "compound" \tab Combines atomic and user-defined types into C-like structs. \cr
    "enum"     \tab Set of named integer values, similar to an R \code{factor}. \cr
    "opaque"   \tab Blobs of arbitrary data with a given size. \cr
    "vlen"     \tab Variable length vectors of a given base type. \cr
  }

The components of types in \code{class} "compound" and "enum" must be specified via function \code{type.insert.nc}.
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna}

\examples{
##  Create a new NetCDF4 dataset and define types
nc <- create.nc("type.def.nc", format="netcdf4")

# Tests commented out until type.insert.nc is implemented,
# otherwise file creation fails:
#type.def.nc(nc, "struct", "compound", size=32)
#type.def.nc(nc, "factor", "enum", basetype="NC_INT")

type.def.nc(nc, "blob", "opaque", size=128)
type.def.nc(nc, "vector", "vlen", basetype="NC_FLOAT")

close.nc(nc)
}

\keyword{file}
