\name{type.def.nc}

\alias{type.def.nc}

\title{Define a NetCDF Type}

\description{Define complex data structures based on existing NetCDF data types.}

\usage{
type.def.nc(ncfile, typename, class, size=NULL, basetype=NULL,
            names=NULL, values=NULL, subtypes=NULL, dimsizes=NULL)
}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{typename}{Name to identify the new data type. Must begin with an alphabetic character, followed by zero or more alphanumeric characters including the underscore ("\code{_}"). Case is significant.}
  \item{class}{One of the keywords "compound", "enum", "opaque" or "vlen".}
  \item{size}{("opaque") Size in bytes of a single item of the opaque type.}
  \item{basetype}{("enum" or "vlen") Base type, given as the name or identifier of an existing NetCDF type. Only built-in integer types (e.g. "NC_INT") are allowed for \code{class} "enum".}
  \item{names}{("compound" or "enum") Name of each field or member (character vector).}
  \item{values}{("enum") Numeric value of each member (numeric vector).}
  \item{subtypes}{("compound") NetCDF type of each field, given by name (character vector) or identifier (numeric vector).}
  \item{dimsizes}{("compound") Array dimensions of each field, specified as a list of numeric vectors. Dimensions are given in R order (leftmost index varies fastest; opposite to CDL conventions). If a list item is \code{NULL}, the corresponding field is a scalar.}
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
}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF4 dataset and define types
nc <- create.nc("type.def.nc", format="netcdf4")

# Compound type:
type.def.nc(nc, "astruct", "compound",
            names=c("siteid", "height", "colour"),
            subtypes=c("NC_INT", "NC_DOUBLE", "NC_SHORT"),
            dimsizes=list(NULL, NULL, c(3)))

# Enum type:
type.def.nc(nc, "afactor", "enum", basetype="NC_INT",
            names=c("peanut butter", "jelly"),
            values=c(101,102))

# Opaque type:
type.def.nc(nc, "ablob", "opaque", size=128)

# Vlen type:
type.def.nc(nc, "avector", "vlen", basetype="NC_FLOAT")

close.nc(nc)
}

\keyword{file}
