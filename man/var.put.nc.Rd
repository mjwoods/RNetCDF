\name{var.put.nc}

\alias{var.put.nc}

\title{Write Data to a NetCDF Variable}

\description{Write the contents of a NetCDF variable.}

\usage{var.put.nc(ncfile, variable, data, start=NA, count=NA, na.mode=0, pack=FALSE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the NetCDF variable.}
  \item{data}{An R vector or array of data to be written to the NetCDF variable. Values are taken from \code{data} in the order of R vector elements, so that leftmost indices vary fastest over an array.}
  \item{start}{A vector of indices specifying the element where writing starts along each dimension of \code{variable}. Indices are numbered from 1 onwards, and the order of dimensions is shown by \code{\link[RNetCDF]{print.nc}} (array elements are stored sequentially with leftmost indices varying fastest). By default (\code{start=NA}), all dimensions of \code{variable} are written from the first element onwards. Otherwise, \code{start} must be a vector whose length is not less than the number of dimensions in \code{variable} (excess elements are ignored). Any \code{NA} values in vector \code{start} are set to 1.}
  \item{count}{A vector of integers specifying the number of values to write along each dimension of \code{variable}. The order of dimensions is the same as for \code{start}. By default (\code{count=NA}), \code{count} is set to \code{dim(data)} for an array or \code{length(data)} for a vector. Otherwise, \code{count} must be a vector whose length is not less than the number of dimensions in \code{variable} (excess elements are ignored). Any \code{NA} value in vector \code{count} indicates that the corresponding dimension should be written from the \code{start} index to the end of the dimension. Note that an unlimited dimension initially has zero length, and the dimension is extended by setting the corresponding element of \code{count} greater than the current length.}
  \item{na.mode}{Set the mode for handling missing values (\code{NA}) in numeric variables: 0=accept \code{_FillValue}, then \code{missing_value} attribute; 1=accept only \code{_FillValue} attribute; 2=accept only \code{missing_value} attribute; 3=no missing value conversion; 4=valid range from valid_min and valid_max or valid_range, fill value from _FillValue, with defaults for each type (see \url{http://www.unidata.ucar.edu/software/netcdf/docs/attribute_conventions.html}).}
  \item{pack}{Variables are packed if \code{pack=TRUE} and the attributes \code{add_offset} and \code{scale_factor} are defined. Default is \code{FALSE}.}
}

\details{This function writes values to a NetCDF variable. Data values in R are automatically converted to the correct type of NetCDF variable.

Text represented by R type \code{character} can be written to NetCDF types \code{NC_CHAR} and \code{NC_STRING}, and R type \code{raw} can be written to NetCDF type \code{NC_CHAR}. When writing to \code{NC_CHAR} variables, \code{character} variables have an implied dimension corresponding to the string length. This implied dimension must be defined explicitly as the fastest-varying dimension of the \code{NC_CHAR} variable, and it must be included as the first element of arguments \code{start} and \code{count} taken by this function.

Due to the lack of native support for 64-bit integers in R, NetCDF types \code{NC_INT64} and \code{NC_UINT64} require special attention. This function accepts the usual R \code{integer} (signed 32-bit) and \code{numeric} (double precision) types, but to represent integers larger than about 53-bits without truncation, \code{character} vectors are also supported.

NetCDF numeric variables cannot portably represent \code{NA} values from R. NetCDF does allow attributes to be defined for variables, and several conventions exist for attributes that define missing values and valid ranges. The convention in use can be specified by argument \code{na.mode}. Values of \code{NA} in argument \code{data} are converted to a missing or fill value before writing to the NetCDF variable. Unusual cases can be handled directly in user code by setting \code{na.mode=3}.

Variables of user-defined types are supported, with assumptions about the corresponding data structures in R. "vlen" arrays must be stored in R as a list of vectors; array dimensions may be applied to the list. "opaque" arrays must be stored in R as raw (byte) arrays, with dimensions of the netcdf variable extended by a leading dimension for bytes of the opaque type. "enum" arrays must be stored in R as factor arrays.

To reduce the storage space required by a NetCDF file, numeric variables can be "packed" into types of lower precision. The packing operation involves subtraction of attribute \code{add_offset} before division by attribute \code{scale_factor}. This packing operation is performed automatically for variables defined with the two attributes \code{add_offset} and \code{scale_factor} if argument \code{pack} is set to \code{TRUE}. If \code{pack} is \code{FALSE}, \code{data} values are assumed to be packed correctly and are written to the variable without alteration.

Data in a NetCDF variable is represented as a multi-dimensional array. The number and length of dimensions is determined when the variable is created. The \code{start} and \code{count} arguments of this routine indicate where the writing starts and the number of values to write along each dimension.

Awkwardness arises mainly from one thing: NetCDF data are written with the last dimension varying fastest, whereas R works opposite. Thus, the order of the dimensions according to the CDL conventions (e.g., time, latitude, longitude) is reversed in the R array (e.g., longitude, latitude, time).}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\note{\code{NC_BYTE} is always interpreted as signed. For best performance, it is recommended that the definition of dimensions, variables and attributes is completed before variables are read or written.}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("var.put.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)
dim.def.nc(nc, "max_string_length", 32)

##  Create three variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))
var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))

##  Put some missing_value attribute for temperature
att.put.nc(nc, "temperature", "missing_value", "NC_DOUBLE", -99999.9)

##  Define variable values
mytime        <- c(1:2)
mytemperature <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9)
myname        <- c("alfa", "bravo", "charlie", "delta", "echo")

dim(mytemperature) <- c(5,2)

##  Put subsets of the data:
var.put.nc(nc, "time", mytime, start=2, count=1)
var.put.nc(nc, "temperature", mytemperature[3:4,2], start=c(3,2), count=c(2,1))
var.put.nc(nc, "name", myname[3:4], start=c(NA,3), count=c(NA,2))
sync.nc(nc)

##  Put all of the data:
var.put.nc(nc, "time", mytime)
var.put.nc(nc, "temperature", mytemperature)
var.put.nc(nc, "name", myname)

close.nc(nc)
}

\keyword{file}
