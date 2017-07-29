\name{var.put.nc}

\alias{var.put.nc}

\title{Write Data to a NetCDF Variable}

\description{Write the contents of a NetCDF variable.}

\usage{var.put.nc(ncfile, variable, data, start=NULL, count=NULL, na.mode=0, pack=FALSE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the NetCDF variable.}
  \item{data}{An R vector or array of data to be written to the NetCDF variable. Values are taken from \code{data} in the order of R vector elements, so that leftmost indices vary fastest over an array.}
  \item{start}{A vector of indices specifying the element where writing starts along each dimension of \code{variable}. Indices are numbered from 1 onwards, and the order of dimensions is shown by \code{\link[RNetCDF]{print.nc}} (array elements are stored sequentially with leftmost indices varying fastest). Any \code{NA} values in \code{start} are set to 1. If necessary, \code{start} is truncated or extended with ones to the rank of \code{variable}. By default (\code{start=NULL}), all dimensions of \code{variable} are written from the first element onwards.}
  \item{count}{A vector of integers specifying the number of values to write along each dimension of \code{variable}. The order of dimensions is the same as for \code{start}. If necessary, \code{count} is truncated or extended with ones to the rank of \code{variable}. \code{NA} values indicate that the corresponding dimension of \code{variable} should be written from \code{start} to end. By default (\code{count=NULL}), the leading dimensions of \code{count} are set to the dimensions of \code{data}. Note that an unlimited dimension initially has zero length, and the dimension is extended by setting the corresponding element of \code{count} greater than the current length.}
  \item{na.mode}{Set the mode for handling missing values (\code{NA}) in numeric variables: 0=accept \code{_FillValue} or \code{missing_value} attribute, 1=accept only \code{_FillValue} attribute, 2=accept only \code{missing_value} attribute.}
  \item{pack}{Variables are packed if \code{pack=TRUE} and the attributes \code{add_offset} and \code{scale_factor} are defined. Default is \code{FALSE}.}
}

\details{This function writes values to a NetCDF variable. Type conversion is performed by the NetCDF library, so that numeric values in R are automatically converted to the correct type of NetCDF variable.

However, text represented by R types \code{raw} and \code{character} can only be written to NetCDF type \code{NC_CHAR}. The dimensions of R \code{raw} variables map directly to NetCDF dimensions, but \code{character} variables have an implied dimension corresponding to the string length. This implied dimension must be defined explicitly as the fastest-varying dimension of the \code{NC_CHAR} variable, and it must be included as the first element of arguments \code{start} and \code{count} taken by this function.

Values of \code{NA} are supported in numeric variables if the variable's missing value attribute (as defined in \code{na.mode}) is set. They are converted to the corresponding value before writing to disk. If \code{na.mode=0} and both attributes are defined, the value of \code{_FillValue} is used.

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
