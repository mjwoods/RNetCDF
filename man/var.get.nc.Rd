\name{var.get.nc}

\alias{var.get.nc}

\title{Read Data from a NetCDF Variable}

\description{Read the contents of a NetCDF variable.}

\usage{var.get.nc(ncfile, variable, start=NA, count=NA,
        na.mode=4, collapse=TRUE, unpack=FALSE, rawchar=FALSE, fitnum=FALSE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the NetCDF variable.}
  \item{start}{A vector of indices specifying the element where reading starts along each dimension of \code{variable}. Indices are numbered from 1 onwards, and the order of dimensions is shown by \code{\link[RNetCDF]{print.nc}} (array elements are stored sequentially with leftmost indices varying fastest). By default (\code{start=NA}), all dimensions of \code{variable} are read from the first element onwards. Otherwise, \code{start} must be a vector whose length is not less than the number of dimensions in \code{variable} (excess elements are ignored). Any \code{NA} values in vector \code{start} are set to 1.}
  \item{count}{A vector of integers specifying the number of values to read along each dimension of \code{variable}. The order of dimensions is the same as for \code{start}. By default (\code{count=NA}), all dimensions of \code{variable} are read from \code{start} to end. Otherwise, \code{count} must be a vector whose length is not less than the number of dimensions in \code{variable} (excess elements are ignored). Any \code{NA} value in vector \code{count} indicates that the corresponding dimension should be read from the \code{start} index to the end of the dimension.}
  \item{na.mode}{Set the mode for handling missing values (\code{NA}) in numeric variables: 0=accept \code{_FillValue}, then \code{missing_value} attribute; 1=accept only \code{_FillValue} attribute; 2=accept only \code{missing_value} attribute; 3=no missing value conversion; 4=valid range from valid_min and valid_max or valid_range, fill value from _FillValue, with defaults for each type except \code{NC_BYTE} and \code{NC_UBYTE} (see \url{http://www.unidata.ucar.edu/software/netcdf/docs/attribute_conventions.html}).}
  \item{collapse}{\code{TRUE} if degenerated dimensions (length=1) should be omitted.}
  \item{unpack}{Packed variables are unpacked if \code{unpack=TRUE} and the attributes \code{add_offset} and \code{scale_factor} are defined. Default is \code{FALSE}.}
  \item{rawchar}{This option only relates to NetCDF variables of type \code{NC_CHAR}. When \code{rawchar} is \code{FALSE} (default), a NetCDF variable of type \code{NC_CHAR} is converted to a \code{character} array in R. The \code{character} values are from the fastest-varying dimension of the NetCDF variable, so that the R \code{character} array has one fewer dimensions than the \code{NC_CHAR} array. If \code{rawchar} is \code{TRUE}, the bytes of \code{NC_CHAR} data are read into an R \code{raw} array of the same shape.}
  \item{fitnum}{By default, all numeric variables are read into R as double precision values. When \code{fitnum==TRUE}, the smallest R numeric type that can exactly represent each external type is used, as follows:
  \tabular{ll}{
    \code{NC_BYTE}       \tab \code{\link[base]{integer}} \cr
    \code{NC_UBYTE}      \tab \code{\link[base]{integer}} \cr
    \code{NC_SHORT}      \tab \code{\link[base]{integer}} \cr
    \code{NC_USHORT}     \tab \code{\link[base]{integer}} \cr
    \code{NC_INT}        \tab \code{\link[base]{integer}} \cr
    \code{NC_UINT}       \tab \code{\link[base]{double}} \cr
    \code{NC_FLOAT}      \tab \code{\link[base]{double}} \cr
    \code{NC_DOUBLE}     \tab \code{\link[base]{double}} \cr
    \code{NC_INT64}      \tab \code{\link[bit64]{integer64}} \cr
    \code{NC_UINT64}     \tab \code{\link[bit64]{integer64}} \cr
  }}
}

\details{
NetCDF numeric variables cannot portably represent \code{NA} values from R. NetCDF does allow attributes to be defined for variables, and several conventions exist for attributes that define missing values and valid ranges. The convention in use can be specified by argument \code{na.mode}. Values of a NetCDF variable that are deemed to be missing are automatically converted to \code{NA} in the results returned to R. Unusual cases can be handled directly in user code by setting \code{na.mode=3}.

To reduce the storage space required by a NetCDF file, numeric variables are sometimes "packed" into types of lower precision. The original data can be recovered (approximately) by multiplication of the stored values by attribute \code{scale_factor} followed by addition of attribute \code{add_offset}. This unpacking operation is performed automatically for variables with attributes \code{scale_factor} and \code{add_offset} if argument \code{unpack} is set to \code{TRUE}. If \code{unpack} is \code{FALSE}, values are read from each variable without alteration.

Data in a NetCDF variable is represented as a multi-dimensional array. The number and length of dimensions is determined when the variable is created. The \code{start} and \code{count} arguments of this routine indicate where the reading starts and the number of values to read along each dimension.

The argument \code{collapse} allows to keep degenerated dimensions (if set to \code{FALSE}). As default, array dimensions with length=1 are omitted (e.g., an array with dimensions [2,1,3,4] in the NetCDF dataset is returned as [2,3,4]).

Awkwardness arises mainly from one thing: NetCDF data are written with the last dimension varying fastest, whereas R works opposite. Thus, the order of the dimensions according to the CDL conventions (e.g., time, latitude, longitude) is reversed in the R array (e.g., longitude, latitude, time).}

\value{An array with dimensions determined by \code{count} and a data type that depends on the type of \code{variable}. For NetCDF variables of type \code{NC_CHAR}, the R type is either \code{character} or \code{raw}, as specified by argument \code{rawchar}. For \code{NC_STRING}, the R type is \code{character}. Numeric variables are read as double precision by default, but the smallest R type that exactly represents each external type is used if \code{fitnum} is \code{TRUE}.

Variables of user-defined types are supported. "compound" arrays are read into R as lists, with items named for the compound fields; items of base NetCDF data types are converted to R arrays, with leading dimensions from the field dimensions (if any) and trailing dimensions from the NetCDF variable. "enum" arrays are read into R as factor arrays. "opaque" arrays are read into R as raw (byte) arrays, with a leading dimension for bytes of the opaque type and trailing dimensions from the NetCDF variable. "vlen" arrays are read into R as a list with dimensions of the NetCDF variable; items in the list may have different lengths; base NetCDF data types are converted to R vectors.

The dimension order in the R array is reversed relative to the order reported by NetCDF commands such as \code{ncdump}, because NetCDF arrays are stored in row-major (C) order whereas R arrays are stored in column-major (Fortran) order.

Arrays of type \code{character} drop the fastest-varying dimension of the corresponding \code{NC_CHAR} array, because this dimension corresponds to the length of the individual \code{character} elements. For example, an \code{NC_CHAR} array with dimensions (5,10) would be returned as a \code{character} vector containing 5 elements, each with a maximum length of 10 characters.}

\references{\url{http://www.unidata.ucar.edu/software/netcdf/}}

\note{\code{NC_BYTE} is always interpreted as signed.}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("var.get.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)
dim.def.nc(nc, "max_string_length", 32)

##  Create three variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))
var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))

##  Put some _FillValue attribute for temperature
att.put.nc(nc, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)

##  Define variable values
mytime        <- c(1:2)
mytemperature <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9)
myname        <- c("alfa", "bravo", "charlie", "delta", "echo")

##  Put the data
var.put.nc(nc, "time", mytime, 1, length(mytime))
var.put.nc(nc, "temperature", mytemperature, c(1,1), c(5,2))
var.put.nc(nc, "name", myname, c(1,1), c(32,5))

sync.nc(nc)

##  Get the data (or a subset)
var.get.nc(nc, 0)
var.get.nc(nc, "temperature")
var.get.nc(nc, "temperature", c(3,1), c(1,1))
var.get.nc(nc, "temperature", c(3,2))
var.get.nc(nc, "temperature", c(NA,2), c(NA,1))
var.get.nc(nc, "name")
var.get.nc(nc, "name", c(1,2), c(4,2))
var.get.nc(nc, "name", c(1,2), c(NA,2))

close.nc(nc)
}

\keyword{file}
