\name{var.put.nc}

\alias{var.put.nc}

\title{Put Data Into a NetCDF Variable}

\description{Write the contents of a NetCDF variable.}

\usage{var.put.nc(ncfile, variable, data, start=NA, count=NA, na.mode=0, pack=FALSE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the variable.}
  \item{data}{The (multidimensional) array containing the data to write.}
  \item{start}{A vector of indices indicating where to start writing the passed values (beginning at 1). The length of this vector must equal the number of dimensions the variable has. Order is leftmost varying fastest (as got from \code{\link[RNetCDF]{print.nc}}; opposite to the CDL conventions). If set to \code{NA}, writing starts for each dimension at position 1.}
  \item{count}{A vector of integers indicating the count of values to write along each dimension. Order is leftmost varying fastest (as got from \code{\link[RNetCDF]{print.nc}}; opposite to the CDL conventions). The length of this vector must equal the number of dimensions the variable has. If set to \code{NA}, the dimesions are taken from \code{data}.}
  \item{na.mode}{Set the mode how missing values (\code{NA}) are handled: 0=accept \code{_FillValue} or \code{missing_value} attribute, 1=accept only \code{_FillValue} attribute, 2=accept only \code{missing_value} attribute.}
  \item{pack}{Variables are packed if \code{pack=TRUE} and the attributes \code{add_offset} and \code{scale_factor} are defined. Default is \code{FALSE}.}
}

\details{This function writes values to a NetCDF variable. Type conversion is done by the NetCDF library itself. This means, that double precision values are passed from R to the corresponding C-function, no matter which type the variable has. 

Only the R type \code{character} is treated separately. When writing values of type \code{NC_CHAR}, it is mandatory that the first element of \code{count} contains the value of this dimension's length (usually \code{max_string_length}), the maximum string length is given by this value. R arrays of type \code{character} need therefore one additional dimension when written to a NetCDF dataset.

Values of \code{NA} are supported if the variable's missing value attribute (as defined in \code{na.mode}) is set. They are converted to the corresponding value before written to disk. If \code{na.mode=0} and both attributes are defined, the value of \code{_FillValue} is used.

To reduce the storage space required by a NetCDF file, numeric variables can be "packed" into types of lower precision. The packing operation involves subtraction of attribute \code{add_offset} before division by attribute \code{scale_factor}. This packing operation is performed automatically for variables defined with the two attributes \code{add_offset} and \code{scale_factor} if argument \code{pack} is set to \code{TRUE}. If \code{pack} is \code{FALSE}, \code{data} values are assumed to be packed correctly and are written to the variable without alteration.

Data in a NetCDF file is conceived as being a multi-dimensional array. The number and length of dimensions is determined when the variable is created. The \code{start} and \code{count} indices that this routine takes indicate where the writing starts along each dimension, and the count of values along each dimension to write. 

Awkwardness arises mainly from one thing: NetCDF data are written with the last dimension varying fastest, whereas R works opposite. Thus, the order of the dimensions according to the CDL conventions (e.g., time, latitude, longitude) is reversed in the R array (e.g., longitude, latitude, time).}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\note{\code{NC_BYTE} is always interpreted as signed. For best performance, it is recommended that the definition of dimensions, variables and attributes is completed before variables are read or written.}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("foo.nc")

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

##  Put the data with indicated start/count
var.put.nc(nc, "time", mytime, 1, length(mytime))
var.put.nc(nc, "temperature", mytemperature, c(1,1), c(5,2))
var.put.nc(nc, "name", myname, c(1,1), c(32,5))

sync.nc(nc)

##  Put the data with default start/count
var.put.nc(nc, "time", mytime)
var.put.nc(nc, "temperature", mytemperature)
var.put.nc(nc, "name", myname)

close.nc(nc)
}

\keyword{file}
