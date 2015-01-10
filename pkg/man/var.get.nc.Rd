\name{var.get.nc}

\alias{var.get.nc}

\title{Get a NetCDF Variable}

\description{Get data from a NetCDF variable.}

\usage{var.get.nc(ncfile, variable, start=NA, count=NA, na.mode=0)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{ID or name of the variable.}
  \item{start}{A vector of indices indicating where to start reading the values (beginning at 1). The length of this vector must equal the number of dimensions the variable has. Order is leftmost varying fastest (as got from \code{\link[RNetCDF]{print.nc}}; opposite to the CDL conventions). If not specified (\code{start=NA}), reading starts at index 1.}
  \item{count}{A vector of integers indicating the count of values to read along each dimension. Order is leftmost varying fastest (as got from \code{\link[RNetCDF]{print.nc}}; opposite to the CDL conventions). The length of this vector must equal the number of dimensions the variable has. If not specified (\code{count=NA}), the entire variable or all values along the corresponding dimension(s) are read.}
  \item{na.mode}{Set the mode how missing values (\code{NA}) are handled: 0=accept \code{_FillValue} or \code{missing_value} attribute, 1=accept only \code{_FillValue} attribute, 2=accept only \code{missing_value} attribute.}
}

\details{This function returns the value of a variable. Returned values are always in ordinary R double precision (apart from character variables), no matter what precision they are in the on-disk dataset. 

Values of \code{NA} are supported; values in the data file that match the variable's missing value attribute (as defined in \code{na.mode}) are automatically converted to \code{NA} before being returned to the user. If \code{na.mode=0} and both attributes are defined, the value of \code{_FillValue} is used.

Data in a NetCDF file is conceived as being a multi-dimensional array. The number and length of dimensions is determined when the variable is created. The \code{start} and \code{count} indices that this routine takes indicate where the reading starts along each dimension, and the count of values along each dimension to read. 

Awkwardness arises mainly from one thing: NetCDF data are written with the last dimension varying fastest, whereas R works opposite. Thus, the order of the dimensions according to the CDL conventions (e.g., time, latitude, longitude) is reversed in the R array (e.g., longitude, latitude, time).}

\value{A multidimensional array of type \code{numeric} or \code{character} if the data type is \code{NC_CHAR}. No distinction is made between the different storage types of numeric objects. The dimension order according to the CDL conventions is swapped in the R array, because NetCDF data are written with the last dimension varying fastest, whereas R works opposite. Arrays of type \code{character} loose their first dimension, because strings can be indexed with one dimension in R and the first dimension (usually \code{max_string_length}) is therefore needless.}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\note{\code{NC_BYTE} is always interpreted as signed.}

\author{Pavel Michna}

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

##  Put the data
var.put.nc(nc, "time", mytime, 1, length(mytime))
var.put.nc(nc, "temperature", mytemperature, c(1,1), c(5,2))
var.put.nc(nc, "name", myname, c(1,1), c(32,5))

sync.nc(nc)

##  Get the data (or a subset)
var.get.nc(nc, 0)
var.get.nc(nc, "temperature")
var.get.nc(nc, "temperature", c(NA,2), c(NA,1))
var.get.nc(nc, "name")
var.get.nc(nc, "name", c(1,2), c(4,2))
var.get.nc(nc, "name", c(1,2), c(NA,2))

close.nc(nc)
}

\keyword{file}
