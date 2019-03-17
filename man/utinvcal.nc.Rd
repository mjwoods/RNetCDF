\name{utinvcal.nc}

\alias{utinvcal.nc}

\title{Convert UTC Referenced Dates Into Temporal Amounts}

\description{Convert a UTC referenced date into a temporal amount.}

\usage{utinvcal.nc(unitstring, value)}

\arguments{
  \item{unitstring}{A temporal unit with an origin (e.g., ``days since 1900-01-01'').}
  \item{value}{Dates to convert as a numeric vector or array, or a vector of strings or POSIXct values.}
}

\value{A vector containing the amount(s) of the temporal unit(s) corresponding to the given date(s).}

\details{Uses the UDUNITS library to convert a UTC-referenced date and time into the amount, \code{value}, of the temporal unit, \code{unitstring}.
       
If the dates are given in string form, the structure must be exactly \code{"YYYY-MM-DD hh:mm:ss"}.

A vector of POSIXct values is also accepted as input. These are converted to the specified units by a linear transformation, without an intermediate separation into date components.
}

\seealso{\code{\link{utcal.nc}}}

\references{\url{http://www.unidata.ucar.edu/software/udunits/}}

\author{Pavel Michna, Milton Woods}

\examples{
if (inherits(try(utcal.nc("seconds since 1970-01-01", 0)), "try-error")) {
  warning("UDUNITS calendar conversions not supported by this build of RNetCDF")

} else {

  ##  Convert UTC referenced time to other time units
  utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(1900,1,1,5,25,0))
  utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", "1900-01-01 05:25:00")
  utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", ISOdatetime(1900,1,1,5,25,0,tz="UTC"))

}

##  An example of reading and writing a netcdf time coordinate 
##  is given in the help for utcal.nc
}

\keyword{utilities}
