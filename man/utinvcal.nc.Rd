\name{utinvcal.nc}

\alias{utinvcal.nc}

\title{Convert UTC Referenced Dates Into Temporal Amounts}

\description{Convert a UTC referenced date into a temporal amount.}

\usage{utinvcal.nc(unitstring, value)}

\arguments{
  \item{unitstring}{A temporal unit with an origin (e.g., ``days since 1900-01-01'').}
  \item{value}{A vector or a matrix containing the dates to convert. Each row must contain year, month, day, hour, minute and second.}
}

\value{A vector containing the amount(s) of the temporal unit(s) that the date(s) correspond(s) to.}

\details{Converts a UTC-referenced date and time into the amount, \code{value}, of the temporal unit, \code{unitstring}.
       
The udunits package uses a mixed Gregorian/Julian calendar system. Dates prior to 1582-10-15 are assumed to use the Julian calendar, which was introduced by Julius Caesar in 46 BCE and is based on a year that is exactly 365.25 days long. Dates on and after 1582-10-15 are assumed to use the Gregorian calendar, which was introduced on that date and is based on a year that is exactly 365.2425 days long. (A year is actually approximately 365.242198781 days long.) Seemingly strange behavior of the udunits package can result if a user-given time interval includes the changeover date.

If the dates are given in string form, the structure must be exactly \code{"YYYY-MM-DD hh:mm:ss"}.}

\references{\url{http://www.unidata.ucar.edu/packages/udunits/}}

\author{Pavel Michna}

\examples{
##  Convert UTC referenced time to other time units
utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(1900,1,1,5,25,0))
utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", "1900-01-01 05:25:00")
}

\keyword{utilities}
