\name{utcal.nc}

\alias{utcal.nc}

\title{Convert Temporal Amounts to UTC Referenced Dates}

\description{Convert temporal amounts to UTC referenced date and time.}

\usage{utcal.nc(unitstring, value)}

\arguments{
  \item{unitstring}{A temporal unit with an origin (e.g., ``days since 1900-01-01'').}
  \item{value}{An amount (quantity) of the given temporal unit.}
}

\details{Converts the amount, \code{value}, of the temporal unit, \code{unitstring}, into a UTC-referenced date and time.
       
The udunits package uses a mixed Gregorian/Julian calendar system. Dates prior to 1582-10-15 are assumed to use the Julian calendar, which was introduced by Julius Caesar in 46 BCE and is based on a year that is exactly 365.25 days long. Dates on and after 1582-10-15 are assumed to use the Gregorian calendar, which was introduced on that date and is based on a year that is exactly 365.2425 days long. (A year is actually approximately 365.242198781 days long.) Seemingly strange behavior of the udunits package can result if a user-given time interval includes the changeover date.}

\value{A matrix containing the corresponding date(s) and time(s), with the following columns: year, month, day, hour, minute, second.}

\references{\url{http://www.unidata.ucar.edu/packages/udunits/}}

\author{Pavel Michna}

\examples{
##  Convert units to UTC referenced time
utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5))
}

\keyword{utilities}
