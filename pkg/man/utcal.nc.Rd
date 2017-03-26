\name{utcal.nc}

\alias{utcal.nc}

\title{Convert Temporal Amounts to UTC Referenced Dates}

\description{Convert temporal amounts to UTC referenced date and time.}

\usage{utcal.nc(unitstring, value, type="n")}

\arguments{
  \item{unitstring}{A temporal unit with an origin (e.g., ``days since 1900-01-01'').}
  \item{value}{An amount (quantity) of the given temporal unit.}
  \item{type}{Character string which determines the output type. Can be \code{n} for numeric, \code{s} for string or \code{c} for POSIXct output.}
}

\value{If the output type is set to numeric, result is a matrix containing the corresponding date(s) and time(s), with the following columns: year, month, day, hour, minute, second. If the output type is string, result is a vector of strings in the form \code{"YYYY-MM-DD hh:mm:ss"}. Otherwise result is a vector of POSIXct values.}

\details{Converts the amount, \code{value}, of the temporal unit, \code{unitstring}, into a UTC-referenced date and time.
       
The UDUNITS package uses a mixed Gregorian/Julian calendar system. Dates prior to 1582-10-15 are assumed to use the Julian calendar, which was introduced by Julius Caesar in 46 BCE and is based on a year that is exactly 365.25 days long. Dates on and after 1582-10-15 are assumed to use the Gregorian calendar, which was introduced on that date and is based on a year that is exactly 365.2425 days long. (A year is actually approximately 365.242198781 days long.) Seemingly strange behavior of the UDUNITS package can result if a user-given time interval includes the changeover date.}

\references{\url{http://www.unidata.ucar.edu/software/udunits/}}

\author{Pavel Michna}

\examples{
##  Convert units to UTC referenced time
utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5))
utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5), type="s")
utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5), type="c")

## Create netcdf file with a time coordinate variable.
nt <- 24
time_posixct <- seq(ISOdatetime(1900,1,1,0,0,0,tz="UTC"), by="hour", len=nt)

time_unit <- "hours since 1900-01-01 00:00:00 +00:00"
time_coord <- utinvcal.nc(time_unit, time_posixct)

nc <- create.nc("utcal.nc")

att.put.nc(nc, "NC_GLOBAL", "Conventions", "NC_CHAR", "CF-1.6")
# Other recommended attributes are defined at http://cfconventions.org

dim.def.nc(nc, "time", nt)
var.def.nc(nc, "time", "NC_INT", "time")
att.put.nc(nc, "time", "units", "NC_CHAR", time_unit) 

var.put.nc(nc, "time", time_coord)

close.nc(nc)

## Read time coordinate from netcdf file:
nc <- open.nc("utcal.nc")

time_coord2 <- var.get.nc(nc, "time")
time_unit2 <- att.get.nc(nc, "time", "units")

close.nc(nc)

time_posixct2 <- utcal.nc(time_unit2, time_coord2, "c")

stopifnot(all.equal(time_posixct, time_posixct2))
}

\keyword{utilities}
