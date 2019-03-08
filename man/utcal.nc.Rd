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

Functions \code{utcal.nc} and \code{utinvcal.nc} provide a convenient way to convert time values between the forms used by NetCDF variables and R functions. Most R functions require times to be expressed as seconds since the beginning of 1970 in the UTC time zone, typically using objects of class \code{POSIXct} or \code{POSIXlt}. NetCDF files store times in numeric variables with a wide variety of units. The units and calendar are stored in attributes of the time variable, as described by the CF Conventions. Units are expressed as a string, in the form of a time unit since a fixed date-time (e.g. ``hours since 2000-01-01 00:00:00 +00:00'', or more simply ``hours since 2000-01-01'').

Time values are converted to \code{\link{POSIXct}} by the UDUNITS library via \code{\link[units]{as_units}}. When the requested \code{type} is numeric or string, \code{\link{POSIXct}} values are converted using \code{\link{as.POSIXlt}} or \code{\link{format.POSIXct}}.

Functions \code{utcal.nc} and \code{utinvcal.nc} do not provide control over the calendar used for date conversions, and results may differ between platforms for dates that are far from the epoch (1970-01-01). If the details of calendars are important for your application, please consider using a specialised calendar package in conjunction with the file operations provided by RNetCDF.
}

\seealso{\code{\link{utinvcal.nc}}}

\references{\url{http://www.unidata.ucar.edu/software/udunits/}

\url{http://cfconventions.org}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Convert units to UTC referenced time
utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5))
utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5), type="s")
utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5), type="c")

## Create netcdf file with a time coordinate variable.

# Create a time variable (using type POSIXct for convenience):
nt <- 24
time_posixct <- seq(ISOdatetime(1900,1,1,0,0,0,tz="UTC"), by="hour", len=nt)

# Convert time variable to specified units:
time_unit <- "hours since 1900-01-01 00:00:00 +00:00"
time_coord <- utinvcal.nc(time_unit, time_posixct)

# Create a netcdf file:
nc <- create.nc("utcal.nc")

# Global attributes:
att.put.nc(nc, "NC_GLOBAL", "Conventions", "NC_CHAR", "CF-1.6")
att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "RNetCDF example: time coordinate")
att.put.nc(nc, "NC_GLOBAL", "institution", "NC_CHAR", "University of Areland")
att.put.nc(nc, "NC_GLOBAL", "source", "NC_CHAR", 
  paste("RNetCDF", installed.packages()["RNetCDF","Version"], sep="_"))
att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR",
  paste(Sys.time(), "File created"))
att.put.nc(nc, "NC_GLOBAL", "references", "NC_CHAR",
  "http://www.unidata.ucar.edu/software/udunits")
att.put.nc(nc, "NC_GLOBAL", "comment", "NC_CHAR",
  "Uses attributes recommended by http://cfconventions.org")

# Define time coordinate and attributes:
dim.def.nc(nc, "time", nt)
var.def.nc(nc, "time", "NC_DOUBLE", "time")
att.put.nc(nc, "time", "long_name", "NC_CHAR", "time")
att.put.nc(nc, "time", "units", "NC_CHAR", time_unit)
# Calendar is optional (gregorian is the default):
att.put.nc(nc, "time", "calendar", "NC_CHAR", "gregorian")

# Write the data:
var.put.nc(nc, "time", time_coord)

close.nc(nc)

## Read time coordinate from netcdf file:

# Open the file prepared earlier:
nc <- open.nc("utcal.nc")

# Read time coordinate and attributes:
time_coord2 <- var.get.nc(nc, "time")
time_unit2 <- att.get.nc(nc, "time", "units")

close.nc(nc)

# Convert the time variable to POSIXct:
time_posixct2 <- utcal.nc(time_unit2, time_coord2, "c")

# Compare with original POSIXct variable:
stopifnot(all.equal(time_posixct, time_posixct2))
}

\keyword{utilities}
