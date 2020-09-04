\name{utinit.nc}

\alias{utinit.nc}

\title{Initialize the UDUNITS2 Library}

\description{Initialize the UDUNITS2 library.}

\usage{utinit.nc(path="")}

\arguments{
  \item{path}{Path to an XML-formatted unit-database for UDUNITS2.}
}

\details{This function initializes the UDUNITS2 library. It is called by \code{.onLoad} when the package is loaded. Normally, the user does not need to call this function.

UDUNITS2 obtains a unit system by reading an XML file. The file name is given by argument \code{path}, if it is a non-empty character string. Otherwise, the file name is taken from environment variable \code{UDUNITS2_XML_PATH}, if it is a non-empty character string. By default, the file name is set to an XML file distributed with RNetCDF.
}

\references{\url{https://www.unidata.ucar.edu/software/udunits/}}

\author{Pavel Michna, Milton Woods}

\keyword{utilities}
