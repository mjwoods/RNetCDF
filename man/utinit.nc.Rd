\name{utinit.nc}

\alias{utinit.nc}

\title{Initialize the UDUNITS Library}

\description{Initialize the UDUNITS library.}

\usage{utinit.nc(path="")}

\arguments{
  \item{path}{Path to a units file containing initializing unit definitions.}
}

\details{This function initializes the UDUNITS library. It is called by \code{.First.lib} when the package is loaded. Normally, the user does not need to call this function.

If path is non-NULL and not empty, then it specifies a units file containing initializing unit definitions; otherwise, the environment variable \code{UDUNITS_PATH} is checked and, if it exists and is not empty, then it is assumed to contain the pathname of the units file; otherwise, a compile-time default pathname is used.      
}

\references{\url{http://www.unidata.ucar.edu/software/udunits/}}

\author{Pavel Michna}

\keyword{utilities}
