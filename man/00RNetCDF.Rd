\name{RNetCDF}

\alias{RNetCDF}

\title{R Interface to NetCDF Datasets}

\description{
This package provides an interface to Unidata's NetCDF library functions (version 4). The routines and the documentation follow the NetCDF C interface, so the corresponding manuals can be consulted for more detailed information.

NetCDF is an abstraction that supports a view of data as a collection of self-describing, portable objects that can be accessed through a simple interface. Array values may be accessed directly, without knowing details of how the data are stored. Auxiliary information about the data, such as what units are used, may be stored with the data. Generic utilities and application programs can access NetCDF datasets and transform, combine, analyze, or display specified fields of the data.

The external types supported by all versions of the NetCDF interface are:

  \tabular{ll}{
    \code{NC_CHAR}      \tab 8-bit characters intended for representing text. \cr
    \code{NC_BYTE}      \tab 8-bit signed integers. \cr
    \code{NC_SHORT}     \tab 16-bit signed integers. \cr
    \code{NC_INT}       \tab 32-bit signed integers. \cr
    \code{NC_FLOAT}     \tab 32-bit IEEE floating-point. \cr
    \code{NC_DOUBLE}    \tab 64-bit IEEE floating-point. \cr
  }

The NetCDF4 interface supports additional external types, including:

  \tabular{ll}{
    \code{NC_UBYTE}      \tab 8-bit unsigned integers. \cr
    \code{NC_USHORT}     \tab 16-bit unsigned integers. \cr
    \code{NC_UINT}       \tab 32-bit unsigned integers. \cr
    \code{NC_INT64}      \tab 64-bit signed integers. \cr
    \code{NC_UINT64}     \tab 64-bit unsigned integers. \cr
    \code{NC_STRING}     \tab variable length character strings. \cr
  }

These types are called ``external'', because they correspond to the portable external representation for NetCDF data. When a program reads external NetCDF data into an internal variable, the data is converted, if necessary, into the specified internal type. Similarly, if you write internal data into a NetCDF variable, this may cause it to be converted to a different external type, if the external type for the NetCDF variable differs from the internal type. 

First versions of the R and C code of this package were based on the \code{netCDF} package by Thomas Lumley and the \code{ncdf} package by David Pierce. Milton Woods added some enhancements of the NetCDF library versions 3.6 and 4.x.
}

\references{
  \url{http://www.unidata.ucar.edu/software/netcdf/}
}

\note{The NetCDF library must be already installed on the system.}

\author{Pavel Michna}

\keyword{file}
