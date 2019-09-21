\name{RNetCDF}

\alias{RNetCDF}
\alias{RNetCDF-package}

\title{R Interface to NetCDF Datasets}

\section{Overview}{
This package provides an interface to Unidata's NetCDF library functions (version 4) and furthermore access to Unidata's UDUNITS (version 2) calendar conversions. The routines and the documentation follow the NetCDF and UDUNITS C interface, so the corresponding manuals can be consulted for more detailed information.

NetCDF is an abstraction that supports a view of data as a collection of self-describing, portable objects that can be accessed through a simple interface. Array values may be accessed directly, without knowing details of how the data are stored. Auxiliary information about the data, such as what units are used, may be stored with the data. Generic utilities and application programs can access NetCDF datasets and transform, combine, analyze, or display specified fields of the data.

First versions of the R and C code of this package were based on the \code{netCDF} package by Thomas Lumley and the \code{ncdf} package by David Pierce. Milton Woods added some enhancements of the NetCDF library versions 3.6 and 4.x.
}

\section{Functions}{
Help pages are available for the following RNetCDF functions:

  \tabular{ll}{
    \bold{Category} \tab \bold{Function} \cr
    Dataset   \tab \code{\link{close.nc}} \cr
              \tab \code{\link{create.nc}} \cr
              \tab \code{\link{file.inq.nc}} \cr
              \tab \code{\link{open.nc}} \cr
              \tab \code{\link{print.nc}} \cr
              \tab \code{\link{read.nc}} \cr
              \tab \code{\link{sync.nc}} \cr
    Group     \tab \code{\link{grp.def.nc}} \cr
              \tab \code{\link{grp.inq.nc}} \cr
              \tab \code{\link{grp.rename.nc}} \cr
    Attribute \tab \code{\link{att.copy.nc}} \cr
              \tab \code{\link{att.delete.nc}} \cr
              \tab \code{\link{att.get.nc}} \cr
              \tab \code{\link{att.inq.nc}} \cr
              \tab \code{\link{att.put.nc}} \cr
              \tab \code{\link{att.rename.nc}} \cr
    Dimension \tab \code{\link{dim.def.nc}} \cr
              \tab \code{\link{dim.inq.nc}} \cr
              \tab \code{\link{dim.rename.nc}} \cr
    Data type \tab \code{\link{type.def.nc}} \cr
              \tab \code{\link{type.inq.nc}} \cr
    Variable  \tab \code{\link{var.def.nc}} \cr
              \tab \code{\link{var.get.nc}} \cr
              \tab \code{\link{var.inq.nc}} \cr
              \tab \code{\link{var.put.nc}} \cr
              \tab \code{\link{var.rename.nc}} \cr
    Calendar  \tab \code{\link{utcal.nc}} \cr
              \tab \code{\link{utinit.nc}} \cr
              \tab \code{\link{utinvcal.nc}}
  }
}

\section{Data Types}{
The external types supported by all NetCDF datasets are:

  \tabular{ll}{
    \code{NC_CHAR}      \tab 8-bit characters intended for representing text. \cr
    \code{NC_BYTE}      \tab 8-bit signed integers. \cr
    \code{NC_SHORT}     \tab 16-bit signed integers. \cr
    \code{NC_INT}       \tab 32-bit signed integers. \cr
    \code{NC_FLOAT}     \tab 32-bit IEEE floating-point. \cr
    \code{NC_DOUBLE}    \tab 64-bit IEEE floating-point. \cr
  }

Datasets in NetCDF4 format support additional external types, including:

  \tabular{ll}{
    \code{NC_UBYTE}      \tab 8-bit unsigned integers. \cr
    \code{NC_USHORT}     \tab 16-bit unsigned integers. \cr
    \code{NC_UINT}       \tab 32-bit unsigned integers. \cr
    \code{NC_INT64}      \tab 64-bit signed integers. \cr
    \code{NC_UINT64}     \tab 64-bit unsigned integers. \cr
    \code{NC_STRING}     \tab variable length character strings. \cr
  }

These types are called ``external'', because they correspond to the portable external representation for NetCDF data. When a program reads external NetCDF data into an internal variable, the data is converted, if necessary, into the specified internal type. Similarly, if you write internal data into a NetCDF variable, this may cause it to be converted to a different external type, if the external type for the NetCDF variable differs from the internal type. 

In addition to the external types, NetCDF4 supports user-defined types. See \code{\link{type.def.nc}} for more explanation.
}

\references{
  \url{http://www.unidata.ucar.edu/software/netcdf/}
  
  \url{http://www.unidata.ucar.edu/software/udunits/}
}

\note{When installing RNetCDF from source code, the netcdf4 library and header files must be installed on the system. Calendar functions will only be enabled in RNetCDF if the udunits2 library and header files are detected during the build process.}

\author{Pavel Michna, Milton Woods}

\keyword{file}
