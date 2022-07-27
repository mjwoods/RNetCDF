\name{create.nc}

\alias{create.nc}

\title{Create a NetCDF Dataset}

\description{Create a new NetCDF dataset.}

\usage{create.nc(filename, clobber=TRUE, share=FALSE, prefill=TRUE,
         format="classic", large=FALSE, diskless=FALSE, persist=FALSE,
         mpi_comm=NULL, mpi_info=NULL)}

\arguments{
  \item{filename}{Filename for the NetCDF dataset to be created.}
  \item{clobber}{The creation mode. If \code{TRUE} (default), any existing dataset with the same filename will be overwritten. Otherwise set to \code{FALSE}.}
  \item{share}{The buffer scheme. If \code{FALSE} (default), dataset access is buffered and cached for performance. However, if one or more processes may be reading while another process is writing the dataset, set to \code{TRUE}.}
  \item{prefill}{The prefill mode. If \code{TRUE} (default), newly defined variables are initialised with fill values when they are first accessed. This allows unwritten array elements to be detected when reading, but it also implies duplicate writes if all elements are subsequently written with user-specified data. Enhanced write performance can be obtained by setting \code{prefill=FALSE}.}
  \item{format}{The file format. One of \code{"classic"}, \code{"offset64"}, \code{"data64"}, \code{"netcdf4"} or \code{"classic4"}. See below for details.}
  \item{large}{(Deprecated) \code{large=TRUE} sets the file format to \code{"offset64"} when \code{format="classic"}.}
  \item{diskless}{When \code{diskless=TRUE}, the file is created in memory without writing to disk. This allows netcdf datasets to be used as fast, temporary files. When the file is closed, the contents are lost unless \code{persist=TRUE}.}
  \item{persist}{When \code{persist=TRUE}, a file created with \code{diskless=TRUE} is flushed to disk when closed. In some cases, this may be faster than manipulating files directly on disk.}
  \item{mpi_comm}{Fortran handle of MPI communicator for parallel I/O. The default of \code{NULL} implies serial I/O. Valid Fortran handles may be obtained from your chosen MPI package for R - for example \link[pbdMPI]{comm.c2f}.}
  \item{mpi_info}{Fortran handle of MPI Info object for parallel I/O. The default value \code{NULL} implies serial I/O. Valid Fortran handles may be obtained from your chosen MPI package for R - for example \link[pbdMPI]{info.c2f}.}
}

\value{Object of class \code{NetCDF} which points to the NetCDF dataset, returned invisibly.}

\details{This function creates a new NetCDF dataset, returning an object of class \code{NetCDF} that can be used in R.

The file format is specified by the \code{format} argument, which may take the following values:
\describe{
  \item{\code{"classic"}}{(default) Original netcdf file format, still widely used and recommended for maximum portability of datasets. Uses a signed 32-bit offset in its internal structures, so files larger than 2GB can only be created under limited conditions.}
  \item{\code{"offset64"}}{64-bit offset extension of original format, introduced by netcdf-3.6. Allows larger files and variables than \code{"classic"} format, but there remain some restrictions on files larger than 2GB.}
  \item{\code{"data64"}}{Extension of \code{"classic"} format to support large files (i.e. over 2GB) and large variables (over 2B array elements). This format was introduced in netcdf-4.4.0.}
  \item{\code{"netcdf4"}}{Netcdf in an HDF5 container, introduced by netcdf-4.0. Allows dataset sizes up to filesystem limits, and extends the feature set of the older formats.}
  \item{\code{"classic4"}}{Same file format as \code{"netcdf4"}, but this option ensures that only classic netcdf data structures are stored in the file for compatibility with older software (when linked with the netcdf4 library).}
}
}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create empty NetCDF datasets with different formats
file1 <- tempfile("create3_", fileext=".nc")
nc <- create.nc(file1)
close.nc(nc)
unlink(file1)

file2 <- tempfile("create64_", fileext=".nc")
nc2 <- create.nc(file2,format="offset64")
close.nc(nc2)
unlink(file2)

file3 <- tempfile("create4_", fileext=".nc")
nc3 <- create.nc(file3,format="netcdf4")
close.nc(nc3)
unlink(file3)
}

\keyword{file}
