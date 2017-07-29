# RNetCDF - Interface to NetCDF Datasets for R

RNetCDF provides an R interface to the NetCDF file format designed by Unidata
for efficient storage of array-oriented scientific data and descriptions.
This R interface is closely based on the C API of the NetCDF library,
and it includes calendar conversions from the Unidata UDUNITS library.
The current implementation supports operations on NetCDF datasets
in classic, 64-bit offset and netcdf4-classic file formats.
Work is progressing to add full support for netcdf4.

An introduction to the RNetCDF package is given in http://journal.r-project.org/archive/2013-2/michna-woods.pdf .
