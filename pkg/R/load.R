".First.lib" <- function(lib, pkg) {
    library.dynam("RNetCDF", pkg, lib)
    utinit.nc()
}
