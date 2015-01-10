".First.lib" <- function(lib, pkg) {
    Sys.setenv("UDUNITS_PATH"=system.file("udunits.dat", package=pkg))
    library.dynam("RNetCDF", pkg, lib)
    utinit.nc()
}
