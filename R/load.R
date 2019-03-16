".onLoad" <- function(lib, pkg) {
    utinit.nc()
}


".onUnload" <- function(libpath) {
    .Call(R_nc_utterm)
}
