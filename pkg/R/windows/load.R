".onLoad" <- function(lib, pkg) {
    if(as.numeric(R.Version()$minor) > 2.4)
        Sys.setenv("UDUNITS_PATH"=system.file("udunits.dat", package=pkg))
    utinit.nc(system.file("udunits.dat", package=pkg))
}

