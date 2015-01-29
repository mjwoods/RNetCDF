# Assume that RNetCDF is linked with udunits2 in Makevars.win.

".onLoad" <- function(lib, pkg) {

    # Initialise udunits library and allow it to find a units database
    # in the UDUNITS2_XML_PATH environment variable or a built-in default path:
    result <- try(utinit.nc(""), silent=TRUE)

    # If library failed to initialise,
    # try to use the units database shipped with RNetCDF.
    if (inherits(result, "try-error")) {
        datafile <- system.file("udunits", "udunits2.xml", 
           package=pkg, lib.loc=lib, mustWork=TRUE)

        # Set the environment so that nested database files can be found:
	Sys.setenv(UDUNITS2_XML_PATH=datafile)

        result <- try(utinit.nc(""), silent=TRUE)

        if (inherits(result, "try-error")) {
	    packageStartupMessage("RNetCDF: udunits2 library failed to initialise")
        } else {
	    packageStartupMessage("RNetCDF: udunits2.xml loaded from RNetCDF package")
        }
    }
}
