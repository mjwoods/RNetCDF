".onLoad" <- function(lib, pkg) {
    # Initialise udunits library and allow it to find a units database.
    # First, environment variables are checked:
    #   udunits2 uses UDUNITS2_XML_PATH,
    #   udunits uses UDUNITS_PATH.
    # Then the library will try a default path set at compile time.
#    result <- try(utinit.nc(""), silent=TRUE)

#    if (inherits(result,"try-error")) {
        # Try udunits2 database that ships with RNetCDF:
        units2 <- system.file("udunits", "udunits2.xml", 
                    package=pkg, lib.loc=lib, mustWork=TRUE)
        utinit.nc(units2)
#    }
}
