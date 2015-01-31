# Assume that RNetCDF is linked with udunits2 in Makevars.win.

".onLoad" <- function(lib, pkg) {

    # Initialise udunits2 library and allow it to find a units database in
    # environment variable UDUNITS2_XML_PATH or a built-in default path:
    result <- try(utinit.nc(""), silent=TRUE)

    # If library failed to initialise:
    if (inherits(result, "try-error")) {

        # Stop if environment variable UDUNITS2_XML_PATH was set:
        if (nchar(Sys.getenv("UDUNITS2_XML_PATH")) > 0) {
	    packageStartupMessage(
"RNetCDF: udunits2 failed to initialise with units database
           in file specified by environment variable UDUNITS2_XML_PATH")
            return()
        }

        # Try the units database that ships with RNetCDF.
        # Set UDUNITS2_XML_PATH so that nested database files can be found.
	result <- try({
            datafile <- system.file("udunits", "udunits2.xml", package=pkg, lib.loc=lib)
            Sys.setenv(UDUNITS2_XML_PATH=datafile)
            utinit.nc("")
            }, silent=TRUE)

        if (inherits(result, "try-error")) {
	    packageStartupMessage(
"RNetCDF: udunits2 failed to initialise with in-built units database.
           Please specify a file in environment variable UDUNITS2_XML_PATH.")
        } else {
            # Return silently on success, because user probably doesn't care that
            # the internal database was used unless they set UDUNITS2_XML_PATH.
        }
    }
}

