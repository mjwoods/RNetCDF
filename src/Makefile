## Makefile for RNetCDF.
# Although Makevars is the recommended way to compile R packages,
# it forces use of the C compiler (CC) defined during R installation.
# This Makefile allows users to specify an MPI compiler wrapper which:
# - sets include and library paths;
# - links MPI libraries, whose names may vary with compiler flags and vendors;
# - is the most common and reliable method to build software with MPI.
# Updates to the Makefile may be required if the R build system changes.

R_SHLIB_MK = $(R_HOME)/share/make/shlib.mk

all:
	$(MAKE) -f "$(R_HOME)/etc/$(R_ARCH)/Makeconf" \
          -f Makefile.common \
          -f "$(R_SHLIB_MK)" \
          RNetCDF_all

