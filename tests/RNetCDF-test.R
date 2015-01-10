#===============================================================================#
#                                                                               #
#  Name:       RNetCDF-test.R                                                   #
#                                                                               #
#  Version:    1.6.3-1                                                          #
#                                                                               #
#  Purpose:    Test functions to the NetCDF interface for R.                    #
#                                                                               #
#  Author:     Pavel Michna (michna@giub.unibe.ch)                              #
#              Milton Woods (m.woods@bom.gov.au)                                #
#                                                                               #
#  Copyright:  (C) 2010-2014 Pavel Michna                                       #
#                                                                               #
#===============================================================================#
#                                                                               #
#  This program is free software; you can redistribute it and/or modify         #
#  it under the terms of the GNU General Public License as published by         #
#  the Free Software Foundation; either version 2 of the License, or            #
#  (at your option) any later version.                                          #
#                                                                               #
#  This program is distributed in the hope that it will be useful,              #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of               #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                #
#  GNU General Public License for more details.                                 #
#                                                                               #
#  You should have received a copy of the GNU General Public License            #
#  along with this program; if not, write to the Free Software                  #
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA    #
#                                                                               #
#===============================================================================#
#  Implementation and Revisions                                                 #
#-------------------------------------------------------------------------------#
#  Author   Date       Description                                              #
#  ------   ----       -----------                                              #
#  pm       29/12/10   First implementation                                     #
#  mw       18/07/12   Test packed variables                                    #
#                                                                               #
#===============================================================================#


#===============================================================================#
#  Load library                                                                 #
#===============================================================================#

library(RNetCDF)


#===============================================================================#
#  Run tests                                                                    #
#===============================================================================#

#-------------------------------------------------------------------------------#
#  NetCDF library functions                                                     #
#-------------------------------------------------------------------------------#

#--Initialize ------------------------------------------------------------------#
cat("Starting NetCDF tests...\n")
nccount <- 0

##  Create a new NetCDF dataset and define two dimensions
nc <- create.nc("foo.nc")

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)
dim.def.nc(nc, "max_string_length", 32)

##  Create three variables, one as coordinate variable
var.def.nc(nc, "time", "NC_INT", "time")
var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))
var.def.nc(nc, "packvar", "NC_BYTE", c("station"))
var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))

##  Put some missing_value attribute for temperature
att.put.nc(nc, "temperature", "missing_value", "NC_DOUBLE", -99999.9)

## Define the packing used by packvar
att.put.nc(nc, "packvar", "scale_factor", "NC_DOUBLE", 10)
att.put.nc(nc, "packvar", "add_offset", "NC_DOUBLE", -5)

##  Define variable values
mytime        <- c(1:2)
mytemperature <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9)
mypackvar     <- seq_len(5)*10-5
myname        <- c("alfa", "bravo", "charlie", "delta", "echo")

##  Put the data
var.put.nc(nc, "time", mytime, 1, length(mytime))
var.put.nc(nc, "temperature", mytemperature, c(1,1), c(5,2))
var.put.nc(nc, "packvar", mypackvar, pack=TRUE)
var.put.nc(nc, "name", myname, c(1,1), c(32,5))

sync.nc(nc)

#-- Test 1 (read) --------------------------------------------------------------#
cat("Test 1 ... ")

x <- c(1, 2)
y <- var.get.nc(nc, 0)

if(sum(y %in% x) == 2) {
    cat("OK\n")
    nccount <- nccount + 1
} else
    cat("failed\n")

#-- Test 2 (read) --------------------------------------------------------------#
cat("Test 2 ... ")

x <- matrix(data=c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9), ncol=2)
y <- var.get.nc(nc, "temperature")

if(sum(y %in% x) == 10) {
    cat("OK\n")
    nccount <- nccount + 1
} else
    cat("failed\n")

#-- Test 3 (read) --------------------------------------------------------------#
cat("Test 3 ... ")

x <- c(6.6, 7.7, NA, NA, 9.9)
y <- var.get.nc(nc, "temperature", c(NA,2), c(NA,1))

if(sum(y %in% x) == 5) {
    cat("OK\n")
    nccount <- nccount + 1
} else
    cat("failed\n")

#-- Test 4 (read) --------------------------------------------------------------#
cat("Test 4 ... ")

x <- c("alfa", "bravo", "charlie", "delta", "echo" )
y <- var.get.nc(nc, "name")

if(sum(y %in% x) == 5) {
    cat("OK\n")
    nccount <- nccount + 1
} else
    cat("failed\n")

#-- Test 5 (read) --------------------------------------------------------------#
cat("Test 5 ... ")

x <- c("brav", "char")
y <- var.get.nc(nc, "name", c(1,2), c(4,2))

if(sum(y %in% x) == 2) {
    cat("OK\n")
    nccount <- nccount + 1
} else
    cat("failed\n")

#-- Test 6 (read) --------------------------------------------------------------#
cat("Test 6 ... ")

x <- c("bravo", "charlie")
y <- var.get.nc(nc, "name", c(1,2), c(NA,2))

if(sum(y %in% x) == 2) {
    cat("OK\n")
    nccount <- nccount + 1
} else
    cat("failed\n")

#-- Test 7 (read unpacked) -----------------------------------------------------#
cat("Test 7 ... ")

y <- var.get.nc(nc, "packvar", unpack=TRUE)

if(isTRUE(all.equal(mypackvar, as.vector(y)))) {
    cat("OK\n")
    nccount <- nccount + 1
} else
    cat("failed\n")

#-- Close file -----------------------------------------------------------------#
close.nc(nc)


#-------------------------------------------------------------------------------#
#  UDUNITS calendar functions                                                   #
#-------------------------------------------------------------------------------#

#--Initialize ------------------------------------------------------------------#
cat("Starting UDUNITS tests...\n")
utcount <- 0

#-- Test 1 (utcal.nc() - numeric values) ---------------------------------------#
cat("Test 1 ... ")

x <- matrix(data=c(1899, 1900, 1900, 1900, 1900, 1900,
                     12,    1,    1,    1,    1,    1,
		     31,    1,    1,    1,    1,    1,
		     23,    0,    1,    2,    3,    4,
		      0,    0,    0,    0,    0,    0,
		      0,    0,    0,    0,    0,    0),
	    ncol=6)
y <- utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5))

if(sum(y == x) == 36) {
    cat("OK\n")
    utcount <- utcount + 1
} else
    cat("failed\n")

#-- Test 2 (utcal.nc() - string values) ----------------------------------------#
cat("Test 2 ... ")

x <- c("1899-12-31 23:00:00", "1900-01-01 00:00:00", "1900-01-01 01:00:00",
       "1900-01-01 02:00:00", "1900-01-01 03:00:00", "1900-01-01 04:00:00")
y <- utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5), type="s")

if(sum(y == x) == 6) {
    cat("OK\n")
    utcount <- utcount + 1
} else
    cat("failed\n")

#-- Test 3 (utinvcal.nc() - numeric values) ------------------------------------#
cat("Test 3 ... ")

x <- 6.416667
y <- utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(1900,1,1,5,25,0))

if(round(x, 6) == round(y, 6)) {
    cat("OK\n")
    utcount <- utcount + 1
} else
    cat("failed\n")

#-- Test 4 (utinvcal.nc() - string values) -------------------------------------#
cat("Test 4 ... ")

x <- 6.416667
y <- utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", "1900-01-01 05:25:00")

if(round(x, 6) == round(y, 6)) {
    cat("OK\n")
    utcount <- utcount + 1
} else
    cat("failed\n")


#-------------------------------------------------------------------------------#
#  Overall summary                                                              #
#-------------------------------------------------------------------------------#
cat("Totally ", nccount+utcount, "/ 11 tests passed. ")

if(nccount != 7)
    stop("Some NetCDF tests failed.", call.=FALSE)

if(utcount != 4)
    stop("Some UDUNITS tests failed.", call.=FALSE)

cat("Package seems to work properly.\n")


#===============================================================================#

#===============================================================================#
#  SCRATCH									#
#===============================================================================#

