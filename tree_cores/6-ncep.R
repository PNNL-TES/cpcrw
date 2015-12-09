# R script to get NCEP reanalysis data for CPCRW site
# It just pulls from netcdf files downloaded December 9, 2015 from
# http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.surface.html
# Ben Bond-Lamberty December 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "6-ncep.R"

library(ncdf4)


# -----------------------------------------------------------------------------
get_ncep_data <- function(ncvar, ncfile, lon, lat) {
    printlog("Opening", ncfile)
    nc <- nc_open(ncfile)
    whichlat <- which.min(abs(ncvar_get(nc, "lat") - lat))
    whichlon <- which.min(abs(ncvar_get(nc, "lon") - lon))
    
    printlog("Retrieving", ncvar)
    x <- ncvar_get(nc, ncvar, start=c(whichlon, whichlat, 1), count = c(1, 1, -1))
    d <- data.frame(X = x,
                    year = 1948 + floor(seq_along(x) / 12),
                    month = seq_along(x) %% 12)
    names(d)[1] <- ncvar
    
    printlog("Closing", ncfile)
    nc_close(nc)
    d
}


# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep = "/"), split = TRUE)

printlog("Welcome to", SCRIPTNAME)

# CPCRW location
lon <- 180 - 147.5
lat <- 65.167

airdata <- get_ncep_data("air", "~/Data/NCEP/air.mon.mean.nc", lon, lat)
precipdata <- get_ncep_data("pr_wtr", "~/Data/NCEP/pr_wtr.mon.mean.nc", lon, lat)
ncepdata <- merge(airdata, precipdata)

printlog("Merging air and precip data...")
printlog("All done with", SCRIPTNAME)

save_data(ncepdata, scriptfolder = FALSE)

print(sessionInfo())
sink()
