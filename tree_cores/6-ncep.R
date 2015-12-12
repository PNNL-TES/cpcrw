# R script to get NCEP reanalysis data for CPCRW site
# It just pulls from netcdf files downloaded December 9, 2015 from
# http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.surface.html
# Ben Bond-Lamberty December 2015

# TODO: the month calculation in get_ncep_data isn't working correctly!
# it's spitting out 1-12 for 1948, and then 0-11?


# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "6-ncep.R"

library(dplyr)
library(ncdf4)
library(ggplot2)
theme_set(theme_bw())

# -----------------------------------------------------------------------------
get_ncep_data <- function(ncvar, ncfile, lon, lat) {
    printlog("Opening", ncfile)
    nc <- nc_open(ncfile)
    print(summary(nc))
    whichlat <- which.min(abs(ncvar_get(nc, "lat") - lat))
    whichlon <- which.min(abs(ncvar_get(nc, "lon") - lon))
    
    printlog("Retrieving", ncvar)
    x <- ncvar_get(nc, ncvar, start=c(whichlon, whichlat, 1), count = c(1, 1, -1))
    d <- data.frame(X = x,
                    year = 1948 + floor(seq_along(x - 1) / 12),
                    month = ((seq_along(x) - 1) %% 12) + 1)
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

printlog("Merging air and precip data...")
ncepdata <- merge(airdata, precipdata)
print(summary(ncepdata))

printlog("Fitting segmented models...")
library(segmented)
m1 <- lm(air ~ year, data = annual)
m1.seg <- segmented(m1, seg.Z = ~year)
brk1 <- summary(m1.seg)$psi[2]
m2 <- lm(pr_wtr ~ year, data = annual)
m2.seg <- segmented(m2, seg.Z = ~year)
brk2 <- summary(m2.seg)$psi[2]

printlog("Plotting...")
ncepdata %>% 
    group_by(year) %>% 
    summarise(air = mean(air), pr_wtr = sum(pr_wtr)) -> 
    annual
p1 <- qplot(year, air, data = annual) #+ geom_smooth()
p1 <- p1 + xlab("Year") + ylab("Air temperature (°C)")
p1 <- p1 + geom_smooth(method = 'lm', data = subset(annual, year <= brk1))
p1 <- p1 + geom_smooth(method = 'lm', data = subset(annual, year >= brk1))
print(p1)
save_plot("air")

p2 <- qplot(year, pr_wtr, data = annual) #+ geom_smooth()
p2 <- p2 + xlab("Year") + ylab("Precipition (cm)")
p2 <- p2 + geom_smooth(method = 'lm', data = subset(annual, year <= brk2))
p2 <- p2 + geom_smooth(method = 'lm', data = subset(annual, year >= brk2))
print(p2)
save_plot("pr_wtr")

printlog("All done with", SCRIPTNAME)

save_data(ncepdata, scriptfolder = FALSE)

print(sessionInfo())
sink()
