# R script to
#  - read in merged tree ring/plot/core dataset
#  - compute chronology, errors, make figures

# TODO: crossdating

# Ben Bond-Lamberty January 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "5-chronology.R"

COREDATA     <- "outputs/coredata_merged.csv"

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(dplyr)  # 0.4
library(magrittr)
library(reshape2)
library(dplR)  # 1.6.2

coredata <- read_csv(COREDATA)
printdims(coredata)

# reshape data, as dplR functions expect series as columns, years in rows
rwl <- dcast(coredata, Year~Core, value.var="Width_mm")
printdims(rwl)

printlog("Detrending...")
rwl_detrended <- detrend( rwl, method="Spline" )

# Perform a simple random sub-sampling to assess error
printlog("Running random sub-sampling...")
n <- 10
prop <- 1/n  # 50%
results <- data.frame()
for(i in 1:n) {
    samples <- sample(2:ncol(rwl_detrended), size=(ncol(rwl_detrended)-1) * prop)
    rwl2 <- rwl_detrended[c(1, samples)]
    rwl2_chron <- chron(rwl2)
    rwl2_chron$Year <- rwl$Year
    results <- rbind(results, rwl2_chron)
}
variability <- group_by(results, Year) %>% 
    summarise(max=max(xxxstd), min=min(xxxstd), sd=sd(xxxstd))


printlog("Computing main chronology...")
rwl_chron <- chron(rwl_detrended)
rwl_chron$Year <- rwl$Year

printlog("Merging chronology with variability from r.s.s...")
chronology <- merge(rwl_chron, variability)
savedata(chronology)

printlog("Plotting...")
p <- ggplot(chronology, aes(Year, xxxstd)) + geom_line()
p <- p + geom_point(aes(size=samp.depth))
p <- p + geom_ribbon(aes(ymin=min, ymax=max), alpha=.2) 
p <- p + geom_ribbon(aes(ymin=xxxstd-sd, ymax=xxxstd+sd), alpha=0.5)
p <- p + ylab("Detrended ring-width increment (mm)")
p <- p + scale_size_continuous("Cores")
p <- p + coord_cartesian(xlim=c(1935, 2015), ylim=c(0.7, 1.3))
print(p)
saveplot("chronology")

print(p + coord_cartesian(xlim=c(2000, 2013.5), ylim=c(0.85, 1.15)))
saveplot("chronology-recent")

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
