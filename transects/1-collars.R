# R script to
#  - read in and plot collar positions

# Ben Bond-Lamberty January 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "1-collars.R"

COLLAR_DATA       <- "CPCRW_2014_collar_positions.csv"
TRANSECT_DATA       <- "CPCRW_2014_Transect_Positions.csv"

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(dplyr) # 0.4
library(magrittr)

cdata <- read_csv(COLLAR_DATA)
printdims(cdata)
tdata <- read_csv(TRANSECT_DATA)
printdims(tdata)
names(tdata)[2] <- "Position_SN"
tdata$Transect <- paste0("T", tdata$Transect)

printlog("Creating plotting data...")
collarmap <- expand.grid(Transect=tdata$Transect, Collar=unique(cdata$Collar))
collarmap <- merge(merge(collarmap, cdata), tdata)
savedata(collarmap, scriptfolder=FALSE)

p <- ggplot(collarmap, aes(Position_SN, Position_m)) + geom_point()
p <- p + xlab("Meters (south to north)") + ylab("Meters (east to west)")
print(p)
saveplot("collarmap", scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
