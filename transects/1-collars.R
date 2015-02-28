# R script to
#  - read in and plot collar positions

# Ben Bond-Lamberty January 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "1-collars.R"

COLLAR_DATA       <- "CPCRW_2014_collar_positions.csv"
TRANSECT_DATA       <- "CPCRW_2014_Transect_Positions.csv"

# -----------------------------------------------------------------------------
# Packages and reproducibility

library(checkpoint)  # version 0.3.8
checkpoint("2015-02-27")
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

cdata <- read_csv(COLLAR_DATA)
printdims(cdata)
tdata <- read_csv(TRANSECT_DATA)
printdims(tdata)
names(tdata)[2] <- "Position_SN"
tdata$Transect <- paste0("T", tdata$Transect)

printlog("Creating plotting data...")
collarmap <- expand.grid(Transect=tdata$Transect, Collar=unique(cdata$Collar))
collarmap <- merge(merge(collarmap, cdata), tdata)
collarmap$MainCollar <- collarmap$Position_m %in% c(0, 4, 12, 20, 24, 32, 40, 44, 52, 60, 64, 72)

save_data(collarmap, scriptfolder=FALSE)



p <- ggplot(collarmap, aes(Position_m, Position_SN, size=MainCollar)) + geom_point()
p <- p + ylab("Meters (south to north)") + xlab("Meters (west to east)")
p <- p + scale_size_discrete(guide=FALSE)
t1 <- 20
t2 <- 30
p <- p + annotate(geom="rect", ymin=-10, ymax=85, xmin=-10, xmax=t1, alpha=0.1)
p <- p + annotate(geom="rect", ymin=-10, ymax=85, xmin=t1, xmax=t2, alpha=0.2)
p <- p + annotate(geom="rect", ymin=-10, ymax=85, xmin=t2, xmax=85, alpha=0.3)
p <- p + coord_equal(xlim=c(-5, 75), ylim=c(-5, 75))
print(p)
save_plot("collarmap", scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
