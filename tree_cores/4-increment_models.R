# R script to
#  - read in ring increment data
#  - merge with plot/core data
#  - compute size versus RWI by plot and species
#  - save

# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("0-functions.R")

SCRIPTNAME        <- "4-increment_models.R"

RINGWIDTHDATA     <- "outputs/ringwidths.csv"
COREDATA          <- "tree_cores.csv"
MINYEAR           <- 2008

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(plyr)
library(magrittr)

ringwidths <- read_csv(RINGWIDTHDATA)
coredata <- read_csv(COREDATA)

printlog("Merging...")
d <- merge(coredata, ringwidths, by="Core")
d <- d[d$Year>MINYEAR,]
counts <- ddply(d, .(Transect, Position), summarise, ringcount=length(Year))
counts$Transect <- factor(counts$Transect, levels=c("T5", "T6", "T7", "T8", "T9", "T10"))


p1 <- qplot(Transect,Position,label=ringcount, geom="text", data=counts)
p1 <- p1 + ggtitle(paste("Ring counts. Min year =", MINYEAR))
print(p1)
saveplot("4-ringcounts")

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
