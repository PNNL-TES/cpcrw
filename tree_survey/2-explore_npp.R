# R script to
#  - read in ring increment data
#  - merge with plot/core data
#  - compute size versus RWI by plot and species
#  - save

# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "2-explore_npp.R"

NPP_DATA            <- paste(OUTPUT_DIR, "npp.csv", sep="/")
ALD_DATA            <- "../ald/cpcrw_ald.csv"

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(magrittr)

npp <- read_csv(NPP_DATA)
ald <- read_csv(ALD_DATA)
ald[ald$Depth_cm==">150", "Depth_cm"] <- "151"
ald$Depth_cm <- as.numeric(ald$Depth)
ald <- ald %>%
    group_by(Transect, Position_m) %>%
    summarise(Depth_cm=mean(Depth_cm))

ald$Transect <- factor(ald$Transect, levels=c("T5", "T6", "T7", "T8", "T9", "T10"))

printlog("Plotting ALD...")
p1 <- ggplot(ald, aes(Transect, Position_m)) + geom_tile(aes(fill=Depth_cm))
p1 <- p1 + ylab("Transect Position_m (m)") + xlab("Transect")
print(p1)
saveplot("2-ald")

printlog("Merging ALD and NPP data...")
d <- merge(npp, ald)

p2 <- ggplot(d, aes(Depth_cm, npp_gC)) + geom_point() + geom_smooth(method='lm')
p2 <- p2 + xlab("ALD (cm)") + ylab("NPP (gC/m2/yr")
print(p2)
saveplot("2-ald_vs_npp")

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
