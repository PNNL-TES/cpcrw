# R script to
#  - read in ring increment data
#  - merge with plot/core data
#  - compute size versus RWI by plot and species
#  - save

# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("../0-functions.R")

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
library(dplyr)
library(magrittr)

ringwidths <- read_csv(RINGWIDTHDATA)
coredata <- read_csv(COREDATA)

# Diagnostics on core IDs before merging
ringwidths$Core <- tolower(ringwidths$Core)
coredata$Core <- tolower(coredata$Core)
printlog("Unique cores in ring width data =", length(unique(ringwidths$Core)))
printlog("Unique cores in coring data =", length(unique(coredata$Core)))
printlog("Cores in ring width data but not in coring data:")
print(setdiff(ringwidths$Core, coredata$Core))
printlog("Cores in coring data but not in ring width data:")
print(setdiff(coredata$Core, ringwidths$Core))

printlog("Merging...")
d <- merge(coredata, ringwidths, by="Core")

printlog("Diagnostic plot...")
trees <- d %>% 
    group_by(Transect, Position_m) %>% 
    summarise(n=length(unique(Core)), ringcount=n())
trees$Transect <- factor(trees$Transect, levels=c("T5", "T6", "T7", "T8", "T9", "T10"))

p1 <- qplot(Transect, Position_m, label=paste0(n, "/", ringcount), geom="text", data=trees)
p1 <- p1 + ggtitle("Number of trees/rings processed")
print(p1)
saveplot("4-treecounts")


# Fit linear models relating ring width (mm) to DBH (cm) by species,
# for each Position_m on each transect

# TODO:
# If an error occurs (model can't be fit) skip
# Fit for transect; currently fitting only for each Position_m
printlog("Excluding data before", MINYEAR)
d <- d[d$Year>MINYEAR,]

by_d <- d %>% 
    group_by(Position_m, Species) 

models <- by_d %>% do(
    mod = lm(Width_mm ~ DBH_cm, data = .),
    n = group_size(.)
)

increment_models <- summarise(models, 
                           n=n,
                           Position_m=Position_m,
                           Species=Species,
                           R2=summary(mod)$adj.r.squared,
                           m=coef(mod)[1],
                           b=coef(mod)[2]
)

savedata(increment_models, scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
