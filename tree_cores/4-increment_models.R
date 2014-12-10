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

printlog("Merging...")
d <- merge(coredata, ringwidths, by="Core")

printlog("Excluding data before", MINYEAR)
d <- d[d$Year>MINYEAR,]

trees <- d %>% group_by(Transect, Position) %>% summarise(n=length(unique(Core)))
trees$Transect <- factor(trees$Transect, levels=c("T5", "T6", "T7", "T8", "T9", "T10"))

p1 <- qplot(Transect, Position, label=n, geom="text", data=trees)
p1 <- p1 + ggtitle("Number of trees processed")
print(p1)
saveplot("4-treecounts")

# Fit linear models relating ring width (mm) to DBH (cm) by species,
# for each position on each transect

# TODO:
# If an error occurs (model can't be fit) skip
# Fit for transect; currently fitting only for each position
by_d <- d %>% 
    group_by(Position, Species) 

models <- by_d %>% do(
    mod = lm(Width_mm ~ DBH_cm, data = .),
    n = group_size(.)
)

increment_models <- summarise(models, 
                           n=n,
                           Position=Position,
                           Species=Species,
                           R2=summary(mod)$adj.r.squared,
                           m=coef(mod)[1],
                           b=coef(mod)[2]
)

savedata(increment_models)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
