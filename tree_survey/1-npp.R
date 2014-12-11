# R script to
#  - read in ring increment data
#  - merge with plot/core data
#  - compute size versus RWI by plot and species
#  - save

# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "1-npp.R"

INCREMENT_MODELS    <- "../tree_cores/outputs/increment_models.csv"
TREE_SURVEY         <- "tree_survey.csv"

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(magrittr)

increment_models <- read_csv(INCREMENT_MODELS)
tree_survey <- read_csv(TREE_SURVEY)
printdims(tree_survey)
tree_survey$Notes <- NULL
printlog("Eliminating dead trees...")
tree_survey <- subset(tree_survey, Status=="Alive")
printdims(tree_survey)

printlog("Merging tree survey data with increment models from coring...")
d <- merge(tree_survey, increment_models, by=c("Position_m", "Species"), all=TRUE)

# Testing: Bonanza Creek (Mack et al.) allometry data
# from http://www.lter.uaf.edu/data_detail.cfm?datafile_pkey=27
allometry <- read.csv("allometry/27_spruce_allometry.txt", sep=",",na.strings="-")
allometry <- allometry[c(3, 10)]
names(allometry) <- c("DBH_cm", "Biomass_g")
allometry <- na.omit(subset(allometry, DBH_cm > 0))
m <- lm(log(Biomass_g) ~ log(DBH_cm), data=allometry)

printlog("Computing increments...")
d$Increment_mm <- with(d, DBH_cm * m + b)
d$DBH_old <- with(d, DBH_cm - 2 * Increment_mm / 10.0)

printlog("Computing biomass...")
d$Biomass <- exp(log(d$DBH_cm) * coef(m)[2] + coef(m)[1])
d$Biomass_old <- exp(log(d$DBH_old) * coef(m)[2] + coef(m)[1])

printlog("Computing NPP...")
npp <- d %>%
    group_by(Transect, Position_m) %>%
    summarise(npp_gC=sum(Biomass, na.rm=T) - sum(Biomass_old, na.rm=T))
npp$Transect <- factor(npp$Transect, levels=c("T5", "T6", "T7", "T8", "T9", "T10"))

# Change to final units
plotsize <- 5 ^ 2 * pi / 2   # plotsize, semicircle w/ radius of 5 m
npp$npp_gC <- npp$npp_gC / plotsize
npp$npp_gC <- npp$npp_gC / 2.0  # to g C

print(summary(npp))
savedata(npp)

printlog("Plotting...")

p1 <- ggplot(npp, aes(Transect, Position_m)) + geom_tile(aes(fill=npp_gC))
p1 <- p1 + ylab("Transect Position_m (m)") + xlab("Transect")
print(p1)
saveplot("1-npp1")

p2 <- ggplot(npp, aes(factor(Position_m), npp_gC)) + geom_boxplot()
p2 <- p2 + xlab("Transect Position_m (m)") + ylab("NPP (gC/m2/yr)")
print(p2)
saveplot("1-npp2")


# Species-specific
printlog("Doing species-specific plots...")

npp_species <- d %>%
    group_by(Transect, Position_m, Species) %>%
    summarise(npp_gC=sum(Biomass, na.rm=T) - sum(Biomass_old, na.rm=T))

npp_species$npp_gC <- npp_species$npp_gC / plotsize
npp_species$npp_gC <- npp_species$npp_gC / 2.0  # to g C

# ...and finally take mean of transects!
npp_species <- npp_species %>%
    group_by(Position_m, Species) %>%
    summarise(npp_gC=mean(npp_gC))

p3 <- ggplot(npp_species, aes(factor(Position_m), npp_gC, fill=Species, group=Species)) + geom_bar(stat='identity')
p3 <- p3 + xlab("Transect Position_m (m)") + ylab("NPP (gC/m2/yr)")
print(p3)
saveplot("1-npp3")


printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
