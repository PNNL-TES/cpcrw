# R script to
#  - read in tree survey and display results
# This was a quick script made during the survey to check out progress

# Ben Bond-Lamberty July 2014

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME      <- "1-tree_survey.R"

TREE_SURVEY     <- "tree_survey.csv"

# -----------------------------------------------------------------------------
# Packages and reproducibility

library(checkpoint)  # version 0.3.8
checkpoint("2015-02-27")
library(ggplot2)
theme_set(theme_bw())

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)


d <- read_csv(TREE_SURVEY)
d <- d[d$Status=="Alive",]
d$Transect <- factor(d$Transect, levels=c("T5","T6","T7","T8","T9","T10"))

library(plyr)
library(reshape)

d$BA <- (d$DBH_cm / 2 / 100) ^ 2 * pi # basal area in m2

d1 <- ddply(d, .(Transect, Position_m), summarise, BA=sum(BA), Density=length(DBH_cm))

d_bs <- ddply(subset(d,Species=="PIMA"), .(Transect, Position_m), summarise, BS_BA=sum(BA))

d1 <- merge(d1, d_bs, all=T)
d1[is.na(d1$BS_BA), "BS_BA"] <- 0.0
d1$BS_BA <- d1$BS_BA / d1$BA

plotsize <- 5 ^ 2 * pi / 2   # plotsize, semicircle w/ radius of 5 m
d1$BA <- d1$BA / plotsize * 100 * 100 # convert to /ha
d1$Density <- d1$Density / plotsize * 100 * 100 # convert to /ha

names(d1) <- c("Transect","Position_m","Basal area (m2/ha)","Density (/ha)","Black spruce fraction BA")

d2 <- melt(d1, id.vars=c(1:2))

p <- qplot(Position_m, value, data=d2, color=Transect)+geom_smooth(method='lm', fill=NA) +
    facet_grid(variable~., scales="free")
p <- p + geom_smooth(group=1, color='darkgrey', method='lm')
print(p)
save_plot("vegsurvey1")

#tpos <- data.frame(Transect=levels(d2$Transect),TPosition=c(0,8,28,40,48,68))
#d2 <- merge(d, tpos)

#d3 <- cast(d2, Transect+Position+TPosition~variable)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()

