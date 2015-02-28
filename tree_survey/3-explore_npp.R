# R script to
#  - read in ring increment data
#  - merge with plot/core data
#  - compute size versus RWI by plot and species
#  - save

# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "3-explore_npp.R"

NPP_DATA            <- paste(OUTPUT_DIR, "npp.csv", sep="/")
ALD_DATA            <- "../ald/cpcrw_ald.csv"

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

npp <- read_csv(NPP_DATA)
ald_original <- read_csv(ALD_DATA)
ald_original[ald_original$Depth_cm==">150", "Depth_cm"] <- "151"
ald_original$Depth_cm <- as.numeric(ald_original$Depth)
ald_original$Transect <- factor(ald_original$Transect, levels=c("T5", "T6", "T7", "T8", "T9", "T10"))

ald <- ald_original %>%
    group_by(Transect, Position_m) %>%
    summarise(Depth_cm=mean(Depth_cm))
save_data(ald)

printlog("Plotting ALD...")
p1 <- ggplot(ald, aes(Transect, Position_m)) + geom_tile(aes(fill=Depth_cm))
p1 <- p1 + ylab("Transect position (m)") + xlab("Transect")
print(p1)
save_plot("ald1")

printlog("Summarizing ALD by transect position...")
ald2 <- na.omit(ald_original) %>%
    group_by(Position_m) %>%
    summarise(mean=mean(Depth_cm), min=min(Depth_cm), max=max(Depth_cm), sd=sd(Depth_cm))
save_data(ald2)

p1a <- ggplot(ald2, aes(Position_m, mean)) + geom_line()
p1a <- p1a + geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2)
p1a <- p1a + geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd), alpha=0.5)
p1a <- p1a + xlab("Transect position (m)") + ylab("Maximum ALD (cm)")
p1a <- p1a + geom_hline(yintercept=150, linetype=2)
print(p1a)
save_plot("ald2")

npp_summary <- na.omit(npp) %>%
    group_by(Position_m) %>%
    summarise(mean=mean(npp_gC), sd=sd(npp_gC))


printlog("Merging ALD and NPP data...")
d <- merge(npp, ald)

p2 <- ggplot(d, aes(Depth_cm, npp_gC)) + geom_point() + geom_smooth()
p2 <- p2 + xlab("ALD (cm)") + ylab("NPP (gC/m2/yr)")
print(p2)
save_plot("ald_vs_npp")

printlog("Model for ALD versus NPP...")
m1 <- lm(npp_gC ~ Depth_cm, data=d)
print(summary(m1))
pdf(paste(outputdir(), "m1-ald_vs_npp.pdf", sep="/"))
par(mfrow=c(2,2))
plot(m1)
dev.off()


printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
