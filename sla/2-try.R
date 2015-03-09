# R script to compare CPCRW SLA with data from TRY database

# Ben Bond-Lamberty March 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME  <- "2-try.R"
SLA_DATA    <- "outputs/sla_merged.csv"
TRY_DATA    <- "try/681.csv"

# -----------------------------------------------------------------------------
# Packages and reproducibility
library(checkpoint)  # version 0.3.8
try(checkpoint("2015-02-27"))
library(dplyr)
library(ggplot2)
theme_set(theme_bw())



# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

# Load and clean SLA, ALD, and LAI datasets
cpcrw <- read_csv(SLA_DATA)
try_data <- read_csv(TRY_DATA)

printlog("Subsetting and changing species for CPCRW data...")
cpcrw <- subset(cpcrw, Species %in% c("PIMA", "BEPA"))
cpcrw$SpeciesName <- "Picea mariana"
cpcrw$SpeciesName[cpcrw$Species=="BEPA"] <- "Betula papyrifera"

printlog("Plotting...")
try1 <- subset(try_data, TraitID==11)  # 11=SLA in TRY
print(summary(try1))
p <- ggplot(try1, aes(SpeciesName, StdValue)) + geom_violin(color="grey")
p <- p + geom_point(position="jitter", color="grey")
p <- p + geom_point(data=cpcrw, aes(y=SLA/10, color=Depth_cm), 
                    position="jitter")
p <- p + xlab("Species") + ylab(expression(SLA~(cm^2~g^{-1}))) 
p <- p + scale_color_continuous("ALD (cm)")
print(p)
save_plot("try_comparison1")

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
