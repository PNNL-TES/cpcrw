# R script to
#  - read in tree survey and display results
# This was a quick script made during the survey to check out progress

# Ben Bond-Lamberty July 2014

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "1-sla.R"

SLA_DATA          <- "26Aug2014_SLA.csv"

TREE_SURVEY         <- "tree_survey.csv"
ALD_DATA          <- "../ald/cpcrw_ald.csv"
LAI_DATA     <- "../tree_survey/outputs/npp.csv"

library(dplyr)

# ==============================================================================
# Main

#sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

# Load SLA, ALD, and LAI datasets
sla <- read_csv(SLA_DATA)
sla$Transect <- "T5"
printdims(sla)
ald <- read_csv(ALD_DATA)
printdims(ald)
lai <- read_csv(LAI_DATA)
printdims(lai)

# Deal with ">150" values in ALD data
printlog("Assuming ALD values with '>' are 10% greater than max...")
greaterthans <- grepl(">", ald$Depth_cm)
ald$Depth_cm[greaterthans] <- as.numeric(gsub(">","",ald$Depth_cm[greaterthans])) *1.1
ald$Depth_cm <- as.numeric(ald$Depth_cm)

# Compute SLA, using projected leaf area (PLA) to hemisurface leaf
# area (HSLA) conversion based on Bond-Lamberty et al. (2003) values
printlog("Calculating SLA...")
pla_to_hsla <- data.frame(Species=c("ALSP", "BEPA", "PIMA"),
                          p2h=c(1.0, 1.0, 1.55))
sla <- merge(sla, pla_to_hsla)
sla$SLA <- with(sla, p2h * SurfaceArea_cm2 / WeightDry_g)

printlog("Plotting transect position versus SLA...")
p <- ggplot(sla, aes(factor(Position_m), SLA, color=Species)) + geom_violin()
p <- p + xlab("Transect position (m)")
p <- p + ylab(expression(SLA~(cm^2~g^{-1}))) 
print(p)
saveplot("sla")

# To do: expolate / summarize ALD and LAI data as necessary

printlog("Merging SLA data with LAI and ALD...")
sla_merged <- sla %>% merge(ald) %>% merge(lai)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()

