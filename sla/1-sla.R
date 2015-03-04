# R script to
# - read and calculate SLA data
# - read active layer depth (ALD) and leaf area index (LAI)
#     these have already been computed by other scripts
# - summarize, make plots, run regressions

# Ben Bond-Lamberty February 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME  <- "1-sla.R"
RAW_DATA    <- "26Aug2014_SLA.csv"
ALD_DATA    <- "../ald/cpcrw_ald.csv"
LAI_DATA    <- "../tree_survey/outputs/npp.csv"

# -----------------------------------------------------------------------------
# Packages and reproducibility
library(checkpoint)  # version 0.3.8
try(checkpoint("2015-02-27"))
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# -----------------------------------------------------------------------------
# Compute specific leaf area (SLA)
compute_sla <- function(sla) {
    printlog("Calculating SLA...")
    # Projected leaf area (PLA) to hemisurface leaf area (HSLA) 
    # conversion based on Bond-Lamberty et al. (2003) values
    pla_to_hsla <- data.frame(Species=c("ALSP", "BEPA", "PIMA"),
                              p2h=c(1.0, 1.0, 1.55))
    sla <- merge(sla, pla_to_hsla)
    sla$SLA <- with(sla, p2h * SurfaceArea_cm2 / WeightDry_g)
    sla
} # compute_sla

# -----------------------------------------------------------------------------
# Clean active layer depth (ALD) data
clean_ald <- function(ald) {
    # Deal with ">150" values in ALD data
    printlog("Assuming ALD values with '>' are 10% greater than max...")
    greaterthans <- grepl(">", ald$Depth_cm)
    ald$Depth_cm[greaterthans] <- as.numeric(gsub(">","",ald$Depth_cm[greaterthans])) *1.1
    ald$Depth_cm <- as.numeric(ald$Depth_cm)
    ald
} # clean_ald

# -----------------------------------------------------------------------------
# Clean leaf area index (LAI) data
clean_lai <- function(lai) {
    lai
} # clean_lai

# -----------------------------------------------------------------------------
# Summarize SLA, save data, make basic plot
summarize_sla <- function(sla) {
    printlog("Summarizing SLA...")
    sla_summary <- sla %>% group_by(Species) %>% 
        summarise(min=min(SLA), max=mean(SLA), 
                  mean=mean(SLA), median=median(SLA), sd=sd(SLA),
                  n=n())
    sla_summary[-1] <- round(sla_summary[-1], 1)
    print(sla_summary)
    save_data(sla_summary)

    pima_summary <- sla %>% group_by(Species, Position_m) %>%
        filter(Species=="PIMA") %>%
        summarise(min=min(SLA), max=mean(SLA), 
                  mean=mean(SLA), median=median(SLA), sd=sd(SLA),
                  n=n())
    pima_summary[-1] <- round(pima_summary[-1], 1)
    print(pima_summary)
    save_data(pima_summary)
    
    printlog("Plotting transect position versus SLA...")
    p <- ggplot(sla, aes(factor(Position_m), SLA, color=Species)) 
    p <- p + xlab("Transect position (m)")
    p <- p + ylab(expression(SLA~(cm^2~g^{-1}))) 
    print(p + geom_violin())
    save_plot("sla1")
    
    p <- p + geom_point(position="jitter") 
    p <- p + geom_smooth(aes(group=Species), method='lm')
    print(p)
    save_plot("sla2")
} # summarize_sla


# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

# Load and clean SLA, ALD, and LAI datasets
sla <- read_csv(RAW_DATA)
ald <- read_csv(ALD_DATA)
lai <- read_csv(LAI_DATA)

sla <- compute_sla(sla)
ald <- clean_ald(ald)
lai <- clean_lai(lai)

# Basic data summary
summarize_sla(sla)

# Data merge
printlog("Merging SLA data with LAI and ALD...")
sla_merged <- sla %>% merge(ald) %>% merge(lai)

save_data(sla_merged, scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
