# R script to
#  - read in merged tree ring/plot/core dataset
#  - compute chronology, errors, make figures

# TODO: crossdating

# Ben Bond-Lamberty January 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "5-chronology.R"

COREDATA     <- "outputs/coredata_merged.csv"


build_chronology <- function(coredata) {
    printlog("coredata summary of width_mm:")
    print(summary(coredata$Width_mm))
    # reshape data, as dplR functions expect series as columns, years in rows
    rwl <- dcast(coredata, Year~Core, value.var="Width_mm")
    printdims(rwl)
    
    printlog("Detrending...")
    rwl_detrended <- detrend( rwl, method="Spline" )
    
    # Perform a simple random sub-sampling to assess error
    printlog("Running random sub-sampling...")
    n <- 10
    prop <- 1/n  # 50%
    results <- data.frame()
    for(i in 1:n) {
        samples <- sample(2:ncol(rwl_detrended), size=(ncol(rwl_detrended)-1) * prop)
        rwl2 <- rwl_detrended[c(1, samples)]
        rwl2_chron <- chron(rwl2)
        rwl2_chron$Year <- rwl$Year
        results <- rbind(results, rwl2_chron)
    }
    variability <- group_by(results, Year) %>% 
        summarise(max=max(xxxstd), min=min(xxxstd), sd=sd(xxxstd))
    
    
    printlog("Computing main chronology...")
    rwl_chron <- chron(rwl_detrended)
    rwl_chron$Year <- rwl$Year

    printlog("Mean increment =", mean(rwl_chron$xxxstd), "sd =", sd(rwl_chron$xxxstd))
    
    printlog("Merging chronology with variability from r.s.s...")
    merge(rwl_chron, variability)
}


# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(dplyr)  # 0.4
library(reshape2)
library(dplR)  # 1.6.2
library(ggplot2)
theme_set(theme_bw())

coredata <- read_csv(COREDATA)
printdims(coredata)

chronology <- build_chronology(coredata)
save_data(chronology, scriptfolder = FALSE)

printlog("Plotting...")
p <- ggplot(chronology, aes(Year, xxxstd)) + geom_line()
p <- p + geom_point(aes(size=samp.depth))
p <- p + geom_ribbon(aes(ymin=min, ymax=max), alpha=.2) 
p <- p + geom_ribbon(aes(ymin=xxxstd-sd, ymax=xxxstd+sd), alpha=0.5)
p <- p + ylab("Detrended mean index (unitless)")
p <- p + scale_size_continuous("Cores")
p <- p + coord_cartesian(xlim=c(1935, 2015), ylim=c(0.7, 1.3))
print(p)
save_plot("chronology")

print(p + coord_cartesian(xlim=c(2000, 2013.5), ylim=c(0.85, 1.15)))
save_plot("chronology-recent")


printlog("Building chronologies for different hillslope positions...")
pos <- 50
printlog("**** High cores ****")
chron_high <- build_chronology(subset(coredata, Position_m > pos))
chron_high$Position <- "High"
printlog("**** Low cores ****")
chron_low <- build_chronology(subset(coredata, Position_m <= pos))
chron_low$Position <- "Low"
chron_position <- rbind(chron_high, chron_low)
printlog("Plotting...")
p <- ggplot(chron_position, aes(Year, xxxstd, color=Position)) + geom_line() + geom_point()
p <- p + ylab("Detrended mean index (unitless)")
p <- p + coord_cartesian(xlim=c(1935, 2015), ylim=c(0.7, 1.4))
print(p)
save_plot("chronology_position")

printlog("Building chronologies for different spruce hillslope positions...")
pos <- 50
printlog("**** High cores ****")
chron_high <- build_chronology(subset(coredata, Species == "PIMA" & Position_m > pos))
chron_high$Position <- "High"
printlog("**** Low cores ****")
chron_low <- build_chronology(subset(coredata, Species == "PIMA" & Position_m <= pos))
chron_low$Position <- "Low"
chron_position_spruce <- rbind(chron_high, chron_low)
printlog("Plotting...")
p <- ggplot(chron_position_spruce, aes(Year, xxxstd, color=Position)) + geom_line() 
p <- p + geom_point()
p <- p + ylab("Detrended mean index (unitless)")
p <- p + coord_cartesian(xlim=c(1965, 2015), ylim=c(0.7, 1.2))
p <- p + annotate("text", x = 2000, y = 1.15, color = "#F8766D", fontface = "bold",
                  label = paste("High spruce s.d. = ", round(sd(chron_high$xxxstd), 2)))
p <- p + annotate("text", x = 2000, y = 0.75, color = "#00BFC4", fontface = "bold", 
                  label = paste("Low spruce s.d. = ", round(sd(chron_low$xxxstd), 2)))
print(p)
save_plot("chronology_position_spruce")
save_data(chron_low, scriptfolder = FALSE)
save_data(chron_high, scriptfolder = FALSE)

printlog("Building chronologies for different species...")

printlog("**** Birch cores ****")
chron_birch <- build_chronology(subset(coredata, Species == "BEPA"))
chron_birch$Species <- "Birch"
printlog("**** Spruce cores ****")
chron_spruce <- build_chronology(subset(coredata, Species == "PIMA"))
chron_spruce$Species <- "Spruce"
chron_species <- rbind(chron_birch, chron_spruce)

printlog("Plotting...")
p <- ggplot(chron_species, aes(Year, xxxstd, color=Species)) + geom_line() + geom_point()
#p <- p + geom_ribbon(aes(ymin=min, ymax=max), alpha=.2) 
#p <- p + geom_ribbon(aes(ymin=xxxstd-sd, ymax=xxxstd+sd), alpha=0.5)
p <- p + ylab("Detrended mean index (unitless)")
p <- p + coord_cartesian(xlim=c(1935, 2015), ylim=c(0.8, 1.2))
print(p)
save_plot("chronology_species")


printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
