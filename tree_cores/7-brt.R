# R script to get analyze tree ring chronology as a function of climate
# Done kind of quickly, for AGU
# Ben Bond-Lamberty December 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "7-brt.R"

library(reshape2)
library(ncdf4)
library(ggplot2)
theme_set(theme_bw())

# -----------------------------------------------------------------------------


# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep = "/"), split = TRUE)

printlog("Welcome to", SCRIPTNAME)

chron <- read_csv("outputs/chronology.csv")
chron$samp.depth <- chron$max <- chron$min <- chron$sd <- NULL

ncep <- read_csv("outputs/ncepdata.csv")
ncep %>%
    group_by(year) %>%
    summarise(month = "annual",
              air = mean(air),
              pr_wtr = sum(pr_wtr)) ->
    ncep_summary
ncep_combined <- rbind(ncep, ncep_summary)
x <- melt(ncep_combined, id.vars = c("year", "month"))
y <- dcast(x, year ~ variable + month)

d <- merge(chron, y, by.x = "Year", by.y = "year")

printlog("All done with", SCRIPTNAME)

save_data(ncepdata, scriptfolder = FALSE)

print(sessionInfo())
sink()
