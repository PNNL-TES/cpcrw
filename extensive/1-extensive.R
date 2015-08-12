# R script to
#  - read in tree survey and display results
# This was a quick script made during the survey to check out progress

# Ben Bond-Lamberty July 2014

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME      <- "1-extensive.R"

TRANSECT_INVENTORY    <- "../tree_survey/tree_survey.csv"

EXTENSIVE_INVENTORY   <- "extensive_inventory.csv"
EXTENSIVE_PLOTS       <- "extensive_plots.csv"

# -----------------------------------------------------------------------------
# Packages and reproducibility

#library(checkpoint)  # version 0.3.8
#checkpoint("2015-02-27")
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(ggmap)

# ==============================================================================
# Main

sink(file.path(LOG_DIR, paste0(SCRIPTNAME, ".txt")), split=T)

printlog("Welcome to", SCRIPTNAME)

inventory <- read_csv(EXTENSIVE_INVENTORY)
plots <- read_csv(EXTENSIVE_PLOTS, skip=1)

printlog("Computing summary...")
ha <- 100 * 100 # meters in 1 hectare
extensive_summary <- inventory %>%
  select(-Notes) %>%
  right_join(plots[c("Plot", "PlotRadius_m")], by=c("Plot")) %>%
  group_by(Plot, Species) %>%
  summarise(BA_m2 = sum((DBH_cm / 100 / 2) ^ 2 * pi),
            ntrees = n(),
            area = mean(PlotRadius_m) ^ 2 * pi,
            `BA (m2/ha)` = BA_m2 * ha / area,
            `Density (/ha)` = ntrees * ha / area)

transect_inventory <- read_csv(TRANSECT_INVENTORY)
transect_summary <- transect_inventory %>%
  select(-Date, -Notes) %>%
  filter(Status == "Alive") %>%
  group_by(Transect, Position_m, Species) %>%
  summarise(BA_m2 = sum((DBH_cm / 100 / 2) ^ 2 * pi),
            ntrees = n(),
            area = 5 ^ 2 * pi,
            `BA (m2/ha)` = BA_m2 * ha / area,
            `Density (/ha)` = ntrees * ha / area)

p <- ggplot(transect_summary, aes(`BA (m2/ha)`, `Density (/ha)`, color=Species))
p <- p + geom_point()
p <- p + geom_point(data=extensive_summary, size=I(10), alpha=I(.5)) 
p <- p + coord_trans(x = "log10", y = "log10")
p <- p + ggtitle("Transect data (small points) versus extensive (large points)")
print(p)
save_plot("transect_extensive_comparison")


# Make a map of the points
printlog("Mapping...")
mapcpcrw <- get_map(location = c(lon = mean(plots$Lon), lat = mean(plots$Lat)), 
                    zoom = 12, maptype = "satellite", scale = 2)
p <- ggmap(mapcpcrw) + geom_point(data=plots, aes(x=Lon, y=Lat), color="red")
# p + geom_text(data=plots, aes(Lon, Lat, label=Plot), color="red", size=4, hjust=.5, vjust=-.5)
print(p)
save_plot("extensive_map")

save_data(extensive_summary)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()

