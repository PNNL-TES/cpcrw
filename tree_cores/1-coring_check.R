# R script to QC tree core data
# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("0-functions.R")

SCRIPTNAME		<- "1-coring_check.R"

# ==============================================================================
# Main

sink( paste( LOG_DIR, paste0( SCRIPTNAME, ".txt" ), sep="/" ), split=T )

printlog( "Welcome to", SCRIPTNAME )

loadlibs( c( "ggplot2", "reshape", "plyr", "lubridate" ) )	# the hadleyverse
theme_set( theme_bw() )

# ----- Main script goes here...

# Read core data
coredata <- read.csv("tree_cores.csv")
coredata$type <- "cores"
coredata <- coredata[c("Transect", "Position", "Species", "DBH_cm", "type")]

# Read inventory data and filter for what we want
inventorydata <- read.csv("../tree_survey/tree_survey.csv")
inventorydata$type <- "inventory"
inventorydata <- inventorydata[inventorydata$Status=="Alive", c("Transect", "Position", "Species", "DBH_cm", "type")]

d <- rbind(coredata, inventorydata)

# Graph
p1 <- ggplot(d, aes(factor(Position), DBH_cm, color=type)) 
p1 <- p1 + geom_boxplot() + facet_grid(Transect~.)
p1 <- p1 + xlab("Transect position (m)")
print(p1)
#ggsave("corecheck1.pdf")

p2 <- ggplot(d, aes(DBH_cm)) + geom_density(aes(fill=type), alpha=.5)
p2 <- p2 + facet_grid(Transect~.)
print(p2)
#ggsave("corecheck2.pdf")


# Report
print("Where does inventory DBH exceed core DBH?")
library(plyr)
dsum <- ddply(d, .(Transect,Position,type), summarise, max_DBH=max(DBH_cm))
library(reshape2)
dsum <- na.omit(dcast(dsum,Transect+Position~type))
dsum$Exceeds <- ""
dsum[dsum$inventory-dsum$cores > 1, "Exceeds"] <- "***"
print(dsum)


printlog( "All done with", SCRIPTNAME )
print( sessionInfo() )
sink()
