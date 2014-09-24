# Check to make sure tree cores span each plot and species' diameter range
# BBL September 2014

library(ggplot2)
theme_set(theme_bw())

coredata <- read.csv("tree_cores.csv")
coredata$type <- "cores"
coredata <- coredata[c("Transect", "Position", "Species", "DBH_cm", "type")]

inventorydata <- read.csv("../tree_survey/tree_survey.csv")
inventorydata$type <- "inventory"
inventorydata <- inventorydata[c("Transect", "Position", "Species", "DBH_cm", "type")]


d <- rbind(coredata, inventorydata)

# Graph
p <- ggplot(d, aes(factor(Position), DBH_cm, color=type)) 
p <- p + geom_violin() + facet_grid(Transect~.)
p <- p + xlab("Transect position (m)")
print(p)
#ggsave("corecheck.pdf")

# Report
print("Where does inventory DBH exceed core DBH?")
library(plyr)
dsum <- ddply(d, .(Transect,Position,type), summarise, max_DBH=max(DBH_cm))
library(reshape2)
dsum <- na.omit(dcast(dsum,Transect+Position~type))
dsum$Exceeds <- ""
dsum[dsum$inventory>dsum$cores, "Exceeds"] <- "***"
print(dsum)

print("All done.")
