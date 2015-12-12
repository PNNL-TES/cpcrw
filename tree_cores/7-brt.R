# R script to get analyze tree ring chronology as a function of climate
# Done kind of quickly, for AGU
# Ben Bond-Lamberty December 2015

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME        <- "7-brt.R"

library(dplyr)
library(stringr)
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
chron_low <- read_csv("outputs/chron_low.csv")
chron_low <- chron_low[c("Year", "xxxstd")]
chron_high <- read_csv("outputs/chron_low.csv")
chron_high <- chron_high[c("Year", "xxxstd")]


printlog("Summarizing and reshaping climate data...")
ncep <- read_csv("outputs/ncepdata.csv")
names(ncep)[which(names(ncep) == "air")] <- "airtemp"
names(ncep)[which(names(ncep) == "pr_wtr")] <- "precip"
ncep %>%
    group_by(year) %>%
    summarise(month = "annual",
              airtemp = mean(airtemp),
              precip = sum(precip)) ->
    ncep_summary
ncep_combined <- rbind(ncep, ncep_summary)
x <- melt(ncep_combined, id.vars = c("year", "month"))
xlag <- x
xlag$year <- xlag$year + 1
xlag$variable <- paste0(xlag$variable, "1")
x$variable <- paste0(x$variable, "0")
y <- dcast(rbind(x, xlag), year ~ variable + month)

printlog("Merging core and climate data...")
d <- merge(chron, y, by.x = "Year", by.y = "year")
d_low <- merge(chron_low, y, by.x = "Year", by.y = "year")
d_high <- merge(chron_high, y, by.x = "Year", by.y = "year")

printlog("Running boosted regressions...")
library(gbm)

nvars <- 5
set.seed(20151210)
m_gbm <- gbm(xxxstd ~ ., data = d[-1], n.trees = 500)
smry <- summary(m_gbm)[1:nvars,]
smry$Dataset <- "All"
m_gbm_low <- gbm(xxxstd ~ ., data = d_low[-1], n.trees = 500)
smry_low <- summary(m_gbm_low)[1:nvars,]
smry_low$Dataset <- "Low"
m_gbm_high <- gbm(xxxstd ~ ., data = d_high[-1], n.trees = 500)
smry_high <- summary(m_gbm_high)[1:nvars,]
smry_high$Dataset <- "High"
smry <- rbind(smry, smry_high, smry_low)
# test both upslope and downslope trees

smry_parse <- as.data.frame(str_split_fixed(as.character(smry$var), pattern = "_", n = 2))
names(smry_parse) <- c("variable", "month")
smry_parse$month <- month.abb[as.numeric(as.character(smry_parse$month))]
smry$month <- factor(smry$month, levels = month.abb)
smry <- cbind(smry, smry_parse)

p <-ggplot(smry, aes(Dataset, rel.inf, fill = Dataset)) 
p <- p + geom_bar(stat='identity', position='dodge') + facet_grid(variable ~ month)
p <- p + xlab("Month + landscape position") + ylab("Relative influence")
p <- p + scale_fill_discrete(guide = FALSE)
print(p)
save_plot("gbm")

printlog("All done with", SCRIPTNAME)


print(sessionInfo())
sink()
