# R script to read core image data, compute ring widths,
# rub some quality control (graphs, printing outliers), and save data
# Output file is 'ringwidths.csv'

# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("0-functions.R")

SCRIPTNAME		<- "2-read_imagedata.R"
INPUT_DIR       <- "core_data/"

BADSCANS <- "scans_bad.txt"
GOODSCANS <- "scans_good.txt"
BROKENSCANS <- "scans_broken.txt"

# ==============================================================================
# Main

if(!file.exists(OUTPUT_DIR)) {
    printlog("Creating", OUTPUT_DIR)
    dir.create(OUTPUT_DIR)
}
if(!file.exists(LOG_DIR)) {
    printlog("Creating", LOG_DIR)
    dir.create(LOG_DIR)
}

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(magrittr)

printlog("Reading image data...")
imagelist <- list.files(pattern="*.pos", path=INPUT_DIR)

startyear <- 2013
printlog("Assuming that first ring present is", startyear)
results <- data.frame()
for(f in imagelist) {
    printlog("Reading", f)
    d <- read.table(paste(INPUT_DIR, f, sep="/"), skip=5, sep=",", col.names=c("x", "y"))
    d1 <- d[-1,]
    d2 <- d[-nrow(d),]
    d1$Width_mm <- sqrt((d1$x-d2$x) ^ 2 + (d1$y-d2$y) ^ 2)
    d1$x <- d1$y <- NULL
    d1$Year <- startyear:(startyear - nrow(d1) + 1)
    d1$Core <- sub(".*_", "", f) %>% 
        sub(".pos", "", .) %>%
        sub("^0", "", .) %>%
        toupper()
    results <- rbind(results, d1)
}


# TODO: read badscans, goodscans, brokenscans, merge, check


printdims(results)

printlog("Doing QC on image data...")

results$decade <- floor(results$Year / 10) * 10
results$decade <- paste0(results$decade, "-", results$decade + 9)
results$label <- ""
highvals <- which(results$Width_mm > 3)
results[highvals, "label"] <- paste(results$Core[highvals], results$Year[highvals])

p1 <- ggplot(results, aes(Year, Width_mm, color=Core)) + 
    geom_point() +
    geom_text(aes(label=label), size=3, vjust=-1)
print(p1)
saveplot("2-imagedata_qc1")

p2 <- ggplot(results, aes(decade, Width_mm)) + 
    geom_boxplot() + 
    geom_text(aes(label=label), size=3, vjust=-1)
print(p2)
saveplot("2-imagedata_qc2")

for(d in unique(results$decade)) {
    r <- results[results$decade==d,]
    outliers <- r[is.outlier(r$Width_mm, 10),]
    printlog(nrow(outliers), "outliers in decade", d, "(out of", nrow(r), "rings)")
    if(nrow(outliers)) print(outliers)
}

ringwidths <- results
ringwidths$decade <- ringwidths$label <- NULL
savedata(ringwidths)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
