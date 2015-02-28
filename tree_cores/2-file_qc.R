# R script to QC tree core data
# There are three data steps checked for consistency here:
#   cores - the core numbers found in tree_cores.csv
#   images - the images found in the core_images directory
#   data - the *.pos data files foung in the core_data directory
# These should all be consistent: every tree core should have one and
# only one image, and one and only one data file

# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("../0-functions.R")

SCRIPTNAME		<- "2-file_qc.R"
IMAGE_DIR       <- "core_images/"
DATA_DIR        <- "core_data/"
COREDATA_FILE   <- "tree_cores.csv"

# -----------------------------------------------------------------------------
# Packages and reproducibility

library(checkpoint)  # version 0.3.8
checkpoint("2015-02-27")
library(magrittr)
library(ggplot2)
theme_set(theme_bw())

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

printlog("Reading image list...")
imagelist <- list.files(pattern="*.jpg", path=IMAGE_DIR)
imagenums <- sub(".*_", "", imagelist) %>% 
    sub(".jpg", "", .) %>%
    sub("^0+", "", .) %>%
    toupper()

printlog("Reading data list...")
datalist <- list.files(pattern="*.pos", path=DATA_DIR)
datanums <- sub(".*_", "", datalist) %>% 
    sub(".pos", "", .) %>%
    sub("^0+", "", .) %>%
    toupper()

printlog("Reading tree core list...")
coredata <- read.csv(COREDATA_FILE)
corenums <- toupper(coredata$Core)

printlog(length(corenums), "cores", "listed in", COREDATA_FILE)
printlog(length(imagenums), "image files found in", IMAGE_DIR)
printlog(length(datanums), "data files found in", DATA_DIR)

printlog(length(intersect(imagenums, corenums)), "cores with images")
printlog(length(intersect(datanums, corenums)), "cores with data")
printlog(length(intersect(imagenums, corenums)), "images with data")

cwi <- setdiff(corenums, imagenums)
printlog(length(cwi), "cores without images")
if(length(cwi)) print(cwi)
iwc <- setdiff(imagenums, corenums)
printlog(length(iwc), "images without cores")
if(length(iwc)) print(iwc)
dwi <- setdiff(datanums, imagenums)
printlog(length(dwi), "data without images")
if(length(dwi)) print(dwi)
dwc <- setdiff(datanums, corenums)
printlog(length(dwc), "data without cores")
if(length(dwc)) print(dwc)

if(any(duplicated(corenums))) {
    printlog("Duplicate core numbers found:")
    print(imagenums[duplicated(corenums)])
} else {
    printlog("No duplicate core numbers")
}
if(any(duplicated(imagenums))) {
    printlog("Duplicate image numbers found:")
    print(imagenums[duplicated(imagenums)])
} else {
    printlog("No duplicate image numbers")
}
if(any(duplicated(datanums))) {
    printlog("Duplicate data numbers found:")
    print(imagenums[duplicated(datanums)])
} else {
    printlog("No duplicate data numbers")
}

printlog("All done with", SCRIPTNAME)

print(sessionInfo())
sink()
