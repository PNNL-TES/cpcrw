# R script to process tree ring data recorded with CooRecorder
# Ben Bond-Lamberty December 2014

# Support functions and common definitions
source("0-functions.R")

DATA_DIR        <- "core_data/"

SCRIPTNAME    	<- "3-read_coodata.R"

# -----------------------------------------------------------------------------
# Scan a read-in *.pos file and extract date (if supplied)
determine_lastyear <- function(d)
{
    lyindex <- which(grepl("DATED", d))
    if(length(lyindex) > 0) { 
        lastyr <- as.numeric(substr(d[lyindex], nchar(d[lyindex])-3, nchar(d[lyindex])))
    } else {
        lastyr <- NA
    } 
    return(lastyr)
}

# ==============================================================================
# Main

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(reshape2)
library(magrittr)

# Read in CooRecorder data files, compute ring increments, and write out a data frame
printlog("Welcome to coodata.R")
printlog("We know how to process CooRecorder data files!")
path <- DATA_DIR
printlog("Looking in", path)
files <- list.files(path=path, pattern="*.pos")

# Quickly scan files looking for year data
defaultlastyr <- 2013
maxrings <- 0
printlog("Scanning for last year data; default =", defaultlastyr)
lastyears <- rep(defaultlastyr, length(files))
dated <- rep(FALSE, length(files))
for(i in 1:length(files))
{
    d <- readLines(paste(path, files[i], sep=""))
    maxrings <- max(maxrings, length(d)) # ignore headers
    ly <- determine_lastyear(d)
    printlog(files[i], ly, length(d))
    if(is.na(ly)) {
        printlog("*** No year found! Assigning default value ***")
    } else {
        lastyears[i] <- ly
        dated[i] <- TRUE
    }    
}

# Build the output data frame
printlog("Now building your custom-designed data frame...")
outdata <- as.data.frame(matrix(nrow=maxrings, ncol=length(files)+1))
names(outdata) <- c("Year", sub(".pos","",files))
outdata$Year <- seq(from=max(lastyears), by=-1, length.out=maxrings)

# ...and fill it
for(i in 1:length(files)) {
    printlog(files[i])
    
    d <- readLines(paste(path, files[i], sep=""))
    skiplines <- 4
    if(dated[i]) {
        skiplines <- skiplines+1  # not very elegant
    }
    d[skiplines+1] <- "0,0,0,0,0,0"		# force read.table to six columns
    d1 <- gsub(" +", ",", d)	# Change all spaces to commas
    d2 <- read.table(textConnection(d1), sep=",", skip=skiplines, col.names=c("x1", "y1", "x2", "y2", "z1", "z2"), comment.char="#", fill=T)
    d2 <- d2[-1,]
    
    firstline <- which(outdata$Year==lastyears[i])
    for(j in 1:(nrow(d2)-1)) {
        # A double set of points signifies a gap we need to skip
        if(is.na(d2[j, "x2"])) {
            newpts <- c(d2[j, "x1"], d2[j, "y1"])		# normal
        } else {
            printlog("\tGap:", j)
            newpts <- c(d2[j, "x2"], d2[j, "y2"])		# gap!
        }
        
        oldpts <- c(d2[j+1, "x1"], d2[j+1, "y1"])		# normal
        
        outdata[j, i+1] <- sqrt((newpts[1]-oldpts[1]) ^ 2 + (newpts[2]-oldpts[2]) ^ 2)
        
    }
}

printlog("We are done! Melting...")
ringwidths <- reshape2::melt(outdata, id.vars="Year", variable.name="Core", value.name="Width_mm")
ringwidths <- na.omit(ringwidths)
names(ringwidths)[2] <- "Core"
names(ringwidths)[3] <- "Width_mm"
ringwidths$Core <- ringwidths$Core %>%
    sub("cpcrw_", "", .) %>%
    sub("^0*", "", .)  # remove leading zeros (zeroes?) from core number



printdims(ringwidths)

savedata(ringwidths)

printlog("Making diagnostic plots...")

p1 <- qplot(Year, Width_mm, color=Core, data=ringwidths)
print(p1)
saveplot("3-qc1")

ringwidths$decade <- floor(ringwidths$Year / 10) * 10
ringwidths$decade <- paste0(ringwidths$decade, "-", ringwidths$decade + 9)
ringwidths$label <- ""
highvals <- which(ringwidths$Width_mm > 3)
ringwidths[highvals, "label"] <- paste(ringwidths$Core[highvals], ringwidths$Year[highvals])

p2 <- ggplot(ringwidths, aes(decade, Width_mm)) + 
    geom_boxplot() + 
    geom_text(aes(label=label), size=3, vjust=-1)
print(p2)
saveplot("3-qc2")

for(d in unique(ringwidths$decade)) {
    r <- ringwidths[ringwidths$decade==d,]
    outliers <- r[is.outlier(r$Width_mm, 10),]
    printlog(nrow(outliers), "outliers in decade", d, "(out of", nrow(r), "rings)")
    if(nrow(outliers)) print(outliers)
}

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()

