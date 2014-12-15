# Support functions used by all functions in sub-folders
# Ben Bond-Lamberty December 2014
# TODO: very inconsistent function naming scheme

OUTPUT_DIR		<- "outputs/"
LOG_DIR			<- "logs/"

SEPARATOR		<- "-------------------"

# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function(msg="", ..., ts=TRUE, cr=TRUE) {
	if(ts) cat(date(), " ")
	cat(msg, ...)
	if(cr) cat("\n")
} # printlog

# -----------------------------------------------------------------------------
# Print dimensions of data frame
printdims <- function(d, dname=deparse(substitute(d))) {
	stopifnot(is.data.frame(d))
	printlog(dname, "rows =", nrow(d), "cols =", ncol(d))
} # printdims

# -----------------------------------------------------------------------------
# Return matrix of memory consumption
object.sizes <- function() {
    rev(sort(sapply(ls(envir=.GlobalEnv), function(object.name) 
        object.size(get(object.name)))))
}

# -----------------------------------------------------------------------------
# Return output directory (perhaps inside a script-specific folder)
# If caller species `scriptfolder=FALSE`, return OUTPUT_DIR
# If caller species `scriptfolder=TRUE` (default), return OUTPUT_DIR/SCRIPTNAME
outputdir <- function(scriptfolder=TRUE) {
    output_dir <- OUTPUT_DIR
    if(scriptfolder) output_dir <- paste(output_dir, 
                                         sub(".R$", "", SCRIPTNAME), sep="/")
    if(!file.exists(output_dir)) dir.create(output_dir)
    output_dir
}

# -----------------------------------------------------------------------------
# Save a ggplot figure
saveplot <- function(pname, p=last_plot(), ptype=".pdf", scriptfolder=TRUE) {
	fn <- paste0(outputdir(scriptfolder), "/", pname, ptype)
	printlog("Saving", fn)
	ggsave(fn, p)
} # saveplot

# -----------------------------------------------------------------------------
# Save a data frame
savedata <- function(df, extension=".csv", scriptfolder=TRUE) {
    fn <- paste0(outputdir(scriptfolder), "/", deparse(substitute(df)), extension)
	printlog("Saving", fn)
	write.csv(df, fn, row.names=F)
} # saveplot

# -----------------------------------------------------------------------------
# Open a netCDF file and return handle
open_ncdf <- function(fn, datadir=".") {
	fqfn <- paste(normalizePath(datadir), fn, sep="/")
	printlog("Opening", fqfn)
	stopifnot(file.exists(fqfn))
	open.ncdf(fqfn)
} # open_ncdf

# -----------------------------------------------------------------------------
# Open a csv file and return data
read_csv <- function(fn, datadir=".", ...) {
	fqfn <- paste(datadir, fn, sep="/")
	printlog("Opening", fqfn)
	stopifnot(file.exists(fqfn))
	read.csv(fqfn, stringsAsFactors=F, ...)
} # read_csv

# -----------------------------------------------------------------------------
# Read data from the clipboard
paste_data <- function(header=TRUE) {
	read.table(pipe("pbpaste"), header=header)
} # paste_data

# -----------------------------------------------------------------------------
is.outlier <- function(x, devs=3.2) {
    # See: Davies, P.L. and Gather, U. (1993).
    # "The identification of multiple outliers" (with discussion)
    # J. Amer. Statist. Assoc., 88, 782-801.
    
    x <- na.omit(x)
    lims <- median(x) + c(-1, 1) * devs * mad(x, constant = 1)
    x < lims[ 1 ] | x > lims[2]
}



if( !file.exists(OUTPUT_DIR)) {
    printlog("Creating", OUTPUT_DIR)
    dir.create(OUTPUT_DIR)
}
if( !file.exists( LOG_DIR)) {
    printlog("Creating", LOG_DIR)
    dir.create(LOG_DIR)
}

