# R script to QC tree core data
# Ben Bond-Lamberty December 2014

# Support functions and common definitions

SCRIPTNAME		<- "0-qc.R"
OUTPUT_DIR		<- "outputs/"
LOG_DIR			<- "logs/"
RANDOM_SEED		<- 12345		# comment out to not set seed
SEPARATOR		<- "-------------------"

# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE ) {
	if( ts ) cat( date(), " " )
	cat( msg, ... )
	if( cr ) cat( "\n")
} # printlog

# -----------------------------------------------------------------------------
# Print dimensions of data frame
printdims <- function( d, dname=deparse( substitute( d ) ) ) {
	stopifnot( is.data.frame( d ) )
	printlog( dname, "rows =", nrow( d ), "cols =", ncol( d ) )
} # printdims

# -----------------------------------------------------------------------------
# Return matrix of memory consumption
object.sizes <- function() {
    return( rev( sort( sapply( ls( envir=.GlobalEnv ), function( object.name ) 
        object.size( get( object.name ) ) ) ) ) )
}

# -----------------------------------------------------------------------------
# Save a ggplot figure
saveplot <- function( pname, p=last_plot(), ptype=".pdf" ) {
	stopifnot( file.exists( OUTPUT_DIR ) )
	fn <- paste0( OUTPUT_DIR, "/", pname, ptype )
	printlog( "Saving", fn )
	ggsave( fn, p )
} # saveplot

# -----------------------------------------------------------------------------
# Save a data frame
savedata <- function( df, extension=".csv" ) {
	stopifnot( file.exists( OUTPUT_DIR ) )
	fn <- paste0( OUTPUT_DIR, "/", deparse( substitute( df ) ), extension )
	printlog( "Saving", fn )
	write.csv( df, fn, row.names=F )
} # saveplot

# -----------------------------------------------------------------------------
# Open a csv file and return data
read_csv <- function( fn, datadir=".", ... ) {
	fqfn <- paste( normalizePath( datadir ), fn, sep="/" )
	printlog( "Opening", fqfn )
	stopifnot( file.exists( fqfn ) )
	read.csv( fqfn, stringsAsFactors=F, ... )
} # read_csv

# -----------------------------------------------------------------------------
# Load requested libraries
loadlibs <- function( liblist ) {
	printlog( "Loading libraries..." )
	loadedlibs <- vector()
	for( lib in liblist ) {
		printlog( "Loading", lib )
		loadedlibs[ lib ] <- require( lib, character.only=T )
		if( !loadedlibs[ lib ] )
			warning( "this package is not installed!" )
	}
	invisible( loadedlibs )
} # loadlibs


# ==============================================================================
# Main

if( !file.exists( OUTPUT_DIR ) ) {
	printlog( "Creating", OUTPUT_DIR )
	dir.create( OUTPUT_DIR )
}
if( !file.exists( LOG_DIR ) ) {
	printlog( "Creating", LOG_DIR )
	dir.create( LOG_DIR )
}

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


print( sessionInfo() )
printlog( "All done with", SCRIPTNAME )
sink()
