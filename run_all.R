# This is a poor--very poor--man's makefile
# BBL February 2015

#source('0-functions.R')

source('transects/1-collars.R', chdir=TRUE)

source('tree_cores/1-coring_check.R', chdir=TRUE)
source('tree_cores/2-file_qc.R', chdir=TRUE)
source('tree_cores/3-read_coodata.R', chdir=TRUE)
source('tree_cores/4-increment_models.R', chdir=TRUE)
source('tree_cores/5-chronology.R', chdir=TRUE)

source('tree_survey/1-tree_survey.R', chdir=TRUE)
source('tree_survey/2-npp.R', chdir=TRUE)
source('tree_survey/3-explore_npp.R', chdir=TRUE)

# SLA depends on ALD and LAI
source('sla/1-sla.R', chdir=TRUE)
