rm(list=ls(all=TRUE))

source("R/pckg.R")

source("R/functions.R")

################
# Process data #
################
# source("R/process.combine.data.R")

# load data
load("output//data//FACE_lysimeter.RData")

#######################
# Excel summary table #
#######################
source("R/SummaryExlTable.R")

########
# Figs #
########
source("R//Figs.R")

#########
# Stats #
#########
source("R/Stats.R")
