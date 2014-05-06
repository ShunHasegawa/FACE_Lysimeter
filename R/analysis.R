rm(list=ls(all=TRUE))

# functions
source("R/functions.R")


################
# Process data #
################
# source("R/process.combine.data.R")

# load data
load("output//data//FACE_lysimeter.Rdata")

## split dataset to each nutrient & depth ##
ntrs <- c("no", "nh", "po", "toc", "tn")

lyses <- lapply(ntrs, function(x) ntr.splt (data = lys, ntr = x))

no.d <- lyses[[1]]$deep
no.s <- lyses[[1]]$shallow

nh.d <- lyses[[2]]$deep
nh.s <- lyses[[2]]$shallow

po.d <- lyses[[3]]$deep
po.s <- lyses[[3]]$shallow

toc.d <- lyses[[4]]$deep
toc.s <- lyses[[4]]$shallow

tn.d <- lyses[[5]]$deep
tn.s <- lyses[[5]]$shallow

##########################
# Summary table on excel #
##########################
ring.mean <- ddply(lys, .(time, date, ring, depth, co2), 
                   function(x) colMeans(x[c("no", "nh", "po", "toc", "tc", "ic", "tn")], na.rm = TRUE))
source("functions/Lysimeter_summary_excel_table.R")

#########
# Stats #
#########
source("R/Stats.R")



