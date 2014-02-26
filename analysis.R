rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")

# functions
source("functions/functions.R")

################
# Process data #
################
source("functions/process.combine.data.R")

## split dataset to each nutrient & depth ##
ntrs <- c("no", "nh", "po", "toc", "tn")
lyses <- lapply(ntrs, function(x) ntr.splt (ntr = x))

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


#########
# Stats #
#########

###########
# Nitrate #
###########
source("functions/nitrate.analysis.R")

############
# Ammonium #
############
source("functions/ammonium.analysis.R")

#############
# Phosphate #
#############
source("functions/phosphate.analysis.R")




