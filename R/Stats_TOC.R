###########
# Shallow #
###########
source("R/Stats_TOC_shallow.R")

########
# Deep #
########
source("R/Stats_TOC_deep.R")

############
# Leaching #
############
summary(lys)

# melt
lysMlt <- melt(lys, id = names(lys)[which(!(names(lys) %in% c("no", "nh", "po", "toc", "tc", "ic", "tn")))])

lysLeachMlt <- ddply(lysMlt, .(date, time, ring, plot, co2, time, pre, post, variable), 
                     summarise, 
                     leach = value[depth == "shallow"] - value[depth == "deep"])

# remove NA
lysLeachMlt <- lysLeachMlt[complete.cases(lysLeachMlt), ]

# cast
head(lysLeachMlt)
lysLeach <- cast(lysLeachMlt, date + time + ring + plot + co2 + pre + post ~ variable, value="leach")
head(lysLeach)



