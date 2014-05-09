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

lysLeachMlt <- ddply(lysMlt, .(date, time, ring, plot, co2, time, pre, post, variable, id), 
                     summarise, 
                     leach = (value[depth == "shallow"] - value[depth == "deep"]) / value[depth == "shallow"])

# remove NA
lysLeachMlt <- lysLeachMlt[complete.cases(lysLeachMlt), ]

# remove inf
lysLeachMlt <- lysLeachMlt[!is.infinite(lysLeachMlt$leach), ]

# cast
lysLeach <- cast(lysLeachMlt, date + time + ring + plot + co2 + pre + post + id ~ variable, value="leach")

###########
# Post-CO2 #
###########
lysLeachMlt
range(lysLeach$toc[lysLeach$post], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lysLeach, post), ofst= 2)
bxcxplts(value = "toc", data = subsetD(lysLeach, post), sval= 1.8048, fval = 2)
bxplts(value = "toc", data = subsetD(lysLeach, post), ofst= 1.8048)
# box-cox lambda

# different random factor structure
m1 <- lme((toc + 1.8048)^2 ~ co2 * time, random = ~1|ring/plot, data = subsetD(lysLeach, post), 
          na.action = "na.omit")
m2 <- lme((toc + 1.8048)^2 ~ co2 * time, random = ~1|ring, data = subsetD(lysLeach, post),
          na.action = "na.omit")
m3 <- lme((toc + 1.8048)^2 ~ co2 * time, random = ~1|id, data = subsetD(lysLeach, post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# model5 is best

Iml_Leach_post <- atml[[5]]

# The initial model is: 
Iml_Leach_post$call

Anova(Iml_Leach_post)

