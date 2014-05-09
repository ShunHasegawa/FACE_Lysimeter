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
                     leach = value[depth == "shallow"] - value[depth == "deep"])

# remove NA
lysLeachMlt <- lysLeachMlt[complete.cases(lysLeachMlt), ]

# cast
head(lysLeachMlt)
lysLeach <- cast(lysLeachMlt, date + time + ring + plot + co2 + pre + post + id ~ variable, value="leach")
head(lysLeach)

###########
# Pre-CO2 #
###########
lysLeachMlt
range(lysLeach$toc[lysLeach$post], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lysLeach, pre), ofst= 14)
bxplts(value = "toc", data = subsetD(lysLeach, post), ofst= 14)


# log seems better

# different random factor structure
m1 <- lme(sqrt(toc + 14) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lysLeach, post), 
          na.action = "na.omit")
m2 <- lme(sqrt(toc + 14) ~ co2 * time, random = ~1|ring, data = subsetD(lysLeach, post),
          na.action = "na.omit")
m3 <- lme(sqrt(toc + 14) ~ co2 * time, random = ~1|id, data = subsetD(lysLeach, post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# model1 is best

Iml_Leach_post <- atml[[1]]

# The initial model is: 
Iml_Leach_post$call

Anova(Iml_Leach_post)

