###########
# Shallow #
###########
range(tn.s$tn)

bxplts("tn", ofst = 1, data = tn.s)

# log looks better

levels(tn.s$time)

# pre-co2
model1 <- lme(log(tn+1) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = tn.s)

ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(log(tn+1) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11", "12", "13"), data = tn.s)
ml <- smpl.ml(model1)
anova(ml$model.reml)

########
# Deep #
########
range(tn.d$tn)

bxplts("tn", data = tn.d)

# log

levels(tn.d$time)

# pre-co2
model1 <- lme(log(tn) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = tn.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(log(tn) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11", "12", "13"), data = tn.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)
