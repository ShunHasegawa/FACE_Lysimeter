###########
# Shallow #
###########
range(po.s$po)

bxplts("po", ofst = 0.02, data = po.s)

# remove outlier
po.s <- po.s[po.s$po < max(po.s$po), ]

range(po.s$po)

bxplts("po", ofst = 0.015, data = po.s)
# log looks better

levels(po.s$time)

# pre-co2
model1 <- lme(log(po + 0.015) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = po.s)

ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(log(po + 0.015) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11", "12"), data = po.s)
ml <- smpl.ml(model1)
anova(ml$model.reml)

########
# Deep #
########
range(po.d$po)
bxplts("po", ofst = 0.01, data = po.d)

# sqrt

levels(po.d$time)

# pre-co2
model1 <- lme(sqrt(po+0.01) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = po.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(sqrt(po+0.01) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11", "12"), data = po.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)
