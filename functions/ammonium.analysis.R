###########
# Shallow #
###########
range(nh.s$nh)

bxplts("nh", ofst = 0.056, data = nh.s)
# log looks better

# pre-co2
model1 <- lme(log(nh + 0.056) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = nh.s)

ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(log(nh + 0.056) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11"), data = nh.s)
ml <- smpl.ml(model1)
anova(ml$model.reml)
# there is co2:time interaction --> do contrast

# contrast
levels(nh.s$time)[3:12]

cntrst<- contrast(ml$model.reml,
                  a=list(time=levels(nh.s$time)[3:12],co2="amb"),
                  b=list(time=levels(nh.s$time)[3:12],co2="elev"))

# not working...

########
# Deep #
########
range(nh.d$nh)
bxplts("nh", ofst = 0.08, data = nh.d)

# no transformation
# pre-co2
model1 <- lme(nh+0.08 ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = nh.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(nh + 0.08 ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11"), data = nh.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)
# there is co2:time interaction

# contrast
levels(nh.d$time)[3:8]
cntrst<- contrast(ml$model.reml,
                  a=list(time=levels(nh.d$time)[3:8],co2="amb"),
                  b=list(time=levels(nh.d$time)[3:8],co2="elev"))

# not working