#############
## Shallow ##
#############

range(no.s$no)
bxplts("no", ofst = 0.01, no.s)
# log looks better


# pre-co2
model1 <- lme(log(no + 0.01) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = no.s)

ml <- smpl.ml(model1)
ml$model.reml
anova(ml$model.reml)

# post-co2
model1 <- lme(log(no + 0.01) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11"), data = no.s)

# autocorrelation
models <- atcr.cmpr(model1)
models
# model4 looks better
m4 <- models[[4]]

# model simplificaiton
m5 <- ana(m4)
anova(m5$model.reml)

########
# Deep #
########
range(no.d$no)
bxplts("no", ofst = 0.01, no.d)
# log looks better

# pre-co2
model1 <- lme(log(no) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = no.d)
models <- atcr.cmpr(model1)
models
# model 5
anova(models[[5]])

# post-co2
model1 <- lme(log(no) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11"), data = no.d)
ml <- smpl.ml(model1)
ml$model.reml
anova(ml$model.reml)


models <- atcr.cmpr(model1)
models
# model4
anova(models[[4]])
fml <- ana(models[[4]])
anova(fml$model.reml)




