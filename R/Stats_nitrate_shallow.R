## ----Stat_FACE_Lys_Nitrate_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$no[lys$depth == "shallow" & lys$pre])

bxplts(value = "no", data = subsetD(lys, depth == "shallow" & pre))

# log seems slightly better

# different random factor structure
m1 <- lme(log(no) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & pre))
m2 <- lme(log(no) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & pre))
m3 <- lme(log(no) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & pre))
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atcr.cmpr(m3)$models
# model3 is best

Iml_S_pre <- atcr.cmpr(m3)[[3]]

# The initial model is: 
Iml_S_pre$call

Anova(Iml_S_pre)

# model simplification
# MdlSmpl(Iml_S_pre)
# error message
# probably because if time is removed we can't use 
# autocorrelation any more, so let's do it without 
# autocorrelation again

Iml_S_pre <- atcr.cmpr(m3)[[1]]
Iml_S_pre$call

MdlSmpl(Iml_S_pre)

Fml_S_pre <- MdlSmpl(Iml_S_pre)$model.reml

# The final model is:
Fml_S_pre$call

anova(Fml_S_pre)

# model diagnosis
plot(Fml_S_pre)
qqnorm(Fml_S_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_pre))
qqline(residuals.lm(Fml_S_pre))
# not very great though


## ----Stat_FACE_Lys_Nitrate_S_postCO2

###########
# Post-CO2 #
###########

range(lys$no[lys$depth == "shallow" & lys$post])

bxplts(value = "no", ofst= 0.003, data = subsetD(lys, depth == "shallow" & post))
bxplts(value = "no", ofst= 0.01, data = subsetD(lys, depth == "shallow" & post))
# log seems slightly better

# The initial model is
Iml_S_post <- lmer(log(no + .01) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "shallow" & post), na.action = "na.omit")
Anova(Iml_S_post)

# The final model
Fml_S_post <- stepLmer(Iml_S_post)
Anova(Fml_S_post)
AnvF_Nit_S_Post <- Anova(Fml_S_post, test.statistic = "F")
AnvF_Nit_S_Post

summary(Fml_S_post)

# model diagnosis
plot(Fml_S_post)
qqnorm(resid(Fml_S_post))
qqline(resid(Fml_S_post))

## ----Stat_FACE_Lys_Nitrate_S_preCO2_Smmry
# The initial model is:
Iml_S_pre$call
Anova(Iml_S_pre)

# The final model is :
Fml_S_pre$call
anova(Fml_S_pre)

## ----Stat_FACE_Lys_Nitrate_S_postCO2_Smmry
# The initial model is:
Iml_S_post@call
Anova(Iml_S_post)

# The final model is :
Fml_S_post@call

# Chi
Anova(Fml_S_post)

# F
AnvF_Nit_S_Post