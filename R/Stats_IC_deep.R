## ----Stat_FACE_Lys_IC_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$ic[lys$depth == "deep" & lys$pre], na.rm = TRUE)

bxcxplts(value = "ic", data = subsetD(lys, depth == "deep" & pre), sval = .001, fval =.01)
bxplts(value = "ic", ofst=.001, data = subsetD(lys, depth == "deep" & pre))
# power(1/3) seems slightly better

# different random factor structure
m1 <- lme(ic^(1/3) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & pre), 
          na.action = "na.omit")
m2 <- lme(ic^(1/3) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & pre),
          na.action = "na.omit")
m3 <- lme(ic^(1/3) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & pre),
          na.action = "na.omit")
anova(m1, m2, m3)
# m2 is better

# autocorrelation
atml <- atcr.cmpr(m2, rndmFac= "ring")
atml$models
# no need for autocorrelation

Iml_D_pre <- atml[[1]]

# The initial model is: 
Iml_D_pre$call

Anova(Iml_D_pre)

# model simplification
MdlSmpl(Iml_D_pre)
# no factor was removed

Fml_D_pre <- MdlSmpl(Iml_D_pre)$model.reml

# The final model is:
Fml_D_pre$call

Anova(Fml_D_pre)

# model diagnosis
plot(Fml_D_pre)
qqnorm(Fml_D_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_pre))
qqline(residuals.lm(Fml_D_pre))

## ----Stat_FACE_Lys_IC_D_postCO2

############
# Post-CO2 #
############

range(lys$ic[lys$depth == "deep" & lys$post], na.rm = TRUE)
bxcxplts(value = "ic", data = subsetD(lys, depth == "deep" & post), sval = 0.001, fval =5)
bxcxplts(value = "ic", data = subsetD(lys, depth == "deep" & post), sval = 0.01, fval =.5)
# adding .01 may improve

bxplts(value = "ic", ofst=.01, data = subsetD(lys, depth == "deep" & post))
# use box cox lambda

# different random factor structure
m1 <- lme((ic+.01)^(-0.0202) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & post), 
          na.action = "na.omit")
m2 <- lme((ic+.01)^(-0.0202) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & post),
          na.action = "na.omit")
m3 <- lme((ic+.01)^(-0.0202) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m1 is better

# autocorrelation
atml <- atcr.cmpr(m1, rndmFac= "ring/plot")
atml$models
# model4 is best

Iml_D_post <- atml[[4]]

# The initial model is: 
Iml_D_post$call

Anova(Iml_D_post)

# model simplification
MdlSmpl(Iml_D_post)
# co2:time and co2 are removed

Fml_D_post <- MdlSmpl(Iml_D_post)$model.reml

# The final model is:
Fml_D_post$call

Anova(Fml_D_post)

# model diagnosis
plot(Fml_D_post)
qqnorm(Fml_D_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_post))
qqline(residuals.lm(Fml_D_post))

## ----Stat_FACE_Lys_IC_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
anova(Fml_D_pre)

## ----Stat_FACE_Lys_IC_D_postCO2_Smmry
# The initial model is:
Iml_D_post$call
Anova(Iml_D_post)

# The final model is :
Fml_D_post$call
Anova(Fml_D_post)
