## ----Stat_FACE_Lys_IC_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$ic[lys$depth == "shallow" & lys$pre], na.rm = TRUE)
bxcxplts(value = "ic", data = subsetD(lys, depth == "shallow" & pre), sval = 0.001, fval =.1)
bxplts(value = "ic", ofst= .001, data = subsetD(lys, depth == "shallow" & pre))
# sqrt seems slightly better

# different random factor structure
m1 <- lme(sqrt(ic) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & pre), 
          na.action = "na.omit")
m2 <- lme(sqrt(ic) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & pre),
          na.action = "na.omit")
m3 <- lme(sqrt(ic) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & pre),
          na.action = "na.omit")
anova(m1, m2, m3)
# m2 is slightly better

# autocorrelation
atml <- atcr.cmpr(m2, rndmFac= "ring")
atml$models
# model4 is best

Iml_S_pre <- atml[[4]]

# The initial model is: 
Iml_S_pre$call

Anova(Iml_S_pre)

# model simplification
MdlSmpl(Iml_S_pre)
# co2:time and co2 are removed

Fml_S_pre <- MdlSmpl(Iml_S_pre)$model.reml

# The final model is:
Fml_S_pre$call

Anova(Fml_S_pre)

# model diagnosis
plot(Fml_S_pre)
  # wedge-shaped
qqnorm(Fml_S_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_pre))
qqline(residuals.lm(Fml_S_pre))
  #ops....


## ----Stat_FACE_Lys_IC_S_postCO2

############
# Post-CO2 #
############
range(lys$ic[lys$depth == "shallow" & lys$post], na.rm = TRUE)

bxcxplts(value = "ic", data = subsetD(lys, depth == "shallow" & post), sval = 0.1, fval =10)
bxcxplts(value = "ic", data = subsetD(lys, depth == "shallow" & post), sval = 0.001, fval =.01)

bxplts(value = "ic", ofst=.01, data = subsetD(lys, depth == "shallow" & post))
# use box-cox lambda, but still it's violating homogeneity of variance

# different random factor structure
m1 <- lme((ic+.01)^(-0.3838) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & post), 
          na.action = "na.omit")
m2 <- lme((ic+.01)^(-0.3838) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & post),
          na.action = "na.omit")
m3 <- lme((ic+.01)^(-0.3838) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m2 is better

# autocorrelation
atml <- atcr.cmpr(m2, rndmFac= "ring")
atml$models
# no need for acutocorrelation

Iml_S_post <- atml[[1]]

# The initial model is: 
Iml_S_post$call

Anova(Iml_S_post)

# model simplification
MdlSmpl(Iml_S_post)
# CO2:time and CO2 are removed

Fml_S_post <- MdlSmpl(Iml_S_post)$model.reml

# The final model is:
Fml_S_post$call

Anova(Fml_S_post)

# model diagnosis
plot(Fml_S_post)
  #wedge-shaped
qqnorm(Fml_S_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_post))
qqline(residuals.lm(Fml_S_post))

## ----Stat_FACE_Lys_IC_S_preCO2_Smmry
# The initial model is:
Iml_S_pre$call
xtable(Anova(Iml_S_pre), floating = FALSE)

# The final model is :
Fml_S_pre$call
xtable(Anova(Fml_S_pre), floating = FALSE)

## ----Stat_FACE_Lys_IC_S_postCO2_Smmry
# The initial model is:
Iml_S_post$call
xtable(Anova(Iml_S_post), floating = FALSE)

# The final model is :
Fml_S_post$call
xtable(Anova(Fml_S_post), floating = FALSE)
