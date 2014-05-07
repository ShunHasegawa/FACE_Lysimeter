## ----Stat_FACE_Lys_TN_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$tn[lys$depth == "shallow" & lys$pre], na.rm = TRUE)
bxcxplts(value = "tn", data = subsetD(lys, depth == "shallow" & pre), sval = 0, fval =1)
bxcxplts(value = "tn", data = subsetD(lys, depth == "shallow" & pre), sval = 5, fval =7)
bxplts(value = "tn", ofst= 7, data = subsetD(lys, depth == "shallow" & pre))
  # inverse seems better

# different random factor structure
m1 <- lme(1/(tn+7) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & pre), 
          na.action = "na.omit")
m2 <- lme(1/(tn+7) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & pre),
          na.action = "na.omit")
m3 <- lme(1/(tn+7) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & pre),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# model5 is best

Iml_S_pre <- atml[[5]]

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
  # slight linear patter remains..
qqnorm(Fml_S_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_pre))
qqline(residuals.lm(Fml_S_pre))

## ----Stat_FACE_Lys_TN_S_postCO2

############
# Post-CO2 #
############
range(lys$tn[lys$depth == "shallow" & lys$post], na.rm = TRUE)

bxcxplts(value = "tn", data = subsetD(lys, depth == "shallow" & post), sval = 0.1, fval =10)
bxcxplts(value = "tn", data = subsetD(lys, depth == "shallow" & post), sval = 8, fval =12)
# adding 9 may improve 

bxplts(value = "tn", ofst=9, data = subsetD(lys, depth == "shallow" & post))
# use box-cox lambda

# different random factor structure
m1 <- lme((tn+9)^(-1.8788) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & post), 
          na.action = "na.omit")
m2 <- lme((tn+9)^(-1.8788) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & post),
          na.action = "na.omit")
m3 <- lme((tn+9)^(-1.8788) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m1 is better

# autocorrelation
atml <- atcr.cmpr(m1, rndmFac= "ring/plot")
atml$models
# model4 is best

Iml_S_post <- atml[[4]]

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
qqnorm(Fml_S_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_post))
qqline(residuals.lm(Fml_S_post))

## ----Stat_FACE_Lys_TN_S_preCO2_Smmry
# The initial model is:
Iml_S_pre$call
Anova(Iml_S_pre)

# The final model is :
Fml_S_pre$call
Anova(Fml_S_pre)

## ----Stat_FACE_Lys_TN_S_postCO2_Smmry
# The initial model is:
Iml_S_post$call
Anova(Iml_S_post)

# The final model is :
Fml_S_post$call
Anova(Fml_S_post)
