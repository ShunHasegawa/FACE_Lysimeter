## ----Stat_FACE_Lys_Ammonium_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$nh[lys$depth == "shallow" & lys$pre])

bxplts(value = "nh", data = subsetD(lys, depth == "shallow" & pre))
# log seems slightly fine

# different random factor structure
m1 <- lme(log(nh) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & pre))
m2 <- lme(log(nh) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & pre))
m3 <- lme(log(nh) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & pre))
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
# model5 is best

Iml_S_pre <- atcr.cmpr(m3, rndmFac= "id")[[5]]

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
qqnorm(Fml_S_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_pre))
qqline(residuals.lm(Fml_S_pre))

## ----Stat_FACE_Lys_Ammonium_S_postCO2

############
# Post-CO2 #
############

range(lys$no[lys$depth == "shallow" & lys$post])

bxplts(value = "no", ofst= 0.003, data = subsetD(lys, depth == "shallow" & post))
  # use box cox value

# different random factor structure
m1 <- lme((no + .003)^(.061) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & post))
m2 <- lme((no + .003)^(.061) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & post))
m3 <- lme((no + .003)^(.061) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & post))
anova(m1, m2, m3)
# m1 is better

# autocorrelation
atcr.cmpr(m1, rndmFac= "ring/plot")$models
# model4 is best

Iml_S_post <- atcr.cmpr(m1, rndmFac= "ring/plot")[[4]]

# The initial model is: 
Iml_S_post$call

Anova(Iml_S_post)

# model simplification
MdlSmpl(Iml_S_post)
# CO2:time interactin and co2 are removed

Fml_S_post <- MdlSmpl(Iml_S_post)$model.reml

# The final model is:
Fml_S_post$call

Anova(Fml_S_post)

# model diagnosis
plot(Fml_S_post)
qqnorm(Fml_S_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_post))
qqline(residuals.lm(Fml_S_post))

## ----Stat_FACE_Lys_Ammonium_S_preCO2_Smmry
# The initial model is:
Iml_S_pre$call
Anova(Iml_S_pre)

# The final model is :
Fml_S_pre$call
anova(Fml_S_pre)

## ----Stat_FACE_Lys_Ammonium_S_postCO2_Smmry
# The initial model is:
Iml_S_post$call
Anova(Iml_S_post)

# The final model is :
Fml_S_post$call
Anova(Fml_S_post)
