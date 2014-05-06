## ----Stat_FACE_Lys_Nitrrate_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$no[lys$depth == "deep" & lys$pre])

bxplts(value = "no", data = subsetD(lys, depth == "deep" & pre))
  # sqrt looks silghtly better

# different random factor structure
m1 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & pre))
m2 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & pre))
m3 <- lme(sqrt(no) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & pre))
anova(m1, m2, m3)
# m1 is sl better

# autocorrelation
atcr.cmpr(m1, rndmFac= "ring/plot")$models
# model5 is best

Iml_D_pre <- atcr.cmpr(m1, rndmFac= "ring/plot")[[5]]

# The initial model is: 
Iml_D_pre$call

Anova(Iml_D_pre)

# model simplification
MdlSmpl(Iml_D_pre)
# all factors are removed

Fml_D_pre <- MdlSmpl(Iml_D_pre)$model.reml

# The final model is:
Fml_D_pre$call

anova(Fml_D_pre)

# model diagnosis
plot(Fml_D_pre)
qqnorm(Fml_D_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_pre))
qqline(residuals.lm(Fml_D_pre))
  # not very great


## ----Stat_FACE_Lys_Nitrrate_D_postCO2

###########
# Post-CO2 #
###########

range(lys$no[lys$depth == "deep" & lys$post])

bxplts(value = "no", data = subsetD(lys, depth == "deep" & post))
  # log looks slightly better

# different random factor structure
m1 <- lme(log(no) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & post))
m2 <- lme(log(no) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & post))
m3 <- lme(log(no) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & post))
anova(m1, m2, m3)
# m1 is better

# autocorrelation
atcr.cmpr(m1, rndmFac= "ring/plot")$models
# model4 is best

Iml_D_post <- atcr.cmpr(m1, rndmFac= "ring/plot")[[4]]

# The initial model is: 
Iml_D_post$call

Anova(Iml_D_post)

# model simplification
MdlSmpl(Iml_D_post)
  # CO2:time interactin and co2 are removed

Fml_D_post <- MdlSmpl(Iml_D_post)$model.reml

# The final model is:
Fml_D_post$call

Anova(Fml_D_post)

# model diagnosis
plot(Fml_D_post)
qqnorm(Fml_D_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_post))
qqline(residuals.lm(Fml_D_post))

## ----Stat_FACE_Lys_Nitrrate_S_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
anova(Fml_D_pre)

## ----Stat_FACE_Lys_Nitrrate_S_postCO2_Smmry
# The initial model is:
Iml_D_post$call
Anova(Iml_D_post)

# The final model is :
Fml_D_post$call
Anova(Fml_D_post)
