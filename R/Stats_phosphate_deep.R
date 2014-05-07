## ----Stat_FACE_Lys_Phosphate_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$po[lys$depth == "deep" & lys$pre])

bxplts(value = "po", ofst= 0.001, data = subsetD(lys, depth == "deep" & pre))
 # sqrt looks better

# different random factor structure
m1 <- lme(sqrt(po + .001) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & pre))
m2 <- lme(sqrt(po + .001) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & pre))
m3 <- lme(sqrt(po + .001) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & pre))
anova(m1, m2, m3)
# m1 is slightly better

# autocorrelation
atml <- atcr.cmpr(m1, rndmFac= "ring/plot")
atml$models
# model5 or 4 is best

Iml_D_pre <- atml[[5]]

# The initial model is: 
Iml_D_pre$call

Anova(Iml_D_pre)

# model simplification
MdlSmpl(Iml_D_pre)
  # co2:time and co2 are removed

Fml_D_pre <- MdlSmpl(Iml_D_pre)$model.reml

# The final model is:
Fml_D_pre$call

Anova(Fml_D_pre)

# model diagnosis
plot(Fml_D_pre)
qqnorm(Fml_D_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_pre))
qqline(residuals.lm(Fml_D_pre))

## ----Stat_FACE_Lys_Phosphate_D_postCO2

############
# Post-CO2 #
############

range(lys$po[lys$depth == "deep" & lys$post])

bxplts(value = "po", ofst= 0.007, data = subsetD(lys, depth == "deep" & post))

#  remove hihger outlier
boxplot(lys$po[lys$depth == "deep" & lys$post])
PoRmOl <- subset(lys, po < 0.08)

bxplts(value = "po", ofst= 0.007, data = subsetD(PoRmOl, depth == "deep" & post))
  #row looks slightly better

# different random factor structure
m1 <- lme(po ~ co2 * time, random = ~1|ring/plot, data = subsetD(PoRmOl, depth == "deep" & post))
m2 <- lme(po ~ co2 * time, random = ~1|ring, data = subsetD(PoRmOl, depth == "deep" & post))
m3 <- lme(po ~ co2 * time, random = ~1|id, data = subsetD(PoRmOl, depth == "deep" & post))
anova(m1, m2, m3)
# m2 is better

# autocorrelation
atml <- atcr.cmpr(m2, rndmFac= "ring/plot")
atml$models
# no need for autocorrelation

Iml_D_post <- atml[[1]]

# The initial model is: 
Iml_D_post$call

Anova(Iml_D_post)

# model simplification
MdlSmpl(Iml_D_post)
# no factor is removed

Fml_D_post <- MdlSmpl(Iml_D_post)$model.reml

# The final model is:
Fml_D_post$call

Anova(Fml_D_post)

# model diagnosis
plot(Fml_D_post)
  # wedge-shaped
qqnorm(Fml_D_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_post))
qqline(residuals.lm(Fml_D_post))
  # not great....

## ----Stat_FACE_Lys_Phosphate_D_preCO2_Dmmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
Anova(Fml_D_pre)

## ----Stat_FACE_Lys_Phosphate_D_postCO2_Dmmry
# The initial model is:
Iml_D_post$call
Anova(Iml_D_post)

# The final model is :
Fml_D_post$call
Anova(Fml_D_post)
