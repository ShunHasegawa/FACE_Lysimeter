## ----Stat_FACE_Lys_Ammonium_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$nh[lys$depth == "deep" & lys$pre])
bxplts(value = "nh", data = subsetD(lys, depth == "deep" & pre))
  # log seems fine

# different random factor structure
m1 <- lme(log(nh) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & pre))
m2 <- lme(log(nh) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & pre))
m3 <- lme(log(nh) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & pre))
anova(m1, m2, m3)
# m2 is slightly better

# autocorrelation
atcr.cmpr(m2, rndmFac= "ring")$models
  # no need for autocorrelation

Iml_D_pre <- atcr.cmpr(m2, rndmFac= "ring")[[1]]

# The initial model is: 
Iml_D_pre$call

Anova(Iml_D_pre)

# model simplification
MdlSmpl(Iml_D_pre)
# co2:time is removed

Fml_D_pre <- MdlSmpl(Iml_D_pre)$model.reml

# The final model is:
Fml_D_pre$call

Anova(Fml_D_pre)

# model diagnosis
plot(Fml_D_pre)
qqnorm(Fml_D_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_pre))
qqline(residuals.lm(Fml_D_pre))

## ----Stat_FACE_Lys_Ammonium_D_postCO2

############
# Post-CO2 #
############

range(lys$nh[lys$depth == "deep" & lys$post])
bxplts(value = "nh", ofst=0.08, data = subsetD(lys, depth == "deep" & post))
  # box-cox

# different random factor structure
m1 <- lme((nh + .08)^(.424) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & post))
m2 <- lme((nh + .08)^(.424) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & post))
m3 <- lme((nh + .08)^(.424) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & post))
anova(m1, m2, m3)
# m2 is better

# autocorrelation
atcr.cmpr(m2, rndmFac= "ring")$models
# no need for autocorrelation

Iml_D_post <- atcr.cmpr(m1, rndmFac= "ring/plot")[[1]]

# The initial model is: 
Iml_D_post$call

Anova(Iml_D_post)

# model simplification
MdlSmpl(Iml_D_post)
# no factor was removed

Fml_D_post <- MdlSmpl(Iml_D_post)$model.reml

# The final model is:
Fml_D_post$call

Anova(Fml_D_post)

# contrast
# Contrast
cntrst<- contrast(Fml_D_post, 
                  a = list(time = levels(NhRmOl$time[lys$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(NhRmOl$time[lys$post, drop = TRUE]), co2 = "elev"))
FACE_Lys_NH_D_postCO2_CntrstDf <- cntrstTbl(cntrst, data = subsetD(lys, depth == "shallow" & post), digit = 2)

FACE_Lys_NH_D_postCO2_CntrstDf

# model diagnosis
plot(Fml_D_post)
qqnorm(Fml_D_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_post))
qqline(residuals.lm(Fml_D_post))
  # homogeneity of variance is still violated...

## ----Stat_FACE_Lys_Ammonium_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
xtable(Anova(Iml_D_pre), floating = FALSE)

# The final model is :
Fml_D_pre$call
xtable(Anova(Fml_D_pre), floating = FALSE)

## ----Stat_FACE_Lys_Ammonium_D_postCO2_Smmry
# The initial model is:
Iml_D_post$call
xtable(Anova(Iml_D_post), floating = FALSE)

# The final model is :
Fml_D_post$call
xtable(Anova(Fml_D_post), floating = FALSE)

# contrast
print(xtable(FACE_Lys_NH_D_postCO2_CntrstDf, 
             floating = FALSE, 
             caption = "Contrast"), 
      include.rawnames = FALSE
      )

