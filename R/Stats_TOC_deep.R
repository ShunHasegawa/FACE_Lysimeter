## ----Stat_FACE_Lys_TOC_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$toc[lys$depth == "deep" & lys$pre], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lys, depth == "deep" & pre))
# log seems better

# different random factor structure
m1 <- lme(log(toc) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & pre), 
          na.action = "na.omit")
m2 <- lme(log(toc) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & pre),
          na.action = "na.omit")
m3 <- lme(log(toc) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & pre),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# model5 (or 4) is best

Iml_D_pre <- atml[[5]]

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

## ----Stat_FACE_Lys_TOC_D_postCO2

############
# Post-CO2 #
############

range(lys$toc[lys$depth == "deep" & lys$post], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lys, depth == "deep" & post))
# log seems better

# different random factor structure
m1 <- lme(log(toc) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & post), 
          na.action = "na.omit")
m2 <- lme(log(toc) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & post),
          na.action = "na.omit")
m3 <- lme(log(toc) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m1 is better

# autocorrelation
atml <- atcr.cmpr(m1, rndmFac= "ring/plot")
atml$models
# model3 is best

Iml_D_post <- atml[[3]]

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

# contrast
cntrst<- contrast(Fml_D_post, 
                  a = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "elev"))
FACE_Lys_TOC_D_postCO2_CntrstDf <- cntrstTbl(cntrst, data = subsetD(lys, depth == "deep" & post), digit = 2)

FACE_Lys_TOC_D_postCO2_CntrstDf

# model diagnosis
plot(Fml_D_post)
  #wedge-shaped
qqnorm(Fml_D_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_post))
qqline(residuals.lm(Fml_D_post))
  # not too bad actually

## ----Stat_FACE_Lys_TOC_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
xtable(Anova(Iml_D_pre), floating = FALSE)

# The final model is :
Fml_D_pre$call
xtable(Anova(Fml_D_pre), floating = FALSE)

## ----Stat_FACE_Lys_TOC_D_postCO2_Smmry
# The initial model is:
Iml_D_post$call
xtable(Anova(Iml_D_post), floating = FALSE)

# The final model is :
Fml_D_post$call
xtable(Anova(Fml_D_post), floating = FALSE)
