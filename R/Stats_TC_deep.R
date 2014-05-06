## ----Stat_FACE_Lys_TC_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$tc[lys$depth == "deep" & lys$pre], na.rm = TRUE)

bxcxplts(value = "tc", data = subsetD(lys, depth == "deep" & pre), sval = 0, fval =1)
bxplts(value = "tc", data = subsetD(lys, depth == "deep" & pre))
# sqrt seems slightly better

# different random factor structure
m1 <- lme(sqrt(tc) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & pre), 
          na.action = "na.omit")
m2 <- lme(sqrt(tc) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & pre),
          na.action = "na.omit")
m3 <- lme(sqrt(tc) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & pre),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# no need for autocorrelation

Iml_D_pre <- atml[[1]]

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

## ----Stat_FACE_Lys_TC_D_postCO2

############
# Post-CO2 #
############

range(lys$tc[lys$depth == "deep" & lys$post], na.rm = TRUE)
bxcxplts(value = "tc", data = subsetD(lys, depth == "deep" & post), sval = 0, fval =1)
bxplts(value = "tc", data = subsetD(lys, depth == "deep" & post))
# use box cox lambda

# different random factor structure
m1 <- lme(tc^(-0.263) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & post), 
          na.action = "na.omit")
m2 <- lme(tc^(-0.263) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & post),
          na.action = "na.omit")
m3 <- lme(tc^(-0.263) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# model4 is best

Iml_D_post <- atml[[4]]

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
FACE_Lys_TC_D_postCO2_CntrstDf <- cntrstTbl(cntrst, data = subsetD(lys, depth == "deep" & post), digit = 2)

FACE_Lys_TC_D_postCO2_CntrstDf

# model diagnosis
plot(Fml_D_post)
#wedge-shaped
qqnorm(Fml_D_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_post))
qqline(residuals.lm(Fml_D_post))
# not too bad actually

## ----Stat_FACE_Lys_TC_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
anova(Fml_D_pre)

## ----Stat_FACE_Lys_TC_D_postCO2_Smmry
# The initial model is:
Iml_D_post$call
Anova(Iml_D_post)

# The final model is :
Fml_D_post$call
Anova(Fml_D_post)

# contrast
print(xtable(FACE_Lys_TC_D_postCO2_CntrstDf, floating = FALSE),
      include.rownames = FALSE)