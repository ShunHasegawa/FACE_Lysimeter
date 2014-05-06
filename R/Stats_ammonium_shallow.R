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

range(lys$nh[lys$depth == "shallow" & lys$post])

bxplts(value = "nh", ofst= 0.06, data = subsetD(lys, depth == "shallow" & post))

# remove the higher outlier
boxplot(lys$nh[lys$depth == "shallow" & lys$post])
NhRmOl <- subset(lys, nh < max(nh))
bxplts(value = "nh", ofst= 0.06, data = subsetD(NhRmOl, depth == "shallow" & post))
bxplts(value = "nh", ofst= 0.07, data = subsetD(NhRmOl, depth == "shallow" & post))
 # use box-cox


# different random factor structure
m1 <- lme((nh + .07)^(-0.34) ~ co2 * time, random = ~1|ring/plot, data = subsetD(NhRmOl, depth == "shallow" & post))
m2 <- lme((nh + .07)^(-0.34) ~ co2 * time, random = ~1|ring, data = subsetD(NhRmOl, depth == "shallow" & post))
m3 <- lme((nh + .07)^(-0.34) ~ co2 * time, random = ~1|id, data = subsetD(NhRmOl, depth == "shallow" & post))
anova(m1, m2, m3)
# m3 is better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
# model3 is best

Iml_S_post <- atcr.cmpr(m3, rndmFac= "id")[[3]]

# The initial model is: 
Iml_S_post$call

Anova(Iml_S_post)

# model simplification
MdlSmpl(Iml_S_post)
  # no factor is removed

Fml_S_post <- MdlSmpl(Iml_S_post)$model.reml

# The final model is:
Fml_S_post$call

Anova(Fml_S_post)

# model diagnosis
plot(Fml_S_post)
  # wedge-shaped
qqnorm(Fml_S_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_post))
qqline(residuals.lm(Fml_S_post))
  # not great....

# Contrast
cntrst<- contrast(Fml_S_post, 
                  a = list(time = levels(NhRmOl$time[NhRmOl$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(NhRmOl$time[NhRmOl$post, drop = TRUE]), co2 = "elev"))
FACE_Lys_NH_S_postCO2_CntrstDf <- cntrstTbl(cntrst, data = subsetD(NhRmOl, depth == "shallow" & post), digit = 2)

FACE_Lys_NH_S_postCO2_CntrstDf


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

print(xtable(FACE_Lys_NH_S_postCO2_CntrstDf, floating = FALSE), 
      include.rawnames = FALSE)
