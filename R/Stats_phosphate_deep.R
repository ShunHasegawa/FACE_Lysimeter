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
atml <- atcr.cmpr(m1)
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
  #raw looks slightly better

# The initial model is
Iml_D_post <- lmer(po ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(PoRmOl, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post)

# The final model
Fml_D_post <- stepLmer(Iml_D_post)
Anova(Fml_D_post)
AnvF_P_D_Post <- Anova(Fml_D_post, test.statistic = "F")
AnvF_P_D_Post

summary(Fml_D_post)

# model diagnosis
plot(Fml_D_post)
qqnorm(resid(Fml_D_post))
qqline(resid(Fml_D_post))

############
# Contrast #
############
# Note that contrast doesn't work with lmer model so use lme
df <- within(PoRmOl, {time <- relevel(time, "4")})

LmeMod <- lme(po ~ co2 * time, random = ~1|block/ring/id, 
              data = subsetD(df, depth == "deep" & post),
              na.action = "na.omit")

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "elev"))
FACE_Lys_P_D_postCO2_CntrstDf <- cntrstTbl(cntrst, data = subsetD(lys, depth == "deep" & post), 
                                           variable = "po", depth = "deep")
FACE_Lys_P_D_postCO2_CntrstDf

## ----Stat_FACE_Lys_Phosphate_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
Anova(Fml_D_pre)

## ----Stat_FACE_Lys_Phosphate_D_postCO2_Smmry
# The initial model is:
Iml_D_post@call
Anova(Iml_D_post)

# The final model is :
Fml_D_post@call

# Chi
Anova(Fml_D_post)

# F
AnvF_P_D_Post

# COntrast
FACE_Lys_P_D_postCO2_CntrstDf