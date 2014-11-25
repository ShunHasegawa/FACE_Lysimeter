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
atcr.cmpr(m2)$models
  # no need for autocorrelation

Iml_D_pre <- atcr.cmpr(m2)[[1]]

# The initial model is: 
Iml_D_pre$call

Anova(Iml_D_pre)

# model simplification
MdlSmpl(Iml_D_pre)
# co2:time is removed

Fml_D_pre <- MdlSmpl(Iml_D_pre)$model.reml

# The final model is:
Fml_D_pre$call

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
  # use sqrt

# The initial model
Iml_D_post <- lmer(sqrt(nh + .08) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post)

# The final model
Fml_D_post <- stepLmer(Iml_D_post)
Anova(Fml_D_post)
AnvF_nh_D_Post <- Anova(Fml_D_post, test.statistic = "F")
AnvF_nh_D_Post

# model diagnosis
plot(Fml_D_post)
qqnorm(resid(Fml_D_post))
qqline(resid(Fml_D_post))
  # homogeneity of variance is still violated...

############
# contrast #
############
# Note that contrast doesn't work with lmer model so use lme
tempDF <- within(subsetD(lys, depth == "deep" & post), {time  <- relevel(time, "5")})

LmeMod <- lme(sqrt(nh + .08) ~ co2 * time, random = ~1|block/ring/id, 
              data = tempDF, na.action = "na.omit")

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "elev"))

FACE_Lys_NH_D_postCO2_CntrstDf <- cntrstTbl(cntrst, data = subsetD(lys, depth == "deep" & post), 
                                             variable = "nh", depth = "deep")
FACE_Lys_NH_D_postCO2_CntrstDf

## ----Stat_FACE_Lys_Ammonium_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
Anova(Fml_D_pre)

## ----Stat_FACE_Lys_Ammonium_D_postCO2_Smmry
# The initial model is:
Iml_D_post@call
Anova(Iml_D_post)

# The final model is :
Fml_D_post@call

# Chi
Anova(Fml_D_post)

# F
AnvF_nh_D_Post

# contrast
FACE_Lys_NH_D_postCO2_CntrstDf

