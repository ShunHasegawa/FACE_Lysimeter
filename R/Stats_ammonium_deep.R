## ----Stat_FACE_Lys_Ammonium_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$nh[lys$depth == "deep" & lys$pre])
bxplts(value = "nh", data = subsetD(lys, depth == "deep" & pre))
  # log seems fine

Iml_D_pre_nh <- lmer(log(nh) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                  data = subsetD(lys, depth == "deep" & pre), 
                  na.action = "na.omit")

# The initial model is: 
Iml_D_pre_nh@call

Anova(Iml_D_pre_nh)
Anova(Iml_D_pre_nh, test.statistic = "F")

Fml_D_pre_nh <- stepLmer(Iml_D_pre_nh, alpha.fixed = .1)

# The final model is:
Fml_D_pre_nh@call
Anova(Fml_D_pre_nh, test.statistic = "F")

# model diagnosis
plot(Fml_D_pre_nh)
qqnorm(residuals(Fml_D_pre_nh))
qqline(residuals(Fml_D_pre_nh))

## ----Stat_FACE_Lys_Ammonium_D_postCO2

############
# Post-CO2 #
############
range(lys$nh[lys$depth == "deep" & lys$post])
bxplts(value = "nh", ofst=0.08, data = subsetD(lys, depth == "deep" & post))
  # use sqrt

# The initial model
Iml_D_post_nh <- lmer(sqrt(nh + .08) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post_nh)

# The final model
Fml_D_post_nh <- stepLmer(Iml_D_post_nh)
Anova(Fml_D_post_nh)
AnvF_nh_D_Post <- Anova(Fml_D_post_nh, test.statistic = "F")
AnvF_nh_D_Post

# model diagnosis
plot(Fml_D_post_nh)
qqnorm(resid(Fml_D_post_nh))
qqline(resid(Fml_D_post_nh))
  # homogeneity of variance is still violated...

############
# contrast #
############
# Note that contrast doesn't work with lmer model so use lme
tempDF <- within(subsetD(lys, depth == "deep" & post), {
  time  <- relevel(time, "6")
  co2 <- relevel(co2, "elev")
  })

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
Iml_D_pre_nh@call
Anova(Iml_D_pre_nh, test.statistic = "F")

# The final model is :
Fml_D_pre_nh@call
Anova(Fml_D_pre_nh, test.statistic = "F")

## ----Stat_FACE_Lys_Ammonium_D_postCO2_Smmry
# The initial model is:
Iml_D_post_nh@call
Anova(Iml_D_post_nh)

# The final model is :
Fml_D_post_nh@call

# Chi
Anova(Fml_D_post_nh)

# F
AnvF_nh_D_Post

# contrast
FACE_Lys_NH_D_postCO2_CntrstDf
