## ----Stat_FACE_Lys_Phosphate_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$po[lys$depth == "deep" & lys$pre])

bxplts(value = "po", ofst= 0.001, data = subsetD(lys, depth == "deep" & pre))
 # sqrt looks better

Iml_D_pre_p <- lmer(sqrt(po + .001) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                  data = subsetD(lys, depth == "deep" & pre), na.action = "na.omit")

# The initial model is: 
Iml_D_pre_p@call

Anova(Iml_D_pre_p, test.statistic = "F")

Fml_D_pre_p <- stepLmer(Iml_D_pre_p, alpha.fixed = .1)

# The final model is:
Fml_D_pre_p@call

Anova(Fml_D_pre_p, test.statistic = "F")

# model diagnosis
plot(Fml_D_pre_p)
qqnorm(residuals(Fml_D_pre_p))
qqline(residuals(Fml_D_pre_p))

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
Iml_D_post_p <- lmer(po ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(PoRmOl, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post_p)

# The final model
Fml_D_post_p <- stepLmer(Iml_D_post_p, alpha.fixed = .1)
Anova(Fml_D_post_p)
AnvF_P_D_Post <- Anova(Fml_D_post_p, test.statistic = "F")
AnvF_P_D_Post

summary(Fml_D_post_p)

# model diagnosis
plot(Fml_D_post_p)
qqnorm(resid(Fml_D_post_p))
qqline(resid(Fml_D_post_p))

############
# Contrast #
############
# Note that contrast doesn't work with lmer model so use lme
df <- PoRmOl

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
Iml_D_pre_p@call
Anova(Iml_D_pre_p, test.statistic = "F")

# The final model is :
Fml_D_pre_p@call
Anova(Fml_D_pre_p, test.statistic = "F")

## ----Stat_FACE_Lys_Phosphate_D_postCO2_Smmry
# The initial model is:
Iml_D_post_p@call
Anova(Iml_D_post_p, test.statistic = "F")

# The final model is :
Fml_D_post_p@call

# Chi
Anova(Fml_D_post_p)

# F
AnvF_P_D_Post

# COntrast
FACE_Lys_P_D_postCO2_CntrstDf
