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
atml <- atcr.cmpr(m3)
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
bxcxplts(value = "toc", data = subsetD(lys, depth == "deep" & post), sval = 0, fval = 1)
# use box-cox lamda

# The initial model

# box-cox 
Iml_D_post <- lmer(toc^(-0.2626) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post)

# log
Iml_D_post <- lmer(log(toc) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post)
# not much difference between the above transformations so use log for
# simplicity purposes

Anova(Iml_D_post, test.statistic = "F")
# not need to remove interaction

# The final model
Fml_D_post <- Iml_D_post
Anova(Fml_D_post)
AnvF_toc_D_post <- Anova(Fml_D_post, test.statistic = "F")
AnvF_toc_D_post

summary(Fml_D_post)

# model diagnosis
plot(Fml_D_post)
qqnorm(resid(Fml_D_post))
qqline(resid(Fml_D_post))

# contrast
# Note that contrast doesn't work with lmer model so use lme
LmeMod <- lme(log(toc) ~ co2 * time, random = ~1|block/ring/id, 
              data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(lys$time[lys$post, drop = TRUE]), co2 = "elev"))
FACE_Lys_TOC_D_postCO2_CntrstDf <- cntrstTbl(cntrst, data = subsetD(lys, depth == "deep" & post), 
                                             variable = "toc", depth = "deep")

FACE_Lys_TOC_D_postCO2_CntrstDf


## ----Stat_FACE_Lys_TOC_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
Anova(Fml_D_pre)

## ----Stat_FACE_Lys_TOC_D_postCO2_Smmry
# The initial model is:
Iml_D_post@call
Anova(Iml_D_post)

# The final model is :
Fml_D_post@call

# Chi-squre test
Anova(Fml_D_post)

# F test
AnvF_toc_D_post

# Contrast 
FACE_Lys_TOC_D_postCO2_CntrstDf
