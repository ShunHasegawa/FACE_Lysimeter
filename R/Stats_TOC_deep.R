## ----Stat_FACE_Lys_TOC_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$toc[lys$depth == "deep" & lys$pre], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lys, depth == "deep" & pre))
# log seems better

Iml_D_pre_toc <- lmer(log(toc) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                  data = subsetD(lys, depth == "deep" & pre), 
                  na.action = "na.omit")

# The initial model is: 
Iml_D_pre_toc@call

Anova(Iml_D_pre_toc)
Anova(Iml_D_pre_toc, test.statistic = "F")

# model simplification
Fml_D_pre_toc <- stepLmer(Iml_D_pre_toc, alpha.fixed = .1)

# The final model is:
Fml_D_pre_toc@call

Anova(Fml_D_pre_toc)
Anova(Fml_D_pre_toc, test.statistic = "F")

# model diagnosis
plot(Fml_D_pre_toc)
qqnorm(residuals(Fml_D_pre_toc))
qqline(residuals(Fml_D_pre_toc))

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
Iml_D_post_toc <- lmer(toc^(-0.2626) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post_toc)

# log
Iml_D_post_toc <- lmer(log(toc) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post_toc)
# not much difference between the above transformations so use log for
# simplicity purposes

Anova(Iml_D_post_toc, test.statistic = "F")
# not need to remove interaction

# The final model
Fml_D_post_toc <- Iml_D_post_toc
Anova(Fml_D_post_toc)
AnvF_toc_D_post <- Anova(Fml_D_post_toc, test.statistic = "F")
AnvF_toc_D_post

summary(Fml_D_post_toc)

# model diagnosis
plot(Fml_D_post_toc)
qqnorm(resid(Fml_D_post_toc))
qqline(resid(Fml_D_post_toc))

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
Iml_D_pre_toc@call
Anova(Iml_D_pre_toc)

# The final model is :
Fml_D_pre_toc@call
Anova(Fml_D_pre_toc)

## ----Stat_FACE_Lys_TOC_D_postCO2_Smmry
# The initial model is:
Iml_D_post_toc@call
Anova(Iml_D_post_toc, test.statistic = "F")

# The final model is :
Fml_D_post_toc@call

# Chi-squre test
Anova(Fml_D_post_toc)

# F test
AnvF_toc_D_post

# Contrast 
FACE_Lys_TOC_D_postCO2_CntrstDf
