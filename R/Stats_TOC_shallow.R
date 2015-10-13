## ----Stat_FACE_Lys_TOC_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$toc[lys$depth == "shallow" & lys$pre], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lys, depth == "shallow" & pre))
  # log seems better

# different random factor structure
Iml_S_pre_toc <- lmer(log(toc) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                  data = subsetD(lys, depth == "shallow" & pre), 
                  na.action = "na.omit")

# The initial model is: 
Iml_S_pre_toc@call

Anova(Iml_S_pre_toc)
Anova(Iml_S_pre_toc, test.statistic = "F")

# model simplification
Fml_S_pre_toc <- stepLmer(Iml_S_pre_toc, alpha.fixed = .1)

# The final model is:
Fml_S_pre_toc@call

Anova(Fml_S_pre_toc)

# model diagnosis
plot(Fml_S_pre_toc)
qqnorm(residuals(Fml_S_pre_toc))
qqline(residuals(Fml_S_pre_toc))

## ----Stat_FACE_Lys_TOC_S_postCO2

############
# Post-CO2 #
############

range(lys$toc[lys$depth == "shallow" & lys$post], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lys, depth == "shallow" & post))
bxcxplts(value = "toc", data = subsetD(lys, depth == "shallow" & post), sval = -7.365, fval = -7.364)
bxcxplts(value = "toc", data = subsetD(lys, depth == "shallow" & post), sval = 0, fval = 1, 
         lambda = seq(-5, 5, 0.01))

# The initial model
Iml_S_post_toc <- lmer(toc^(1/3) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "shallow" & post), na.action = "na.omit")
Anova(Iml_S_post_toc)

# model simplification
Fml_S_post_toc <- stepLmer(Iml_S_post_toc)
Fml_S_post_toc@call

Anova(Fml_S_post_toc)
AnvF_toc_S_post <- Anova(Fml_S_post_toc, test.statistic = "F")
AnvF_toc_S_post

# model diagnosis
plot(Fml_S_post_toc)
qqnorm(residuals(Fml_S_post_toc))
qqline(residuals(Fml_S_post_toc))
  # not great...


## ----Stat_FACE_Lys_TOC_S_preCO2_Smmry
# The initial model is:
Iml_S_pre_toc@call
Anova(Iml_S_pre_toc, test.statistic = "F")

# The final model is :
Fml_S_pre_toc@call
Anova(Fml_S_pre_toc, test.statistic = "F")

## ----Stat_FACE_Lys_TOC_S_postCO2_Smmry
# The initial model is:
Iml_S_post_toc@call
Anova(Iml_S_post_toc)

# The final model is :
Fml_S_post_toc@call

# Chi-square
Anova(Fml_S_post_toc)

# F test
AnvF_toc_S_post
