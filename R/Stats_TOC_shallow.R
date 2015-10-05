## ----Stat_FACE_Lys_TOC_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$toc[lys$depth == "shallow" & lys$pre], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lys, depth == "shallow" & pre))
  # log seems better

# different random factor structure
Iml_S_pre <- lmer(log(toc) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                  data = subsetD(lys, depth == "shallow" & pre), 
                  na.action = "na.omit")

# The initial model is: 
Iml_S_pre@call

Anova(Iml_S_pre)

# model simplification
Fml_S_pre <- stepLmer(Iml_S_pre, alpha.fixed = .1)

# The final model is:
Fml_S_pre@call

Anova(Fml_S_pre)

# model diagnosis
plot(Fml_S_pre)
qqnorm(residuals(Fml_S_pre))
qqline(residuals(Fml_S_pre))

## ----Stat_FACE_Lys_TOC_S_postCO2

############
# Post-CO2 #
############

range(lys$toc[lys$depth == "shallow" & lys$post], na.rm = TRUE)

bxplts(value = "toc", data = subsetD(lys, depth == "shallow" & post))
bxcxplts(value = "toc", data = subsetD(lys, depth == "shallow" & post), sval = -7.365, fval = -7.364)
bxcxplts(value = "toc", data = subsetD(lys, depth == "shallow" & post), sval = 0, fval = 1, 
         lambda = seq(-5, 5, 0.01))
  # log seems better

# The initial model
Iml_S_post <- lmer(toc^(-0.2626) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "shallow" & post), na.action = "na.omit")
Anova(Iml_S_post)

# model simplification
Fml_S_post <- stepLmer(Iml_S_post)
Fml_S_post@call

Anova(Fml_S_post)
AnvF_toc_S_post <- Anova(Fml_S_post, test.statistic = "F")
AnvF_toc_S_post

# model diagnosis
plot(Fml_S_post)
qqnorm(residuals(Fml_S_post))
qqline(residuals(Fml_S_post))
  # not great...


## ----Stat_FACE_Lys_TOC_S_preCO2_Smmry
# The initial model is:
Iml_S_pre@call
Anova(Iml_S_pre)

# The final model is :
Fml_S_pre@call
Anova(Fml_S_pre)

## ----Stat_FACE_Lys_TOC_S_postCO2_Smmry
# The initial model is:
Iml_S_post@call
Anova(Iml_S_post)

# The final model is :
Fml_S_post@call

# Chi-square
Anova(Fml_S_post)

# F test
AnvF_toc_S_post
