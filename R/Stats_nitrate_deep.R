## ----Stat_FACE_Lys_Nitrate_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$no[lys$depth == "deep" & lys$pre])

bxplts(value = "no", data = subsetD(lys, depth == "deep" & pre))
  # sqrt looks silghtly better

# different random factor structure
m1 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "deep" & pre))
m2 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "deep" & pre))
m3 <- lme(sqrt(no) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "deep" & pre))
anova(m1, m2, m3)
# m1 is sl better

# autocorrelation
atcr.cmpr(m1)$models
# model5 is best

Iml_D_pre <- atcr.cmpr(m1)[[5]]

# The initial model is: 
Iml_D_pre$call

Anova(Iml_D_pre)

# model simplification
MdlSmpl(Iml_D_pre)
# all factors are removed

Fml_D_pre <- MdlSmpl(Iml_D_pre)$model.reml

# The final model is:
Fml_D_pre$call

anova(Fml_D_pre)

# model diagnosis
plot(Fml_D_pre)
qqnorm(Fml_D_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D_pre))
qqline(residuals.lm(Fml_D_pre))
  # not very great


## ----Stat_FACE_Lys_Nitrate_D_postCO2

###########
# Post-CO2 #
###########

range(lys$no[lys$depth == "deep" & lys$post])

bxplts(value = "no", data = subsetD(lys, depth == "deep" & post))
  # log looks slightly better

Iml_D_post <- lmer(log(no) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post)

# The final model
Fml_D_post <- stepLmer(Iml_D_post)
Anova(Fml_D_post)
AnvF_Nit_D_Post <- Anova(Fml_D_post, test.statistic = "F")
AnvF_Nit_D_Post

summary(Fml_D_post)

# model diagnosis
plot(Fml_D_post)
qqnorm(resid(Fml_D_post))
qqline(resid(Fml_D_post))

## ----Stat_FACE_Lys_Nitrate_D_preCO2_Smmry
# The initial model is:
Iml_D_pre$call
Anova(Iml_D_pre)

# The final model is :
Fml_D_pre$call
anova(Fml_D_pre)

## ----Stat_FACE_Lys_Nitrate_D_postCO2_Smmry
# The initial model is:
Iml_D_post@call
Anova(Iml_D_post)

# The final model is :
Fml_D_post@call

# Chi
Anova(Fml_D_post)

# F
AnvF_Nit_D_Post
