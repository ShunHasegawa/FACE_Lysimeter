## ----Stat_FACE_Lys_Nitrate_D_preCO2
###########
# Pre-CO2 #
###########

range(lys$no[lys$depth == "deep" & lys$pre])

bxplts(value = "no", data = subsetD(lys, depth == "deep" & pre))
  # sqrt looks silghtly better

Iml_D_pre_no <- lmer(sqrt(no) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & pre), na.action = "na.omit")

# The initial model is: 
Iml_D_pre_no@call

Anova(Iml_D_pre_no, test.statistic = "F")

Fml_D_pre_no <- stepLmer(Iml_D_pre_no, alpha.fixed = .1)

# The final model is:
Fml_D_pre_no@call

anova(Fml_D_pre_no)

# model diagnosis
plot(Fml_D_pre_no)
qqnorm(residuals(Fml_D_pre_no))
qqline(residuals(Fml_D_pre_no))
  # not very great

## ----Stat_FACE_Lys_Nitrate_D_postCO2

###########
# Post-CO2 #
###########

range(lys$no[lys$depth == "deep" & lys$post])

bxplts(value = "no", data = subsetD(lys, depth == "deep" & post))
  # log looks slightly better

Iml_D_post_no <- lmer(log(no) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "deep" & post), na.action = "na.omit")
Anova(Iml_D_post_no)

# The final model
Fml_D_post_no <- stepLmer(Iml_D_post_no, alpha.fixed = .1)
Anova(Fml_D_post_no)
AnvF_Nit_D_Post <- Anova(Fml_D_post_no, test.statistic = "F")
AnvF_Nit_D_Post

summary(Fml_D_post_no)

# model diagnosis
plot(Fml_D_post_no)
qqnorm(resid(Fml_D_post_no))
qqline(resid(Fml_D_post_no))

## ----Stat_FACE_Lys_Nitrate_D_preCO2_Smmry
# The initial model is:
Iml_D_pre_no@call
Anova(Iml_D_pre_no, test.statistic = "F")

# The final model is :
Fml_D_pre_no@call
anova(Fml_D_pre_no)

## ----Stat_FACE_Lys_Nitrate_D_postCO2_Smmry
# The initial model is:
Iml_D_post_no@call
Anova(Iml_D_post_no, test.statistic = "F")

# The final model is :
Fml_D_post_no@call

# Chi
Anova(Fml_D_post_no)

# F
AnvF_Nit_D_Post
