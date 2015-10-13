## ----Stat_FACE_Lys_Nitrate_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$no[lys$depth == "shallow" & lys$pre])

bxplts(value = "no", data = subsetD(lys, depth == "shallow" & pre))

# log seems slightly better
Iml_S_pre_no <-  lmer(log(no) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "shallow" & pre), na.action = "na.omit")

# The initial model is: 
Iml_S_pre_no@call

Anova(Iml_S_pre_no, test.statistic = "F")

Fml_S_pre_no <- stepLmer(Iml_S_pre_no, alpha.fixed = .1)

# The final model is:
Fml_S_pre_no@call

anova(Fml_S_pre_no)

# model diagnosis
plot(Fml_S_pre_no)
qqnorm(residuals(Fml_S_pre_no))
qqline(residuals(Fml_S_pre_no))
# not very great though

## ----Stat_FACE_Lys_Nitrate_S_postCO2

###########
# Post-CO2 #
###########

range(lys$no[lys$depth == "shallow" & lys$post])

bxplts(value = "no", ofst= 0.003, data = subsetD(lys, depth == "shallow" & post))
bxplts(value = "no", ofst= 0.01, data = subsetD(lys, depth == "shallow" & post))
# log seems slightly better

# The initial model is
Iml_S_post_no <- lmer(log(no + .01) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(lys, depth == "shallow" & post), na.action = "na.omit")
Anova(Iml_S_post_no)

# The final model
Fml_S_post_no <- stepLmer(Iml_S_post_no, alpha.fixed = .1)
Anova(Fml_S_post_no)
AnvF_Nit_S_Post <- Anova(Fml_S_post_no, test.statistic = "F")
AnvF_Nit_S_Post

summary(Fml_S_post_no)

# model diagnosis
plot(Fml_S_post_no)
qqnorm(resid(Fml_S_post_no))
qqline(resid(Fml_S_post_no))

## ----Stat_FACE_Lys_Nitrate_S_preCO2_Smmry
# The initial model is:
Iml_S_pre_no@call
Anova(Iml_S_pre_no, test.statistic = "F")

# The final model is :
Fml_S_pre_no@call
anova(Fml_S_pre_no)

## ----Stat_FACE_Lys_Nitrate_S_postCO2_Smmry
# The initial model is:
Iml_S_post_no@call
Anova(Iml_S_post_no, test.statistic = "F")

# The final model is :
Fml_S_post_no@call

# Chi
Anova(Fml_S_post_no)

# F
AnvF_Nit_S_Post