## ----Stat_FACE_Lys_Phosphate_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$po[lys$depth == "shallow" & lys$pre])

bxplts(value = "po", data = subsetD(lys, depth == "shallow" & pre))

# remove higher outlier
boxplot(lys$po[lys$depth == "shallow" & lys$pre])
PRmOl <- subset(lys, po < 0.4)
bxplts(value = "po", data = subsetD(PRmOl, depth == "shallow" & pre))
# poiwer(1/3) seems slightly fine

Iml_S_pre_p <- lmer(po^(1/3) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                  data = subsetD(PRmOl, depth == "shallow" & pre), na.action = "na.omit")

# The initial model is: 
Iml_S_pre_p@call

Anova(Iml_S_pre_p, test.statistic = "F")

Fml_S_pre_p <- stepLmer(Iml_S_pre_p, alpha.fixed = .1)

# The final model is:
Fml_S_pre_p@call

Anova(Fml_S_pre_p, test.statistic = "F")

# model diagnosis
plot(Fml_S_pre_p)
qqnorm(residuals(Fml_S_pre_p))
qqline(residuals(Fml_S_pre_p))

## ----Stat_FACE_Lys_Phosphate_S_postCO2

############
# Post-CO2 #
############

range(lys$po[lys$depth == "shallow" & lys$post])

bxplts(value = "po", ofst= 0.02, data = subsetD(lys, depth == "shallow" & post))

# remove the higher outlier
boxplot(lys$po[lys$depth == "shallow" & lys$post])
PRmOl <- subset(lys, po < 0.2)
bxplts(value = "po", ofst= 0.03, data = subsetD(PRmOl, depth == "shallow" & post))
# use log

# The initial model is
Iml_S_post_p <- lmer(log(po + .03) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(PRmOl, depth == "shallow" & post), na.action = "na.omit")
Anova(Iml_S_post_p)

# The final model
Fml_S_post_p <- stepLmer(Iml_S_post_p, alpha.fixed = .1)
Anova(Fml_S_post_p)
AnvF_P_S_Post <- Anova(Fml_S_post_p, test.statistic = "F")
AnvF_P_S_Post

summary(Fml_S_post_p)

# model diagnosis
plot(Fml_S_post_p)
qqnorm(resid(Fml_S_post_p))
qqline(resid(Fml_S_post_p))

## ----Stat_FACE_Lys_Phosphate_S_preCO2_Smmry
# The initial model is:
Iml_S_pre_p@call
Anova(Iml_S_pre_p, test.statistic = "F")

# The final model is :
Fml_S_pre_p@call
Anova(Fml_S_pre_p, test.statistic = "F")

## ----Stat_FACE_Lys_Phosphate_S_postCO2_Smmry
# The initial model is:
Iml_S_post_p@call
Anova(Iml_S_post_p, test.statistic = "F")

# The final model is :
Fml_S_post_p@call

# Chi
Anova(Fml_S_post_p)

# F
AnvF_P_S_Post
