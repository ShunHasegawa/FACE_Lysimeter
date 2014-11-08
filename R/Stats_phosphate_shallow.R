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

# different random factor structure
m1 <- lme((po)^(1/3) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & pre))
m2 <- lme((po)^(1/3) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & pre))
m3 <- lme((po)^(1/3) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & pre))
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atml <- atcr.cmpr(m3)
atml$models
# no need for correlation

Iml_S_pre <- atml[[1]]

# The initial model is: 
Iml_S_pre$call

Anova(Iml_S_pre)

# model simplification
MdlSmpl(Iml_S_pre)
# co2:time and co2 are removed

Fml_S_pre <- MdlSmpl(Iml_S_pre)$model.reml

# The final model is:
Fml_S_pre$call

Anova(Fml_S_pre)

# model diagnosis
plot(Fml_S_pre)
qqnorm(Fml_S_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_pre))
qqline(residuals.lm(Fml_S_pre))

## ----Stat_FACE_Lys_Phosphate_S_postCO2

############
# Post-CO2 #
############

range(lys$po[lys$depth == "shallow" & lys$post])

bxplts(value = "po", ofst= 0.02, data = subsetD(lys, depth == "shallow" & post))

# remove the higher outlier
boxplot(lys$po[lys$depth == "shallow" & lys$post])
PRmOl <- subset(lys, po < 0.4)
bxplts(value = "po", ofst= 0.03, data = subsetD(PRmOl, depth == "shallow" & post))
# use box-cox


# different random factor structure
m1 <- lme((po + .03)^(-1.2323) ~ co2 * time, random = ~1|ring/plot, data = subsetD(PRmOl, depth == "shallow" & post))
m2 <- lme((po + .03)^(-1.2323) ~ co2 * time, random = ~1|ring, data = subsetD(PRmOl, depth == "shallow" & post))
m3 <- lme((po + .03)^(-1.2323) ~ co2 * time, random = ~1|id, data = subsetD(PRmOl, depth == "shallow" & post))
anova(m1, m2, m3)
# m3 is better

# autocorrelation
atml <- atcr.cmpr(m3)
atml$models
# model4 is best

Iml_S_post <- atml[[4]]

# The initial model is: 
Iml_S_post$call

Anova(Iml_S_post)

# model simplification
MdlSmpl(Iml_S_post)
# CO2:time and CO2 are removed

Fml_S_post <- MdlSmpl(Iml_S_post)$model.reml

# The final model is:
Fml_S_post$call

Anova(Fml_S_post)

AnvF_P_S_Post <- Anova(Fml_S_post, test.statistic = "F")

# model diagnosis
plot(Fml_S_post)
qqnorm(Fml_S_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_post))
qqline(residuals.lm(Fml_S_post))
  # not great....

## ----Stat_FACE_Lys_Phosphate_S_preCO2_Smmry
# The initial model is:
Iml_S_pre$call
Anova(Iml_S_pre)

# The final model is :
Fml_S_pre$call
Anova(Fml_S_pre)

## ----Stat_FACE_Lys_Phosphate_S_postCO2_Smmry
# The initial model is:
Iml_S_post$call
Anova(Iml_S_post)

# The final model is :
Fml_S_post$call

# Chi
Anova(Fml_S_post)

# F
AnvF_P_S_Post