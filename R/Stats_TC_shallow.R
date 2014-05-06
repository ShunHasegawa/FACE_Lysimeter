## ----Stat_FACE_Lys_TC_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$tc[lys$depth == "shallow" & lys$pre], na.rm = TRUE)
bxcxplts(value = "tc", data = subsetD(lys, depth == "shallow" & pre), sval = 0, fval =1)
bxplts(value = "tc", data = subsetD(lys, depth == "shallow" & pre))
dev.off()

 # log seems fine

# different random factor structure
m1 <- lme(log(tc) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & pre), 
          na.action = "na.omit")
m2 <- lme(log(tc) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & pre),
          na.action = "na.omit")
m3 <- lme(log(tc) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & pre),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# model3 is best

Iml_S_pre <- atml[[3]]

# The initial model is: 
Iml_S_pre$call

Anova(Iml_S_pre)

# model simplification
MdlSmpl(Iml_S_pre)
# error message due to autocorrelation
# how about 2nd best autocorrelation (model 5)
Iml_S_pre <- atml[[5]]

# The initial model is: 
Iml_S_pre$call
Anova(Iml_S_pre)
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
  # not great
## ----Stat_FACE_Lys_TC_S_postCO2

############
# Post-CO2 #
############

range(lys$tc[lys$depth == "shallow" & lys$post], na.rm = TRUE)
bxcxplts(value = "tc", data = subsetD(lys, depth == "shallow" & post), sval = 0, fval =10)
bxplts(value = "tc", data = subsetD(lys, depth == "shallow" & post))
  # log seems better

# different random factor structure
m1 <- lme(log(tc) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & post), 
          na.action = "na.omit")
m2 <- lme(log(tc) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & post),
          na.action = "na.omit")
m3 <- lme(log(tc) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & post),
          na.action = "na.omit")
anova(m1, m2, m3)
# m3 is better

# autocorrelation
atml <- atcr.cmpr(m3, rndmFac= "id")
atml$models
# model3 is best

Iml_S_post <- atml[[3]]

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

# model diagnosis
plot(Fml_S_post)
qqnorm(Fml_S_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S_post))
qqline(residuals.lm(Fml_S_post))

## ----Stat_FACE_Lys_TC_S_preCO2_Smmry
# The initial model is:
Iml_S_pre$call
Anova(Iml_S_pre)

# The final model is :
Fml_S_pre$call
anova(Fml_S_pre)

## ----Stat_FACE_Lys_TC_S_postCO2_Smmry
# The initial model is:
Iml_S_post$call
Anova(Iml_S_post)

# The final model is :
Fml_S_post$call
Anova(Fml_S_post)
