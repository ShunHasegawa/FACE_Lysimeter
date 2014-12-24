## ----Stat_FACE_Lys_Ammonium_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$nh[lys$depth == "shallow" & lys$pre])

bxplts(value = "nh", data = subsetD(lys, depth == "shallow" & pre))
# log seems slightly fine

# different random factor structure
m1 <- lme(log(nh) ~ co2 * time, random = ~1|ring/plot, data = subsetD(lys, depth == "shallow" & pre))
m2 <- lme(log(nh) ~ co2 * time, random = ~1|ring, data = subsetD(lys, depth == "shallow" & pre))
m3 <- lme(log(nh) ~ co2 * time, random = ~1|id, data = subsetD(lys, depth == "shallow" & pre))
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atcr.cmpr(m3)$models
# model5 is best

Iml_S_pre <- atcr.cmpr(m3)[[5]]

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

## ----Stat_FACE_Lys_Ammonium_S_postCO2

############
# Post-CO2 #
############

range(lys$nh[lys$depth == "shallow" & lys$post])

bxplts(value = "nh", ofst= 0.06, data = subsetD(lys, depth == "shallow" & post))

# remove the higher outlier
boxplot(lys$nh[lys$depth == "shallow" & lys$post])
NhRmOl <- subset(lys, nh < max(nh))
bxplts(value = "nh", ofst= 0.06, data = subsetD(NhRmOl, depth == "shallow" & post))
bxplts(value = "nh", ofst= 0.07, data = subsetD(NhRmOl, depth == "shallow" & post))

bxcxplts(value= "nh", data= subsetD(NhRmOl, depth == "shallow" & post),
         sval = 0.06, fval = 2, lambda = seq(-10, 0, .5))

bxplts(value = "nh", ofst= 1, data = subsetD(NhRmOl, depth == "shallow" & post),
       lambda = seq(-7, -5, .05))
 # use box-cox

# The initial model is
Iml_S_post <- lmer((nh + 1)^(-5.78) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(NhRmOl, depth == "shallow" & post),
                   na.action = "na.omit")
Anova(Iml_S_post)

# The final model
Fml_S_post <- stepLmer(Iml_S_post)
Anova(Fml_S_post)
AnvF_nh_S_Post <- Anova(Fml_S_post, test.statistic = "F")
AnvF_nh_S_Post

summary(Fml_S_post)

# model diagnosis
plot(Fml_S_post)
qqnorm(resid(Fml_S_post))
qqline(resid(Fml_S_post))

# box-cox plot doesn't really improve the model so just use the simpler
# transformation
Iml_S_post <- lmer(log(nh + .07) ~ co2 * time + + (1|block) + (1|ring) + (1|id), 
          data = subsetD(NhRmOl, depth == "shallow" & post))

# The final model
Fml_S_post <- stepLmer(Iml_S_post)
Anova(Fml_S_post)
AnvF_nh_S_Post <- Anova(Fml_S_post, test.statistic = "F")
AnvF_nh_S_Post

summary(Fml_S_post)

# model diagnosis
plot(Fml_S_post)
qqnorm(resid(Fml_S_post))
qqline(resid(Fml_S_post))

## ----Stat_FACE_Lys_Ammonium_S_preCO2_Smmry
# The initial model is:
Iml_S_pre$call
Anova(Iml_S_pre)

# The final model is :
Fml_S_pre$call
Anova(Fml_S_pre)

## ----Stat_FACE_Lys_Ammonium_S_postCO2_Smmry
# The initial model is:
Iml_S_post@call
Anova(Iml_S_post)

# The final model is :
Fml_S_post@call

# Chi
Anova(Fml_S_post)

# F
AnvF_nh_S_Post
