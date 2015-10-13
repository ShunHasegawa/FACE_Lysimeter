## ----Stat_FACE_Lys_Ammonium_S_preCO2
###########
# Pre-CO2 #
###########

range(lys$nh[lys$depth == "shallow" & lys$pre])

bxplts(value = "nh", data = subsetD(lys, depth == "shallow" & pre))
# log seems slightly fine

Iml_S_pre_nh <- lmer(log(nh) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                  data = subsetD(lys, depth == "shallow" & pre), 
                  na.action = "na.omit")

# The initial model is: 
Iml_S_pre_nh@call

Anova(Iml_S_pre_nh, test.statistic = "F")

Fml_S_pre_nh <- stepLmer(Iml_S_pre_nh, alpha.fixed = .1)

# The final model is:
Fml_S_pre_nh@call

Anova(Fml_S_pre_nh, test.statistic = "F")

# model diagnosis
plot(Fml_S_pre_nh)
qqnorm(residuals(Fml_S_pre_nh))
qqline(residuals(Fml_S_pre_nh))

# what if I remove the lower outlier
ol <- which(qqnorm(residuals(Fml_S_pre_nh))$y == min(qqnorm(residuals(Fml_S_pre_nh))$y))
mm <- update(Iml_S_pre_nh, subset = -ol)
Anova(mm, test.statistic = "F")
  # same result
plot(mm)
qqnorm(residuals(mm))
qqline(residuals(mm))

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
Iml_S_post_nh <- lmer((nh + 1)^(-5.78) ~ co2 * time + (1|block) + (1|ring) + (1|id),
                   data = subsetD(NhRmOl, depth == "shallow" & post),
                   na.action = "na.omit")
Anova(Iml_S_post_nh)

# The final model
Fml_S_post_nh <- stepLmer(Iml_S_post_nh, alpha.fixed = .1)
Anova(Fml_S_post_nh)
AnvF_nh_S_Post <- Anova(Fml_S_post_nh, test.statistic = "F")
AnvF_nh_S_Post

summary(Fml_S_post_nh)

# model diagnosis
plot(Fml_S_post_nh)
qqnorm(resid(Fml_S_post_nh))
qqline(resid(Fml_S_post_nh))

# box-cox plot doesn't really improve the model so just use the simpler
# transformation
Iml_S_post_nh <- lmer(log(nh + .07) ~ co2 * time + + (1|block) + (1|ring) + (1|id), 
          data = subsetD(NhRmOl, depth == "shallow" & post))

# The final model
Fml_S_post_nh <- stepLmer(Iml_S_post_nh)
Anova(Fml_S_post_nh)
AnvF_nh_S_Post <- Anova(Fml_S_post_nh, test.statistic = "F")
AnvF_nh_S_Post

summary(Fml_S_post_nh)

# model diagnosis
plot(Fml_S_post_nh)
qqnorm(resid(Fml_S_post_nh))
qqline(resid(Fml_S_post_nh))

## ----Stat_FACE_Lys_Ammonium_S_preCO2_Smmry
# The initial model is:
Iml_S_pre_nh@call
Anova(Iml_S_pre_nh)

# The final model is :
Fml_S_pre_nh@call
Anova(Fml_S_pre_nh)

## ----Stat_FACE_Lys_Ammonium_S_postCO2_Smmry
# The initial model is:
Iml_S_post_nh@call
Anova(Iml_S_post_nh)

# The final model is :
Fml_S_post_nh@call

# Chi
Anova(Fml_S_post_nh)

# F
AnvF_nh_S_Post
