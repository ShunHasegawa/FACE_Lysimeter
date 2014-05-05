###########
# Shallow #
###########
range(toc.s$toc)

bxplts("toc", data = toc.s)

# log looks better

levels(toc.s$time)

# pre-co2
model1 <- lme(log(toc) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = toc.s)

ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(log(toc) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11", "12", "13"), data = toc.s)
ml <- smpl.ml(model1)
anova(ml$model.reml)

########
# Deep #
########
range(toc.d$toc)
bxplts("toc", data = toc.d)

# log

levels(toc.d$time)

# pre-co2
model1 <- lme(log(toc) ~ co2 * time, random = ~1| ring/plot, subset = time %in% c("1", "2", "3"),
              data = toc.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)

# post-co2
model1 <- lme(log(toc) ~ co2 * time, random = ~1| ring/plot, 
              subset = time %in% c("3", "4", "8", "9", "10", "11", "12", "13"), data = toc.d)
ml <- smpl.ml(model1)
anova(ml$model.reml)
# there is co2:time interaction

# contrast
levels(toc.d$time)[3:10]
cntrst<- contrast(ml$model.reml,
                  a=list(time=levels(toc.d$time)[3:10],co2="amb"),
                  b=list(time=levels(toc.d$time)[3:10],co2="elev"))

# table
d <- unique(toc.d$date)[3:10]
ds <- format(d, format = "%b-%Y")

PostCO2_TOC_Deep.cntrst.df <- data.frame(
  date = ds,
  contrast  =  cntrst$Contrast,
  SE = cntrst$SE,
  t = cntrst$testStat,
  df = cntrst$df,
  P.value = cntrst$Pvalue)
  