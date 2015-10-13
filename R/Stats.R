###########
# Nitrate #
###########
source("R/Stats_nitrate.R")

############
# Ammonium #
############
source("R/Stats_ammonium.R")

#############
# Phosphate #
#############
source("R/Stats_phosphate.R")

######
# TC #
######
# source("R/Stats_TC.R")

#######
# TOC #
#######
source("R/Stats_TOC.R")

######
# IC #
######
# source("R/Stats_IC.R")

######
# TN #
######
# source("R/Stats_TN.R")

######################
# Summary stat table #
######################
summary(lys)
# create stat summary table for LMM with CO2 and time
CO2TimeStatList <- list('no_shallow' = AnvF_Nit_S_Post,
                        'no_deep'    = AnvF_Nit_D_Post,
                        'nh_shallow' = AnvF_nh_S_Post,
                        'nh_deep'    = AnvF_nh_D_Post,
                        'po_shallow' = AnvF_P_S_Post,
                        'po_deep'    = AnvF_P_D_Post,
                        'toc_shallow'= AnvF_toc_S_post,
                        'toc_deep'   = AnvF_toc_D_post)

Stat_CO2Time <- ldply(names(CO2TimeStatList), 
                      function(x) StatTable(CO2TimeStatList[[x]], variable = x))
# split variable to variable and depth
splitVar <- ldply(strsplit(as.character(Stat_CO2Time$variable), split = "_"))

Stat_CO2Time <- within(Stat_CO2Time, {
  variable <- factor(splitVar[, 1])
  depth <- factor(splitVar[, 2])
})

save(Stat_CO2Time, file = "output//data/FACE_lysimeter_CO2xTime_Stats.RData")

########################
## Result of contrast ##
########################
ContrastDF <- rbind.fill(list(
  FACE_Lys_NH_D_postCO2_CntrstDf,
  FACE_Lys_P_D_postCO2_CntrstDf,
  FACE_Lys_TOC_D_postCO2_CntrstDf
  ))
save(ContrastDF, file = "output//data/FACE_Lysimeter_ContrastDF.RData")


