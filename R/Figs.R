# summary data frame
RngMean <- ddply(lysMlt, .(time, date, co2, ring, depth, variable), Crt_SmryDF)
TrtMean <- ddply(RngMean, .(time, date, co2, depth, variable), function(x) Crt_SmryDF(x, val = "Mean"))

# set background of figure
theme_set(theme_bw())

###################################
## plot each nutrient separately ##
###################################
vars <- c("Nitrate", "Ammoinum", "Phosphate", "TOC", "TC", "IC", "TN")

# Ring
RngFg <- dlply(RngMean, .(variable), PltRngMean)
fls <- paste("output/figs/FACE_LysimeterRing", vars, sep = "")

# save as pdf and png
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 6))

# CO2 trt
TrtFg <- dlply(TrtMean, .(variable), PltCO2Mean)
fls <- paste("Output//Figs/WTC_LysimeterCO2Trt_", vars, sep = "")

# save as pdf and png
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

###################################
## plot all nutrients separately ##
###################################
p <- PltCO2Mean(TrtMean) + 
  facet_wrap( ~variable, ncol = 2, scale = "free_y")

# modify labels
ylabs <- c(expression(NO[3]^"-"-N),
           expression(NH[4]^"+"-N),
           expression(PO[4]^"3-"-P),
           expression(TOC),
           expression(TC),
           expression(IC),
           expression(TN))

pl <- facet_wrap_labeller(p, labels = ylabs)


ggsavePP(filename = "Output/Figs/WTC_LysimeterCO2", plot = pl, width = 8, height = 8)
