# summary data frame
RngMean <- ddply(lysMlt, .(time, date, co2, ring, depth, variable), Crt_SmryDF)
TrtMean <- ddply(RngMean, .(time, date, co2, depth, variable), function(x) Crt_SmryDF(x, val = "Mean"))
save(TrtMean, file = "output//data/FACE_Lysimeter_CO2Mean.RData")

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
fls <- paste("output/figs/FACE_LysimeterCO2", vars, sep = "")

# save as pdf and png
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

################
## for poster ##
################
poster_theme <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.text.x = element_text(angle=45, vjust= 1, hjust = 1, 
                                                 size = 13),
                      legend.position = "non",
                      axis.title.y = element_text(size = 15),
                      plot.title = element_text(size = 25, face = "bold"))

pl  <- PltCO2Mean(subsetD(TrtMean, variable == "toc")) +
  ggtitle("Dissoved organic C") +
  labs(x = NULL, y = expression((mg~l^"-1")))+
  poster_theme
ggsavePP(filename = "output//figs//GSBI_Poster/FACE_DOC_CO2", plot = pl, width = 6, height = 4)


################################
## plot all nutrients togther ##
################################
p <- PltCO2Mean(TrtMean) + 
  facet_wrap( ~variable, ncol = 2, scale = "free_y")

# modify labels
ylabs <- c(expression(NO[3]^"-"),
           expression(NH[4]^"+"),
           expression(PO[4]^"3-"),
           expression(DOC),
           expression(TC),
           expression(IC),
           expression(TN))

pl <- facet_wrap_labeller(p, labels = ylabs)


ggsavePP(filename = "output/figs/FACE_LysimeterCO2", plot = pl, width = 8, height = 8)


#######################
# Figs for manuscript #
#######################
# subset Nitrate, ammonium, phosphate and TOC
df <- subsetD(TrtMean, variable %in% c("no", "nh", "po", "toc"))

# change variable and depth labels for plotting
df <- within(df, {
  variable <- factor(variable, 
                        labels = c(expression(NO[3]^"-"),
                                   expression(NH[4]^"+"),
                                   expression(PO[4]^"3-"),
                                   expression(DOC)))
  depth <- factor(depth, labels = c("Shallow", "Deep"))
})

###########################
## df for fig sub labels ##
###########################
subLabDF <- data.frame(xv = as.Date("2012-06-15"),
                       ddply(df, .(variable), summarise, 
                             yv = max(Mean + SE, na.rm = TRUE)),
                       co2 = "amb")
# Add depth, note that I want the same yv value for each depth so repeat the
# above data frame for each depth
subLabDF <- expand.grid.df(subLabDF, data.frame(depth = c("Shallow", "Deep")))

## Add labels after sorting by variable
subLabDF <- subLabDF[order(subLabDF$variable), ]
subLabDF$labels <- sort(c(LETTERS[1:4], paste(LETTERS[1:4], "'", sep = "")))

#######################
## df for stat table ##
#######################
# load stat summary table; note that if you need the updated result, you need to
# run Stats.R first
load("output//data//FACE_lysimeter_CO2xTime_Stats.RData")

## compute ylength and ymax for each variable
ylengthDF <- ddply(df, .(variable), 
                   function(x) 
                     data.frame(ylength = max(x$Mean +x$SE, na.rm = TRUE) -
                                  min(x$Mean - x$SE, na.rm = TRUE),
                                ymax = max(x$Mean +x$SE, na.rm = TRUE)))
# ylength is given as the difference between max and min

## relabel Stat_CO2Time to be consistent with the data df
Stat_CO2Time <- within(Stat_CO2Time, {
  variable <- factor(variable, 
                     levels = c("no", "nh", "po", "toc"),
                     labels = c(expression(NO[3]^"-"),
                                expression(NH[4]^"+"),
                                expression(PO[4]^"3-"),
                                expression(DOC)))
  depth <- factor(depth, levels = c("shallow", "deep"), labels = c("Shallow", "Deep"))
  
})

statDF <- StatPositionDF(StatRes = Stat_CO2Time, 
                         variable = levels(ylengthDF$variable), 
                         ytop = ylengthDF$ymax,
                         ylength = ylengthDF$ylength)
# x positin for statDF
varDepDF <- unique(data.frame(statDF[, c("variable", "depth")]))
xvalDF <- data.frame(varDepDF,
                     xval = as.Date(c("2014-1-20", "2014-1-20", 
                                      "2013-11-1", "2014-1-20", 
                                      "2014-1-20", "2013-11-1",
                                      "2013-7-20", "2014-1-20")))
statDF <- merge(statDF, xvalDF, by = c("variable", "depth"))
statDF[statDF$depth == "Shallow" & statDF$ variable == "DOC", ]$yval <- 
  statDF[statDF$depth == "Shallow" & statDF$ variable == "DOC", ]$yval - 50


############
# COntrast #
############
# load contrastDF to annotate stat result and combine with max values from
# TrtMean as y position
load("output//data/FACE_Lysimeter_ContrastDF.RData")

# relabel variable
ContrastDF <- within(ContrastDF, {
  depth <- factor(depth, levels = c("shallow", "deep"), labels = c("Shallow", "Deep"))
  variable <- factor(variable,
                     levels = c("nh", "po", "toc"),
                     labels = c(expression(NH[4]^"+"),
                                expression(PO[4]^"3-"),
                                expression(DOC)))
})

# determin y position
yposDF <- ddply(df, .(date, variable, depth),
                function(x) {x$SE <- ifelse(is.na(x$SE), 0, x$SE)
                             # turn NA in SE into 0
                              data.frame(yval = max(x$Mean + x$SE))})
# merge
Antt_CntrstDF <- merge(ContrastDF, 
                       yposDF, 
                       by = c("date", "variable", "depth"), all.x = TRUE)
Antt_CntrstDF$co2 <- "amb" # co2 column is required as it's used for mapping
Antt_CntrstDF <- subset(Antt_CntrstDF, stars != "") 
# remove empty rows as they causes trouble when using geom_text

################
## plot theme ##
################
science_theme <- theme(
#   panel.grid.major = element_line(size = 0.2, color = "grey"),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = c(.2, .94),
                       legend.title = element_blank())

##################
## creat a plot ##
##################
p <- ggplot(df, aes(x = date, y = Mean, group = co2))

pl <- p + geom_line(aes(linetype = co2), 
                    position = position_dodge(20)) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 15, size = .3,
                position = position_dodge(20)) + 
  geom_point(aes(shape = co2, fill = co2), 
             position = position_dodge(20)) +
  labs(x = "Month", y = expression(Dissolved~nutrients~'in'~soil~solution~(mg~l^"-1"))) +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
             linetype = "dashed", col = "black") +
  scale_x_date(breaks= date_breaks("2 month"),
               labels = date_format("%b-%y"),
               limits = as.Date(c("2012-6-15", "2014-4-2"))) +
  scale_shape_manual(values = c(24, 21), 
                     labels = c("Ambient", expression(eCO[2]))) +
  scale_fill_manual(values = c("black", "white"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Ambient", expression(eCO[2]))) +
  geom_text(aes(x = xv, y = yv * .95, label = labels),
            fontface = "bold",
            data = subLabDF) +
  facet_grid(variable ~ depth, scale = "free_y", labeller = label_parsed) +
  science_theme +
  geom_text(data = subset(statDF, predictor != ""), 
            aes(x = xval, y = yval, label = predictor),
            size = 2, hjust = 1, parse = TRUE) +
  # unless remove [" "] with predictor != "", labels will be messed up due to
  # this empty level
  geom_text(data = statDF, 
            aes(x = xval + 56, y = yval, label = p), 
            size = 2, parse = TRUE) +
  # stat symbols
  geom_text(data = Antt_CntrstDF, aes(x = date, y = yval, label = stars), 
            vjust = 0, parse = TRUE)
ggsavePP(filename = "output//figs/FACE_Manuscript/FACE_Lysimeter", plot = pl,
         width = 7, height = 7)
