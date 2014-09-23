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
fls <- paste("output/figs/FACE_LysimeterCO2", vars, sep = "")

# save as pdf and png
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

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
df$variable <- factor(df$variable, 
                      labels = c(expression(NO[3]^"-"),
                                 expression(NH[4]^"+"),
                                 expression(PO[4]^"3-"),
                                 expression(DOC)))
df$depth <- factor(df$depth, labels = c("Shallow", "Deep"))

# create data frame for fig sub labels
subLabDF <- data.frame(xv = as.Date("2012-06-15"),
                       ddply(df, .(variable), summarise, 
                             yv = max(Mean + SE, na.rm = TRUE)),
                       co2 = "amb")
# Add depth, not that I want the same yv value for each depth so repeat the
# above data frame for each depth
subLabDF <- expand.grid.df(subLabDF, data.frame(depth = c("Shallow", "Deep")))
# Add labels after sorting by variable
subLabDF <- subLabDF[order(subLabDF$variable), ]
subLabDF$labels <-  LETTERS[1:nrow(subLabDF)]


# plot theme
science_theme <- theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = c(.2, .93),
                       legend.title = element_blank())


# creat a plot
p <- ggplot(df, aes(x = date, y = Mean, group = co2))

pl <- p + geom_line(aes(linetype = co2), alpha = .6) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 15, size = .3,
                position = position_dodge(20)) + 
  geom_point(aes(shape = co2, fill = co2), position = position_dodge(20), alpha = .8) +
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
  science_theme

ggsavePP(filename = "output//figs/FACE_Manuscript/FACE_Lysimeter", plot = pl,
         width = 7, height = 8)
