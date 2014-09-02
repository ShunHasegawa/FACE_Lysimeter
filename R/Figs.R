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

###################################
## plot all nutrients separately ##
###################################
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
science_theme <- theme(panel.grid.major = element_line(size = 0.2, color = "grey"), 
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = "top",
                       legend.title = element_blank())

# subset Nitrate, ammonium, phosphate and TOC
df <- subsetD(TrtMean, variable %in% c("no", "nh", "po", "toc"))
df$grp <- df$co2:df$depth
# relevel grp
df$grp <- factor(df$grp, levels = c("amb:shallow", "elev:shallow", "amb:deep", "elev:deep"))

legLab <- c("Shallow-Ambient", expression(Shallow-eCO[2]),
            "Deep-Ambient", expression(Deep-eCO[2]))

p <- ggplot(df, aes(x = date, y = Mean, group = grp))

p2 <- p + geom_line(aes(linetype = grp), alpha = .6) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 15, size = .3,
                position = position_dodge(20)) + 
  geom_point(aes(shape = grp, fill = grp), position = position_dodge(20), alpha = .8) +
  labs(x = "Month", y = expression(Dissolved~nutrients~'in'~soil~water~(mg~l^"-1"))) +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
             linetype = "dashed", col = "black") +
  scale_x_date(breaks= date_breaks("2 month"),
               labels = date_format("%b-%y"),
               limits = as.Date(c("2012-7-1", "2014-4-2"))) +
  scale_shape_manual(values = rep(c(21, 24), each = 2), labels = legLab) +
  scale_fill_manual(values = rep(c("black", "white"), 2), labels = legLab) +
  scale_linetype_manual(values = rep(c("solid", "dashed"), each = 2), labels = legLab) +
  facet_wrap( ~variable, ncol = 2, scale = "free_y") +
  science_theme

# modify labels
ylabs <- c(expression(NO[3]^"-"),
           expression(NH[4]^"+"),
           expression(PO[4]^"3-"),
           expression(DOC))

pl <- facet_wrap_labeller(p2, labels = ylabs)
ggsavePP(filename = "output//figs/FACE_Manuscript/FACE_Lysimeter", plot = pl, width = 7, height = 6)
