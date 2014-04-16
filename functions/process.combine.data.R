rm(list=ls(all=TRUE))

lys<-read.table("Data/lys.time8.txt",header=T, colClasses = c("time" = "factor", "ring" = "factor", "ring" = "factor", "plot" = "factor"))
# remove tn.corrected, coverage
lys <- lys[, -c(5, 10,15)]

lys$date <- as.Date(dmy(lys$date))

# add more dataset and merge
load("Data/TOC//processed/processed.R")
load("output//data//FACE.Lysimeter.Rdata")

# combine toc and aq2 data
names(fin.data)
names(aq2)


# check date as well
unique(fin.data$date)
unique(aq2$date)

# combine aq2 data
toc.aq <- merge(fin.data, aq2, by = c("date", "ring", "plot", "depth"), all = TRUE)

# combine with previous dataet
lys <- rbind.fill(lys, toc.aq)

lys$co2 <- factor(ifelse(lys$ring %in% c("1", "4", "5"), "elev", "amb"))

lys <- lys[order(lys$date),]

# quick graph
library(ggplot2)
library(reshape)
library(plyr)
library(gtools)


names(lys)
lys <- lys[, -grep("time|tc|ic", names(lys))]

lys.mlt <- melt(lys, id = c("date", "ring", "plot", "depth", "co2"))
summary(lys.mlt)


plt.mn <- ddply(lys.mlt, .(date, ring, plot, depth, co2, variable), summarise, value = mean(value, na.rm = TRUE))
plt.mn <- plt.mn[complete.cases(plt.mn), ]
summary(plt.mn)
names(plt.mn)


crt.m.se <- function(data){
  value  <- mean(data$value, na.rm = TRUE)
  SEs <- ci(data$value, na.rm = TRUE)[4]
  N <- sum(!is.na(data$value))
  data.frame(value, SEs, N)
}
rng.mn <- ddply(plt.mn, .(date, ring, depth, co2, variable), crt.m.se)
head(rng.mn)

theme_set(theme_bw())
p <- ggplot(rng.mn, aes(x = date, y = value, col = ring, linetype = ring))

p + geom_line(size = 1) + 
  geom_errorbar(aes(ymin = value - SEs, ymax = value + SEs, col = ring), width = 5) + 
  scale_color_manual(values = palette(), "Ring", labels = paste("Ring", c(1:6),sep = "_")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid", "solid", "dashed"), 
                        "Ring", labels = paste("Ring", c(1:6),sep = "_")) +
  labs(x = "Time", y = "") +
  facet_grid(variable ~ depth, scales = "free_y") +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), linetype = "dashed")

co2.mn <- ddply(rng.mn, .(date, depth, co2, variable), crt.m.se)

p <- ggplot(co2.mn, aes(x = date, y = value, col = co2))

p2 <- p + geom_line(size = 1) + 
  geom_errorbar(aes(ymin = value - SEs, ymax = value + SEs, col = co2), width = 5) + 
  scale_color_manual(values = palette(), "CO2_trt", labels = c("elev", "amb")) +
  labs(x = "Time", y = "") +
  facet_grid(variable ~ depth, scales = "free_y") +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), linetype = "dashed")

ggsave(filen = "output//figs/mltplot.pdf", plot = p2, width = 10, height = 10)


xtabs(~date + ring + plot + depth, data = lys)
summary(lys)







# filling missing months
msMonth <- data.frame(date = as.Date(c("2013-6-15", "2013-9-15", "2013-10-15")))

lys <- rbind.fill(lys, msMonth)

# update time
lys <- lys[,-1]

ti <- data.frame(time = factor(c(1:length(unique(lys$date)))), date = unique(lys$date))
loc <- data.frame(ring = factor(rep(c(1:6), each = 8)),
                  plot = factor(rep(c(1:4), each = 2, 6)), 
                  depth = rep(c("shallow", "deep"), 24))

xtabs(~ring+plot+depth, data = loc)

ti.lc <- merge(ti, loc)

# check if missing cells were filled
xtabs(~date + ring + plot + depth, data = ti.lc)

fin.lys <- merge(lys, ti.lc, by = c("date", "ring", "plot", "depth"), all.y = TRUE)

xtabs(~date + ring + plot + depth, data = fin.lys)

nrow(fin.lys)/48

fin.lys[fin.lys$date == as.Date("2013-03-22") & fin.lys$ring == "3" & fin.lys$plot == "2",]

fin.lys <- fin.lys[-405, ]

xtabs(~date + ring + plot + depth, data = fin.lys)

nrow(fin.lys)/48

lys <- fin.lys


# save
save(lys, file = "output/data/lysimeter.Rdata")
