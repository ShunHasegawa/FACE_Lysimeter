rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")

dir("aq2.rawdata")

p.jul13 <- read.csv("aq2.rawdata/13-10-11 lysi FACE jul2013 o-phos shun.csv")

# format time
times <- strsplit(as.character(p.jul13$Time), split = " ")

d <- sapply(times, "[", 3)
m <- sapply(times, "[", 2)
y <- sapply(times, "[", 5)
sec <- sapply(times, "[", 4)
times.pst <- paste(d, m, y, sec, sep = "/")
p.jul13$time <- dmy_hms(times.pst)

# plot ccv & ccb
plot(Result ~ time, data = subset(p.jul13, Sample.ID == "C C V"))
plot(Result ~ time, data = subset(p.jul13, Sample.ID == "ccb"))


dev.off()

subset(p.jul13, Sample.ID %in% c("C C B", "ccb"))

summary(p.jul13