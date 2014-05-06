lys<-read.table("Data/lys.time8.txt",header=T, colClasses = c("time" = "factor", "ring" = "factor", "ring" = "factor", "plot" = "factor"))
# remove tn.corrected, coverage
lys <- lys[, !(names(lys) %in% c("coverage", "actual.cov", "tn.corrected"))]

lys$date <- as.Date(dmy(lys$date))

# add more dataset and merge
load("output/data/ProcessedTOC.RData")
load("output//data//processedAQ2.Rdata")

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

# remove the rows whose response variabes are all NA
a <- lys[, c("no", "nh", "po", "toc", "tc", "ic", "tn")]
lys <- lys[!(apply(a, 1, (function(x) all(is.na(x))))), ]

# fix co2, time columns
lys$co2 <- factor(ifelse(lys$ring %in% c("1", "4", "5"), "elev", "amb"))

lys$time <- NULL
a <- data.frame(date = unique(lys$date), time = factor(c(1:length(unique(lys$date)))))
lys <- merge(lys, a, by = c("date"), all.x = TRUE)

xtabs(~ring + plot + depth + date, data = lys)

# update stracture
lys$ring <- as.factor(lys$ring)
lys$plot <- as.factor(lys$plot)
lys$depth <- as.factor(lys$depth)

# pre and post co2: note that last month of pre is used as
# base line for post co2
lys$pre <- ifelse(lys$time %in% c(1:3), TRUE, FALSE)
lys$post <- ifelse(!(lys$time %in% c(1:2)), TRUE, FALSE)

# id for layter analysis
lys$id <- lys$ring:lys$plot

save(lys, file = "output/data/FACE_lysimeter.RData")

