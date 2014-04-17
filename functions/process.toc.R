rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")
source("functions/functions.R")

# ring, plot, depth factor
ring <- data.frame(ring = factor(c(1:6)))
plot <- data.frame(plot = factor(c(1:4)))
depth <- data.frame(depth = c("shallow", "deep"))
d <- merge(ring, plot)
locs <- merge(d, depth)
locs$Sample.ID <- paste(locs$ring, locs$plot, toupper(substring(locs$depth, 1, 1)), sep = ".")


##############
# Correct IC #
##############
# IC in some data files are wrong so correct

# function
source("functions/correctIC.R")

## March 2013 ##
Mar13 <- correctIC("FACE.Lysimeter_21MAR2013")

# format date
dates <- strsplit(as.character(Mar13$Sample.Name), split = " ")
Mar13$date <- sapply(dates, "[", 2)
Mar13$date <- as.Date(dmy(Mar13$date))

# combine with ring, plot, depth
mar13.data <- merge(Mar13, locs, by = "Sample.ID", all = TRUE)


## May 2013 ##
May13 <- correctIC("FACE.Lysimeter_02MAY2013")

# format date
May13$date <- as.Date(dmy(as.character(May13$Sample.Name)))
unique(May13$date)

# combine with ring, plot, depth
may13.data <- merge(May13, locs, by = "Sample.ID", all = TRUE)

# other files #
files <- dir(path = "Data//TOC//processed", pattern = "txt$", full.names = TRUE)


## Jul 2013 ##
Jul13 <- read.table("Data//TOC/processed/040713 Lysimiter FACE.txt", skip = 11, fill = TRUE, sep = "\t", header = TRUE, 
                colClasses = c("Sample.ID" = "character"))

# weird sample ID -> rename it
Jul13$Sample.ID[grep("Untitle", Jul13$Sample.ID)] <- "1.1.S"

jul13.data <- processTOC(Jul13)


## Nov 2013 ##
Nov13 <- read.table("Data//TOC/processed/141113 Lysimiter FACE.txt", skip = 11, fill = TRUE, sep = "\t", header = TRUE, 
                   colClasses = c("Sample.ID" = "character"))

nov13.data <- processTOC(Nov13)

# combine all files
int.data <- rbind.fill(may13.data, mar13.data, jul13.data, nov13.data)
int.data <- int.data[order(int.data$date, int.data$ring, int.data$plot, int.data$depth), ]
summary(int.data)
unique(int.data$date)

# extract required columns
names(int.data)
fin.data <- int.data[c("Result.TOC.", "Result.TC.", "Result.IC.", "Result.TN.", "date", "ring", "plot", "depth")]
fin.data <- fin.data[complete.cases(fin.data), ]
colnames(fin.data) <- c("toc", "tc", "ic", "tn", "date", "ring", "plot", "depth")
fin.data$co2 <- factor(ifelse(fin.data$ring %in% c("1","4","5"), "elev", "amb"))

unique(fin.data$date)

# "2013-03-21", "2013-03-26" --> "2013-03-22"
fin.data$date[fin.data$date %in% as.Date(c("2013-03-21", "2013-03-26"))]  <- as.Date("2013-03-22")
xtabs(~date + ring + plot + depth, fin.data)

# save
save(fin.data, file = "toc.rawdata/processed/processed.R")
