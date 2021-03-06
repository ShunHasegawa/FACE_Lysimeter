rm(list=ls(all=TRUE))

# library
source("R/functions.R")

##############
# Correct IC #
##############
# IC in some data files are wrong so correct
## March 2013 ##
Mar13 <- correctIC("FACE.Lysimeter_21MAR2013")

# format date
dates <- strsplit(as.character(Mar13$Sample.Name), split = " ")
Mar13$date <- sapply(dates, "[", 2)
Mar13$date <- as.Date(dmy(Mar13$date))

## May 2013 ##
May13 <- correctIC("FACE.Lysimeter_02MAY2013")

# format date
May13$date <- as.Date(dmy(as.character(May13$Sample.Name)))

###############
# other files #
###############
files <- dir(path = "Data//TOC", pattern = "txt$", full.names = TRUE)


## Jul 2013 ##
Jul13 <- read.table( "Data//TOC/FACE.Lysimeter_04JUL2013.txt", skip = 11, fill = TRUE, sep = "\t", header = TRUE, 
                colClasses = c("Sample.ID" = "character"))

# weird sample ID -> rename it
Jul13$Sample.ID[grep("Untitle", Jul13$Sample.ID)] <- "1.1.S"

jul13.data <- processTOC(Jul13, DateCol = 3)


## Nov 2013 ##
Nov13 <- read.table("Data//TOC/FACE.Lysimeter_14NOV2013.txt", skip = 11, fill = TRUE, sep = "\t", header = TRUE, 
                   colClasses = c("Sample.ID" = "character"))

nov13.data <- processTOC(Nov13, DateCol = 3)

## Feb 2014 ##
FEB14 <- read.table("Data//TOC/FACE.Lysimeter_25FEB2014.txt", skip = 11, fill = TRUE, sep = "\t", header = TRUE, 
                    colClasses = c("Sample.ID" = "character"))
feb14.data <- processTOC(FEB14, DateCol = 3)

## Apr 2014 ##
APR14 <- read.table("Data//TOC/FACE.Lysimeter_02APR2014.txt", skip = 11, fill = TRUE, sep = "\t", header = TRUE, 
                    colClasses = c("Sample.ID" = "character", "Sample.Name" = "character"))
apr14.data <- processTOC(APR14, DateCol = 4)
names(apr14.data)[grep("Sample.Name", names(apr14.data))] <- "Sample.ID"


# combine all files
int.data <- rbind.fill(Mar13, May13, jul13.data, nov13.data, feb14.data, apr14.data)

# remove unnecessary rows and organize
some(int.data)
unique(int.data$Type)
# remove stanrdar & control
N.df <- subset(int.data, Type == "Unknown")

# checked re-run sample 3.4.D on 4 Jul 2013
N.df[N.df$Sample.ID == "3.4.D RERUN", ]
N.df[N.df$date == as.Date("2013-07-04") & N.df$Sample.ID == "3.4.D", ]
  ## there values are really close so just discard the Rerun data
N.df <- N.df[-which(N.df$Sample.ID == "3.4.D RERUN"), ]

# 3.2.D low volume: this time just use as it is
N.df$Sample.ID[which(N.df$Sample.ID == "3.2.D LOW VOLUME")] <- "3.2.D" 

# create ring, depth factor
a <- ldply(strsplit(N.df$Sample.ID, split = "[.]"))
colnames(a) <- c("ring", "plot", "depth")

# merge
N.df <- cbind(N.df, a)
 
  # order
N.df <- N.df[order(N.df$date, N.df$ring, N.df$plot, N.df$depth), ]
summary(int.data)
unique(int.data$date)

# extract required columns
names(N.df)
fin.data <- N.df[c("Result.TOC.", "Result.TC.", "Result.IC.", "Result.TN.", "date", "ring", "plot", "depth")]
summary(fin.data)
colnames(fin.data) <- c("toc", "tc", "ic", "tn", "date", "ring", "plot", "depth")
fin.data$co2 <- factor(ifelse(fin.data$ring %in% c("1","4","5"), "elev", "amb"))
xtabs(~ ring + plot + depth + date, data = fin.data)

# "2013-03-21", "2013-03-26" --> "2013-03-22"
unique(fin.data$date)
subset(fin.data, date < as.Date("2013-03-27") & ring == "3" & plot == "2" & depth == "S")

# this time just use 3-2-S form 26-3-2013
fin.data <- subset(fin.data, !(date == as.Date("2013-03-21") & ring == "3" & plot == "2" & depth == "S"))

fin.data$date[fin.data$date %in% as.Date(c("2013-03-21", "2013-03-26"))]  <- as.Date("2013-03-22")

#depth: s->shallow, d->deep
fin.data$depth <- ifelse(fin.data$depth == "D", "deep", "shallow")

xtabs(~ring + plot + depth + date, fin.data)
# there are two ring3, plot2, shallow, 2013-03-22
subset(fin.data, ring == "3" & plot == "2" & depth == "shallow" & date == as.Date("2013-03-22"))

# save
save(fin.data, file = "output/data/ProcessedTOC.RData")

