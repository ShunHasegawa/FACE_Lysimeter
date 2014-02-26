rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")

Mar13 <- read.table("toc.rowdata//210313 Lysimiter FACE (detailed).txt", skip = 13,  fill = TRUE, sep = "\t",
                    header = TRUE)
  # sep="\t" enables you to read data with space withough separating them to different columns

# subset IC standard
ICdata <- subset(Mar13, Anal. == "IC" & Type == "Standard")
ICdata <- droplevels(ICdata)

plot(Mean.Area ~ Conc., data = ICdata)

# conc. = 10 is wrong so remove
ICdata <- ICdata[ICdata$Conc. != 10.0, ]

# calibration curve with intercept = 0
ml <- lm(Mean.Area ~ Conc., data = ICdata)
summary(ml)
