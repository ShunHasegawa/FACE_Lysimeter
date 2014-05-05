rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")

# functions
source("functions/functions.R")


############################
# Correct NO3 based on CCV #
############################
fls <- dir(path = "Data/AQ2/2014Apr/NO3_NeedToBeCorrected/", pattern = ".csv$")

res1 <- read.csv(paste("Data/AQ2/2014Apr/NO3_NeedToBeCorrected/", fls[1], sep = ""), header = TRUE)
res2 <- read.csv(paste("Data/AQ2/2014Apr/NO3_NeedToBeCorrected/", fls[2], sep = ""), header = TRUE)

write.csv(Crrtct.ccv.df(res1, ccval=7.0353), paste("Data/AQ2/2014Apr/ReadyToProcess/", "Corrected_", fls[1], sep =""), row.names = TRUE)
write.csv(Crrtct.ccv.df(res2, ccval=7.0154), paste("Data/AQ2/2014Apr/ReadyToProcess/", "Corrected_", fls[2], sep =""), row.names = TRUE)
# first ccv was used as ccaval

# process adn combine to as single file
# files to be read
fls <- dir(path = "Data/AQ2//2014Apr/ReadyToProcess/", pattern = ".csv$")

#save
aq2 <- prcs.all.dat(filename = fls)

save(aq2, file = "output//data//FACE.Lysimeter.Rdata")
