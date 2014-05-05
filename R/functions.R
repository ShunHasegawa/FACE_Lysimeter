# split dataset to each nutrient & depth ##
ntr.splt <- function(data = lys, ntr){
  a <- data[c("time", "date", "ring", "plot", "depth", "co2", ntr)]
  a <- a[complete.cases(a), ]
  sp.a <- split(a, a$depth)
  dl.spa <- lapply(sp.a, droplevels)
  return(dl.spa)
}


###################################
# Coorect AQ2 result based on CCV #
###################################

# subset dataset beween each ccv
Crrct.ccv <- function(x, data, ccval = 7.5){
  # subset dataset between given ccvs
  ccv.ps <- grep("V$", as.character(data$Sample.ID))
  
  ts <- data[c(ccv.ps[x] : ccv.ps[x + 1]),]
  trng <- range(ts$times)
  xv <- as.numeric(trng)
  yv <- ts$Result[ts$times == trng[1] | ts$times == trng[2]] 
  
  b <- ccval * (1 / yv[1] - 1 / yv[2]) / (xv[1] - xv[2])
  a <- ccval / yv[1] - b * xv[1]
  
  ts$Result <- ts$Result * (a + b * as.numeric(ts$times))
  ts$times <- NULL
  return(ts)
}

# applied the function above for each subsets of ccvs
Crrtct.ccv.df <- function(data, ccval = 7.5){
  # make time factor as numeric
  a <- sapply(as.character(data$Time), strsplit, " ")
  b <- ldply(a, function(x) paste(x[c(5, 2, 3, 4)], collapse = "/"))
  
  b$V1 <- ymd_hms(b$V1)
  
  names(b) <- c("Time", "times")
  
  # merge
  mrg.res <- merge(data, b, by = "Time")
  
  # reorder accoding to time
  mrg.res <- mrg.res[order(mrg.res$times), ]
  
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID))
  
  # re-caluculate the results
  a <- ldply(1:(length(ccv.ps)-1), function(x) Crrct.ccv(x, data = mrg.res, ccval))
  
  return(a)
}

######################################################
# process and combine aq 2 data, then create a table #
######################################################
# process aq2 data
prcsAQ2 <- function(filename){
  
  res <- read.csv(filename, colClasses= c("Sample.Details" = "character"))
  
  # remove ccv, standartd
  if(length(grep("^C|^STANDARD", as.character(res$Sample.ID))) == 0) 
    res <- res else # if it doesn't contain any ccv or standard
    res <- res[-grep("^C|^STANDARD", as.character(res$Sample.ID)),]
  
  # remove dup, top, middle
  if(length(grep("dup$|top|middle", as.character(res$Sample.Details))) == 0)
    a <- res else
    a <- res[-grep("dup$|top|middle", as.character(res$Sample.Details)),]
  
  # add date, ring, plot, depth
  info <- strsplit(a$Sample.Details, split = "[.]")
  d <- sapply(info, "[", 3)
  a$date <- as.Date(ymd(d))
  a$ring <- as.factor(sapply(info, "[", 4))
  a$plot <- as.factor(sapply(info, "[", 5))
  a$depth <- as.factor(sapply(info, "[", 6))
  a$depth <- factor(ifelse(a$depth == "S", "shallow", "deep"))
  return(a)
}

# combine all processed files
prcs.all.dat <- function(filename){
  # process and combine all files
  all.data <- ldply(filename, function(x) prcsAQ2(paste("Data/AQ2//2014Apr/ReadyToProcess/", x, sep = "")))
  
  # cast for each test and rename
  names(all.data)[grep("Result", names(all.data))]<- "value"
  a.cst <- cast(all.data, date + ring + plot + depth ~ Test.Name)
  names(a.cst)[grep("Nitrate", names(a.cst))] <- "no"
  names(a.cst)[grep("Ammonia", names(a.cst))] <- "nh"
  names(a.cst)[grep("Phos", names(a.cst))] <- "po"
  return(a.cst)
}

######################
# Process TOC output #
######################
processTOC <- function(data, DateCol){
  # reformat date
  names(data)[DateCol] <- "date" # DateCol: position the column for date
  data$date <- as.Date(dmy(data$date))
  return(data)
}

##############
# Correct IC #
##############
correctIC <- function(filename, scfile = "Data/TOC/ICNeedToCorrect/", otfile = "Data//TOC//IC_corrected/"){
  f <- paste(scfile, filename, "_detailed.txt", sep = "")
  d <- read.table(f, skip = 13,  fill = TRUE, sep = "\t", header = TRUE)
  # sep="\t" enables you to read data with space withough separating them to different columns
  
  # subset IC standard
  ICdata <- subset(d, Anal. == "IC" & Type == "Standard")
  ICdata <- droplevels(ICdata)
  
  # conc. = 10 is wrong so remove
  ICdata <- ICdata[ICdata$Conc. != 10.0, ]
  
  # injection vol for standard was 65, but it is 50 for sample assay, adjust it
  ICdata$Mean.Area <- ICdata$Mean.Area * 50 / 65
  
  
  # calibration curve with intercept = 0
  ml <- lm(Mean.Area ~ -1 + Conc., data = ICdata)
  coef(ml)[[1]]
  
  # re-calculate IC values
  d$NewIC <- ifelse(d$Analysis.Inj.. %in% c("TC", "TN"), 
                    d$Mean.Conc., 
                    d$Mean.Area/coef(ml)[1])
  d$NewIC[d$Sample.ID == "IC 5PPM"]
  
  # subset only mean value
  
  mean.res <- d[!duplicated(d[, c("Sample.Name", "Sample.ID", "Analysis.Inj..")]), ]
  xtabs(~Sample.ID+Analysis.Inj.., data = mean.res)
  
  
  # subset required columns
  smry.res <- mean.res[ , c("Sample.Name", "Sample.ID", "Analysis.Inj..", "NewIC")]
  unique(smry.res$Sample.ID)
  
  # remove unnecessary rows
  smry.res <- subset(smry.res, !(Sample.ID %in% c("Untitled", "TC BLANK", "TC 50PPM", "IC 5PPM", "TN 25PPM", "IC BLANK", "TN BLANK")))
  
  colnames(smry.res)[c(3,4)] <- c("variable", "value")
  
  smry.res.cast <- cast(smry.res, Sample.Name + Sample.ID ~ variable)
  
  # calculate TOC
  smry.res.cast$TOC <- with(smry.res.cast, TC - IC)
  
  # read a summary dataset to be re calculated
  f2 <- paste(scfile, filename, ".txt", sep = "")
  res <- read.table(f2, skip = 11, fill = TRUE, sep = "\t", header = TRUE)
  
  # marge corrected result and current result
  nrow(res)
  nrow(smry.res.cast)
  
  cmb <- merge(res, smry.res.cast, by = c("Sample.Name", "Sample.ID"), all.x = TRUE)
  cr.cmb <- cmb
  cr.cmb$Result.TOC. <- cmb$TOC
  cr.cmb$Result.IC. <- cmb$IC
  names(cr.cmb)[14:17]
  cr.cmb <- cr.cmb[ , -c(14:17)]
  
  #extract unknown
  cr.cmb <- subset(cr.cmb, Type == "Unknown")
  
  # save
  f3 <- paste(otfile, filename, ".corrected.txt", sep = "")
  write.table(cr.cmb, file = f3, sep = "\t", row.names = FALSE)
  return(cr.cmb)
}

