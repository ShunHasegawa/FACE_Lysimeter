correctIC <- function(filename, scfile = "toc.rawdata/", otfile = "toc.rawdata/processed/"){
  f <- paste(scfile, filename, " (detailed).txt", sep = "")
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
  mean.res <- d[!duplicated(cbind(d$Sample.ID, d$Analysis.Inj..)), ]
  xtabs(~Sample.ID+Analysis.Inj.., data = mean.res)
  
  
  # subset required columns
  smry.res <- mean.res[ , c("Sample.ID", "Analysis.Inj..", "NewIC")]
  unique(smry.res$Sample.ID)
  
  # remove unnecessary rows
  smry.res <- subset(smry.res, !(Sample.ID %in% c("Untitled", "TC BLANK", "TC 50PPM", "IC 5PPM", "TN 25PPM", "IC BLANK", "TN BLANK")))
  
  colnames(smry.res)[c(2,3)] <- c("variable", "value")
  
  smry.res.cast <- cast(smry.res, Sample.ID ~ variable)
  
  # calculate TOC
  smry.res.cast$TOC <- with(smry.res.cast, TC - IC)
  
  # read a summary dataset to be re calculated
  f2 <- paste(scfile, filename, ".txt", sep = "")
  res <- read.table(f2, skip = 11, fill = TRUE, sep = "\t", header = TRUE)
  
  # marge corrected result and current result
  nrow(res)
  nrow(smry.res.cast)
  
  cmb <- merge(res, smry.res.cast, by = "Sample.ID", all.x = TRUE)
  cr.cmb <- cmb
  cr.cmb$Result.TOC. <- cmb$TOC
  cr.cmb$Result.IC. <- cmb$IC
  names(cr.cmb)[15:18]
  cr.cmb <- cr.cmb[ , -c(15:18)]
  
  #extract unknown
  cr.cmb <- subset(cr.cmb, Type == "Unknown")
  
  # save
  f3 <- paste(otfile, filename, ".corrected.txt", sep = "")
  write.table(cr.cmb, file = f3, sep = "\t", row.names = FALSE)
  return(cr.cmb)
}




