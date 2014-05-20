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

##########################
# Create a summary table #
##########################
CreateTable <- function(dataset, fac, ...){
  a <- dataset[c("date", fac, "value")] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a, date~variable, mean, na.rm = TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) #merge datasets
  mer <- mer[,c(1, order(names(mer)[-grep("date|N", names(mer))])+1, grep("N", names(mer)))] #re-order columns
  mer$date <- as.character(mer$date) # date is turned into character for knitr output 
  return(format(mer, ...))
}

#function which creates excel worksheets
crSheet <- function(sheetname, datasetS, datasetD){
  #create sheet
  sheet <- createSheet(wb, sheetName = sheetname)
  
  #add data to the sheet
  
  # shallow
  addDataFrame("shallow",sheet,row.names=FALSE,col.names=FALSE,startRow=2) # title of the table
  addDataFrame(datasetS, sheet, showNA = TRUE, row.names = FALSE, startRow = 3,
               characterNA = "NA")
  
  # deep
  addDataFrame("deep",sheet,row.names=FALSE,col.names=FALSE,startRow=19) # title of the table
  addDataFrame(datasetD, sheet, showNA = TRUE, row.names = FALSE, startRow = 20,
               characterNA = "NA")
  
  #title of the sheet
  addDataFrame(t(c(sheetname, "unit=ppm")), sheet, startRow = 1, row.names = FALSE, col.names = FALSE)
}

###############################
# Created multiple worksheets #
###############################
# extract required data set from a list of summary tables
# and put them in excel worksheets
# the same nutrient but different layers will be placed 
# in the same worksheet

MltcrSheet <- function(tbl, shnames, ntrs){
  l_ply(1:length(shnames), function(x) {
    lnames <- paste(ntrs[x], c("shallow", "deep"), sep = ".") 
    # names of the required data set in the list
    
    crSheet(sheetname = shnames[x], 
            datasetS = tbl[[ lnames[1] ]], 
            datasetD = tbl[[ lnames[2] ]])
  })
}

############################
# make a summary dataframe #
############################
Crt_SmryDF <- function(data, val = "value"){
  x <- data[ ,val]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N  <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

####################
# plot mean and se #
####################
# general settings
PltMean <- function(data, ...){
  
  # change factor level names for labelling on figs
  data$depth <- factor(data$depth, levels = c("shallow", "deep"), labels = c("Shallow", "Deep")) 
  data$co2 <-  factor(data$co2, levels = c("amb", "elev"), labels = c("Ambient", expression(eCO[2])))
  
  ylabs <- c(expression(NO[3]^"-"-N~(mg~l^-1)),
             expression(NH[4]^"+"-N~(mg~l^-1)),
             expression(PO[4]^"3-"-P~(mg~l^-1)),
             expression(TOC~(mg~l^-1)),
             expression(TC~(mg~l^-1)),
             expression(IC~(mg~l^-1)),
             expression(TN~(mg~l^-1)))
  
  # create ylab according to variable
  ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")
  
  # when plotting multiple variables at the same time
  if(length(unique(data$variable)) > 1) ylab <- expression((mg~l^-1)) else {
    # only one variable
    for (i in 1:7){
      if(unique(data$variable) == ntrs[i]) ylab  <- ylabs[i]
    }
  }
  
  p <- ggplot(data, aes_string(x = "date", y = "Mean", ...))
  
  p2 <- p + geom_line(size = 1) +
    geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", ...) , width = 5) + 
    labs(x = "Time", y = ylab) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
               linetype = "dashed", 
               col = "black") +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-7-1", "2014-4-2"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1))
  return(p2)
}

#####################
# Plot Ring mean #
#####################
PltRngMean <- function(data){
  # change factor level names for labelling
  p <- PltMean(data, col = "ring", linetype = "co2") +
    scale_color_manual(values = palette(), "Ring", 
                       labels = paste("Ring", c(1:6), sep = "_")) +
    scale_linetype_manual(values = c("dashed", "solid"), 
                          expression(CO[2]~trt), 
                          labels = c("Ambient", expression(eCO[2]))) +
    facet_grid(depth~. )
  return(p)
}

######################
# Plot CO2 trt mean #
######################
PltCO2Mean <- function(data){
  p <- PltMean(data, col = "co2", linetype = "depth") +
    scale_color_manual(values = c("blue", "red"), expression(CO[2]~trt)) +
    scale_linetype_manual(values = c("solid", "dashed"), "Depth")
  return(p)
}

#################################
# labells for facet_wrap graphs #
#################################
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  # copied from http://stackoverflow.com/questions/19282897/
  # how-to-add-expressions-to-labels-in-facet-wrap
  # require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

#######################
#model simplification #
#######################
MdlSmpl <- function(model){
  mod2 <- update(model, method = "ML") #change method from REML to ML
  stai <- stepAIC(mod2, trace = FALSE) #model simplification by AIC
  if(length(stai$coefficients$fixed) > 1)
  dr <- drop1(stai, test="Chisq") else #test if removing a factor even more significantly lowers model
    dr <- "All factors are removed"
  model <- update(stai, method="REML")
  if(!is.character(dr))
  ifelse(all(dr[[4]] < 0.05, na.rm=TRUE), anr <- anova(model), anr<-NA) else
    anr  <- NA
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic = stai$anova, drop1 = dr, anova.reml = anr, model.reml = model, model.ml = stai))
}
#############################################
# compare different auto-correlation models #
#############################################

atcr.cmpr <- function(model, rndmFac){
  if(rndmFac == "ring/plot"){
    model2 <- update(model,corr=corCompSymm(form=~1|ring/plot)) 
  } else {
    if(rndmFac == "ring"){
      model2 <- update(model,corr=corCompSymm(form=~1|ring))
    } else {
      model2 <- update(model,corr=corCompSymm(form=~1|id))
    }
  }
  
  model3 <- update(model,correlation=corARMA(q=2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q=1))
  a <- anova(model,model2,model3,model4,model5)
  rownames(a) <- c("NULL", "corCompSymm", "corARMA(q=2)", "corAR1()", "corARMA(q=1)")
  models <- list(model, model2, model3, model4, model5, 'models' = a)
  return(models)
}

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse OR box-cox
bxplts <- function(value, ofst = 0, data, ...){
  data$y <- data[[value]] + ofst #ofst is added to make y >0
  a <- boxcox(y ~ co2 * time, data = data)
  par(mfrow = c(2, 3))
  boxplot(y ~ co2*time, data, main = "row")
  boxplot(log(y) ~ co2*time, main = "log", data)
  boxplot(sqrt(y) ~ co2*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ co2*time, main = "power(1/3)", data)
  boxplot(1/y ~ co2*time, main = "inverse", data)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black") 
  boxplot(y^(BCmax) ~ co2*time, 
          main = "", sep = "=", 
          data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), 
        col.main = texcol)
  par(mfrow = c(1,1))
}

# multiple box-cox power plot for different constant values
bxcxplts <- function(value, data, sval, fval){
  data$yval <- data[[value]]
  ranges <- seq(sval, fval, (fval - sval)/9)
  
  # store parameters given from box-cox plot
  par(mfrow = c(5, 2))
  BCmax <- vector()
  for (i in 1:10){
    data$y <- data$yval + ranges[i]
    a <- boxcox(y ~ co2 * time, data = data)
    BCmax[i] <- a$x[a$y == max(a$y)]
  }
  
  # plot box plot with poer given from box-box for 
  # each contstant value
  par(mfrow = c(5, 2))
  par(omi = c(0, 0, 0, 0), mai = c(0.4, 0.4, 0.4, 0))
  sapply(1:10, function(x) {
    boxplot((yval + ranges[x]) ^ BCmax[x] ~ co2 * time, 
            main = "", data = data)
    texcol <- ifelse(BCmax[x] < 0, "red", "black") 
    title(main = paste("constant=", round(ranges[x], 4), 
                       ", boxcox=", round(BCmax[x], 4)),
          col.main = texcol)
  })
  par(mfrow = c(1,1))
}

##############################
# subset data and droplevels #
##############################
subsetD <- function(...){
  droplevels(subset(...))
}

####################################
# create table of contrast results #
####################################
cntrstTbl <- function(cntrstRes, data, ...){
  d <- unique(data$date)
  ds <- format(d, format = "%b-%Y")
  
  Df <- data.frame(
    date = ds,
    contrast  =  cntrst$Contrast,
    SE = cntrst$SE,
    t = cntrst$testStat,
    df = cntrst$df,
    P.value = cntrst$Pvalue)
  return(format(Df, ...))
}

###############
# Print table #
###############
printTbl <- function(tbl, caption, label, ...){
  print(xtable(tbl,
               caption = caption, 
               label = label, 
               align = rep("l", ncol(tbl) + 1)),
        caption.placement = "top", 
        include.rownames = FALSE,
        table.placement = "H", ...)
}

printRngTbl <- function(tbl, caption, label, ...){
  printTbl(tbl[, 1:7], 
           caption = caption,
           label = label,
           ...)
  printTbl(tbl[, c(1, 8:13)], 
           caption = NULL,
           label = NULL,
           ...)
  printTbl(tbl[, c(1, 14:19)], 
           caption = NULL,
           label = NULL,
           ...)
}

###########################################
# print tables for shallow and deep layer #
###########################################
printLysRngTbl <- function(data, variable, datatype,  ...){
  caps <- paste("Ring mean of", variable, "in soil water in a", c("shallow", "deep"), "layer")
  labs <- paste("table:", "FACE_Ring_Lys_", variable, "_", c("S", "D"),sep = "")
  df <- paste(datatype, c("shallow", "deep"), sep = ".")
  
  printRngTbl(data[[df[1]]], 
              caption = caps[1], 
              label = labs[1],
              ...)
  printRngTbl(data[[df[2]]], 
              caption = caps[2], 
              label = labs[2],
              ...)
}

printLysCO2Tbl <- function(data, variable, datatype,  ...){
  caps <- paste("CO2 trt mean of", variable, "in soil water in a", c("shallow", "deep"), "layer")
  labs <- paste("table:", "FACE_CO2_Lys_", variable, "_", c("S", "D"),sep = "")
  df <- paste(datatype, c("shallow", "deep"), sep = ".")
  
  printTbl(data[[df[1]]], 
              caption = caps[1], 
              label = labs[1],
              ...)
  printTbl(data[[df[2]]], 
              caption = caps[2], 
              label = labs[2],
              ...)
}

