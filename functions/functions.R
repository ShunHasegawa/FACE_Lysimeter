# split dataset to each nutrient & depth ##
ntr.splt <- function(data = lys, ntr){
  a <- data[c("time", "date", "ring", "plot", "depth", "co2", ntr)]
  a <- a[complete.cases(a), ]
  sp.a <- split(a, a$depth)
  dl.spa <- lapply(sp.a, droplevels)
  return(dl.spa)
}

# produce box plots with transformed data (log, sqrt, power(1/3))
bxplts <- function(value, ofst = 0,data){
  dev.off()
  par(mfrow = c(2,2))
  y <- data[[value]] + ofst #ofst is added to make y >0
  boxplot(y ~ co2*time, data)
  boxplot(log(y) ~ co2*time, main = "log", data)
  boxplot(sqrt(y) ~ co2*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ co2*time, main = "power(1/3)", data)
}

# function for mmodel simplification
ana<-function(model){
  mod2<-update(model,method="ML") #change method from REML to ML
  stai<-stepAIC(mod2,trace=FALSE) #model simplification by AIC
  dr<-drop1(stai,test="Chisq") #test if removing a factor even more significantly lowers model
  model<-update(stai,method="REML")
  ifelse(all(dr[[4]]<0.05,na.rm=TRUE),anr<-anova(model),anr<-NA) 
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic=stai$anova,drop1=dr,anova.reml=anr,model.reml=model,model.ml=stai))
}


# compare different auto-correlation models
atcr.cmpr <- function(model){
  model2 <- update(model,corr=corCompSymm(form=~1|ring/plot))
  model3 <- update(model,correlation=corARMA(q=2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q=1))
  a <- anova(model,model2,model3,model4,model5)
  models <- list(model, model2, model3, model4, model5, a)
  return(models)
}

# model simplificaiton after comparing with auto-correlation
smpl.ml <- function(model){
  models <- atcr.cmpr(model) #auto-correlation
  ml <- models[[which(models[[6]]$AIC == min(models[[6]]$AIC))]] #min AIC
  return(ana(ml))
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


