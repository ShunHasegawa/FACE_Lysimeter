ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")

# remove outlier
RmOl <- lys

## po : shallow : pre OR post (time = 3)##
boxplot(lys$po[lys$depth == "shallow" & lys$pre])
ol <- max(lys$po[lys$depth == "shallow" & lys$pre])
RmOl$po[which(RmOl$po == ol)] <- NA

## po : deep : post ##
boxplot(lys$po[lys$depth == "deep" & lys$post])
ol <- max(lys$po[lys$depth == "deep" & lys$post])
RmOl$po[which(RmOl$po == ol)] <- NA

## nh : shallow : post ##
boxplot(lys$nh[lys$depth == "shallow" & lys$post])
ol <- max(lys$nh[lys$depth == "shallow" & lys$post])
RmOl$nh[which(RmOl$nh == ol)] <- NA

# Melt data frame
lysMlt <- melt(RmOl, id = names(RmOl)[which(!(names(RmOl) %in% ntrs))], na.rm = TRUE)
lysMlt$variable <- factor(lysMlt$variable, levels = c(ntrs)) # change the level order of variable 

# ring summary table & mean
RngSmmryTbl <- dlply(lysMlt, .(variable, depth), function(x) CreateTable(x, fac = "ring", digit = 2, nsmall = 4))
RngMean <- ddply(lysMlt, .(time, date, co2, ring, block, depth, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable, depth), function(x) CreateTable(x, fac = "co2", digit = 2, nsamll = 4))

########################
# create xcel workbook #
########################
wb <- createWorkbook()

# worksheet for rowdata

sheet <- createSheet(wb, sheetName="raw_data")
addDataFrame(lys, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheet for rawdata without outlier
sheet <- createSheet(wb, sheetName="raw_data_withoutOutlier")
addDataFrame(RmOl, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
vars <- c("Nitrate", "Ammonium", "Phosphate", "TotalOrganicC", "TotalC", "InorganicC", "TotalN")
shnames <- paste("RingMean", vars, sep = "_")
MltcrSheet(tbl = RngSmmryTbl, shnames = shnames, ntrs = ntrs)

# worksheets for temp trt summary
shnames <- paste("CO2_mean", vars, sep = "_")
MltcrSheet(tbl = TrtSmmryTbl, shnames = shnames, ntrs = ntrs)

# save file
saveWorkbook(wb,"output//table/FACE_Lysimeter.xlsx")
