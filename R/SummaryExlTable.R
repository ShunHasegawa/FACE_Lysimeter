ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")

# Melt data frame
lysMlt <- melt(lys, id = names(lys)[which(!(names(lys) %in% ntrs))], na.rm = TRUE)
lysMlt$variable <- factor(lysMlt$variable, levels = c(ntrs)) # change the level order of variable 

# ring summary table & mean
RngSmmryTbl <- dlply(lysMlt, .(variable, depth), function(x) CreateTable(x, fac = "ring", digit = 2, nsmall = 4))
RngMean <- ddply(lysMlt, .(time, date, co2, ring, depth, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable, depth), function(x) CreateTable(x, fac = "co2", digit = 2, nsamll = 4))

########################
# create xcel workbook #
########################
wb <- createWorkbook()

# worksheet for rowdata and rowdata without outlier
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(lys, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
vars <- c("Nitrate", "Ammonium", "Phosphate", "TotalOrganicC", "TotalC", "InorganicC", "TotalN")
shnames <- paste("RingMean", vars, sep = "_")
MltcrSheet(tbl = RngSmmryTbl, shnames = shnames, ntrs = ntrs)

# worksheets for temp trt summary
shnames <- paste("CO2_mean.", vars, sep = "_")
MltcrSheet(tbl = TrtSmmryTbl, shnames = shnames, ntrs = ntrs)

# save file
saveWorkbook(wb,"output//table/FACE_Lysimeter.xlsx")
