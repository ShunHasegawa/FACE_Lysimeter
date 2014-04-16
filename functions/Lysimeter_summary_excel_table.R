#summary table for excel
head(lys)

#function which creates summary table
source("functions/CreateTable_Lysimeter.R")

#function which creates excel worksheets
source("functions/crSheet_Lysimeter.R")

#create xcel workbook
wb <- createWorkbook()

#worksheet for rowdata
#extract required colmns
res <- lys[c("time","date","co2","ring","plot","depth","no","nh","po","toc","tn")]
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(res,sheet,showNA=TRUE,row.names=FALSE,characterNA="NA")

#crate excel workwheet for each variable
#worksheets for ring summary
shnames <- paste("Ring_mean.",c("Nitrate","Ammonium","Phosphate","Total.Organic.C","Total.N"),sep="")
nut <- c("no","nh","po","toc","tn")

for (i in 1:5){
  crSheet(sheetname=shnames[i],dataset=res,fac="ring",nutrient=nut[i],unit="unit=ppm")
}


#worksheets for co2 summary
shnames <- paste("CO2_mean.",c("Nitrate","Ammonium","Phosphate","Total.Organic.C","Total.N"),sep="")
for (i in 1:5){
  crSheet(sheetname=shnames[i],dataset=ring.mean,fac="co2",nutrient=nut[i],unit="unit=ppm")
}

#save file
saveWorkbook(wb,"table/FACE_Lysimeter.xlsx")
