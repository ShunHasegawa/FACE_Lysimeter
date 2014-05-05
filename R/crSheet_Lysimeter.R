#function which creates excel worksheets
crSheet <- function(sheetname,dataset,fac,nutrient,unit){
  #create sheet
  sheet <- createSheet(wb,sheetName=sheetname)
  
  #add data (depth=shallow) to the sheet 
  addDataFrame(CreateTable(dataset,fac,nutrient,dep="shallow"),sheet,showNA=TRUE,row.names=FALSE,startRow=3,
               characterNA="NA")
  addDataFrame("shallow",sheet,row.names=FALSE,col.names=FALSE,startRow=2)
  
  #add data (depth=deep) to the sheet 
  addDataFrame(CreateTable(dataset,fac,nutrient,dep="deep"),sheet,showNA=TRUE,row.names=FALSE,startRow=20,
               characterNA="NA")
  addDataFrame("deep",sheet,row.names=FALSE,col.names=FALSE,startRow=19)
  
  #title of the sheet
  addDataFrame(t(c(sheetname,unit)),sheet,row.names=FALSE,col.names=FALSE,startRow=1)
  
}
