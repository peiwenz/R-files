
#setwd("G:/Operational Risk/Support to Strategy/2019")

library("xlsx")
library(formattable)

setwd("/ext/home/analyst")
load("fin_data")


field.names <- read.xlsx("SSOI_line_items.xlsx",1)

get.ssoi <- function(crd,field.names,my.data)
{
  my.data[,"field_0025_dt"] <- gsub("-","/",substring(my.data[,"field_0025_dt"],1,10))
  dimnames(my.data)[[1]] <- my.data[,1]
  my.data <- my.data[,-1]
  my.data.matrix <- t(my.data)
  
  n <- dim(my.data.matrix)[2]
  my.data.matrix <- my.data.matrix[,n:1]
  my.data.matrix <- my.data.matrix[,1:pmin(8,n)]
  n <-  dim(my.data.matrix)[2]
  my.data.matrix[is.na(my.data.matrix)] <- 0
  bad.rows <- apply(my.data.matrix,1,function(x) sum(x=="0"))
  my.data.matrix <- my.data.matrix[bad.rows < n,]
  
  
  Totals<-c()
  Avg.Qtr<-c()
  percent.of.Total8Qtrs<-c()
  Totals2018<-c()
  Totals2017<-c()
  percent.of.Total2018<-c()
  year.over.year<-c()
  year.over.year.percent<-c()
  totalrow<-match("field_14030_am",dimnames(my.data.matrix)[[1]])
  for (i in seq(1,length(my.data.matrix[,1]))){
    Totals[[i]]<-sum(my.data.matrix[i,1:8])
    Avg.Qtr[[i]]<-sum(my.data.matrix[i,1:8])/8
    Totals2018[[i]]<-sum(my.data.matrix[i,1:4])
    Totals2017[[i]]<-sum(my.data.matrix[i,5:8])
  }
  for (i in seq(1,length(Totals2018))){
    percent.of.Total8Qtrs[[i]]<-percent(Totals[[i]]/Totals[[totalrow]])
    percent.of.Total2018[[i]]<-percent(Totals2018[[i]]/Totals2018[[totalrow]])
    year.over.year[[i]]<-percent(Totals2018[[i]]-Totals2017[[i]])
    year.over.year.percent[[i]]<-percent((Totals2018[[i]]-Totals2017[[i]])/Totals2017[[i]])
  }
  my.data.matrix<-cbind(my.data.matrix,Totals,Avg.Qtr,percent.of.Total8Qtrs,percent.of.Total2018,Totals2018,Totals2017,year.over.year,year.over.year.percent)
  my.data.matrix
  #match("field_11126_am ",dimnames(my.data.matrix)[[1]])
  #which( dimnames(my.data.matrix)[1]=="field_11126_am " )
  #my.data.matrix
  
  my.fields <- toupper(dimnames(my.data.matrix)[[1]])
  idx <- match(my.fields, as.character(field.names[,"clmn_nm"]),nomatch = 0)
  my.fields[idx!=0] <- as.character(field.names[idx[idx!=0],"itemdescription"])
  dimnames(my.data.matrix)[[1]] <- my.fields
  
  wb<-createWorkbook(type="xlsx")
  
  myformat <- CellStyle(wb, dataFormat=DataFormat("#,##0_);(#,##0)"), alignment=NULL,  border=NULL, fill=NULL, font=NULL)+Border(color="black", position=c("BOTTOM", "LEFT","TOP","RIGHT"), pen=c("BORDER_THIN", "BORDER_THIN","BORDER_THIN","BORDER_THIN"))
  highlight<- CellStyle(wb)+Fill(foregroundColor = 'yellow', backgroundColor = 'yellow',pattern = "SOLID_FOREGROUND")+Border(color="black", position=c("BOTTOM", "LEFT","TOP","RIGHT"), pen=c("BORDER_THIN", "BORDER_THIN","BORDER_THIN","BORDER_THIN"))
  blueback<-CellStyle(wb)+Fill(foregroundColor = 'light blue', backgroundColor = 'light blue',pattern = "SOLID_FOREGROUND")+Font(wb, isBold=TRUE)+Border(color="black", position=c("BOTTOM", "LEFT","TOP","RIGHT"), pen=c("BORDER_THIN", "BORDER_THIN","BORDER_THIN","BORDER_THIN"))
  greenback<-CellStyle(wb)+Fill(foregroundColor = 'light green', backgroundColor = 'light green',pattern = "SOLID_FOREGROUND")+Font(wb, isBold=TRUE)+Border(color="black", position=c("BOTTOM", "LEFT","TOP","RIGHT"), pen=c("BORDER_THIN", "BORDER_THIN","BORDER_THIN","BORDER_THIN"))
  
  
  sheet <- createSheet(wb, sheetName = "SSOI")
  
  n <- dim(my.data.matrix)[2]
  dfColIndex <- rep(list(myformat), n)
  names(dfColIndex) <- 1:n
  
  setColumnWidth(sheet,colIndex = 1,colWidth = 90)
  setColumnWidth(sheet, colIndex=2:(n+1), colWidth=21)
  
  
  addDataFrame(my.data.matrix, sheet, startRow=1, startColumn=1, colStyle = dfColIndex)
  rows<-getRows(sheet)
  cells<-getCells(rows)
  setCellValue(cells[[1]],'Description')
  setCellStyle(cells[[17*27+9]],highlight)
  for (i in seq(1,17)){setCellStyle(cells[[i]],blueback)}
  for (i in seq(2,9)){setCellStyle(cells[[i]],greenback)}
  #force equal
  date<-c('2018/12/31', '2018/09/30' ,'2018/06/30', '2018/03/31', '2017/12/31' ,'2017/09/30', '2017/06/30', '2017/03/31')
  for (i in seq(2,9)){setCellValue(cells[[i]],date[i-1])}
  
  saveWorkbook(wb, paste("findata_",crd,".xlsx",sep=""))
  
  
  return(my.data.matrix)                     
}
get.ssoi(361,field.names,my.data)
