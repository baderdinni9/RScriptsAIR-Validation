

####################################################File Summer#######################################################################

list.of.packages <- c("RODBC", "getopt", "stringr","optparse","plyr","ggplot2","rCharts","slidify", "slidifyLibraries","bit64", "rCharts","plotly","googleVis","data.table")


library(data.table)
library(plyr)
library(bit64)
library(stringr)

#file_list <- list.files(path=folder, pattern=paste(tcid,"_LOSS2CLF_",ValID,".*",".txt",sep="")) # create list of all .csv files in folder
file_list <- list.files(path="//qafile2/Leonardo/Feature Data/loss to csv narendra",pattern=".csv")
# read in each .csv file in file_list and rbind them into a data frame called data 
read_csv_filename <- function(filename){
  ret <- data.frame(fread(filename,autostart = 25))
  ret$Source <- filename #EDIT
  ret
}


datasum <- data.frame(matrix(ncol=21, nrow=length(file_list)))
for(i in 1:length(file_list)){
  data <- read_csv_filename(paste(folder,"/",file_list[i],sep = ""))
  data<-subset(data,Sub.area==0)
  datasum[i,]<-aggregate(cbind(data$), by=list(Category=data$Source), FUN=sum)
  
}

colnames(datasum)<-c('ModelType','LOB1','LOB2','LOB3','LOB4','LOB5','LOB6','LOB7',
                     'LOB8','LOB9','LOB10','LOB11','LOB12','LOB13','LOB14','LOB15','LOB16','LOB17','LOB18','LOB19','LOB20'
)
