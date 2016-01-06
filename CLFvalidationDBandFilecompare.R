

list.of.packages <- c("RODBC", "getopt", "stringr","optparse","plyr","ggplot2","rCharts","slidify", "slidifyLibraries","bit64", "rCharts","plotly","googleVis","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.r-project.org/", type="win.binary")


########################################Fetching the libraries#######################################################################
library(RODBC)
library(ggplot2)
library(googleVis)
library(optparse)


################################################## For Automation Gherkin Use Only ##################################################
option_list <- list(
  make_option("--outfile", help = "CSV file to save results to, this is required")
)

# get command line options
# get command line options
inputs <- parse_args(OptionParser(option_list=option_list), args = commandArgs(TRUE), TRUE, TRUE)

# exit if outfile was not passed
if ( is.null(inputs$options$outfile) ) {
  write("--outfile is a required option\n", stderr())
  q(status=1)
} else {
  outfile <- inputs$options$outfile
  paste("Outfile3: ", inputs$args[1],inputs$args[2],inputs$args[3])
}





resultdb<-inputs$args[1]
ValID<-inputs$args[2]
server<-inputs$args[3]#'qa-ngp-a-db1'
folder <- '//qafile3/Regression/Upgrade/Temptreaty'      # path to folder that holds multiple .csv files
TC<-  inputs$args[5]


resultdb<-'Treaty_Upgrade_MU_Res_updt'
ValID<-'2'
server<-'qa-ngp-sql\\sql2014'
folder <- '//qafile3/Regression/Upgrade/Temptreaty'      # path to folder that holds multiple .csv files
tcid<-  'tc'
tpid<-  'tp'

sqlString<-paste('select * from ',resultdb,'..MastertemptableCLFLOBVal_',ValID,'_',tcid,'_',tpid,collapse="",sep="") 
#sqlconn <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s;UID=sa;PWD=Clasic22", server))

driver <- "DRIVER={SQL Server}"
server <- paste0("SERVER=",server)
trustedConn <- "trusted_connection=TRUE"

sqlconn <- odbcDriverConnect(paste(driver,server,trustedConn,sep=";"))
DatabaseCLFSummer<-data.frame(sqlQuery(sqlconn,sqlString))
#write.table(d, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE, qmethod = "double")


####################################################File Summer#######################################################################

print(folder)
print(TC)
print(ValID)

library(data.table)
library(plyr)
library(bit64)
library(stringr)

file_list <- list.files(path=folder, pattern=paste("_LOSS2CLF_",".*",".txt",sep="")) # create list of all .csv files in folder
print(file_list)
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
  datasum[i,]<-aggregate(cbind(data$LOB.1,data$LOB.2,data$LOB.3,data$LOB.4,data$LOB.5,data$LOB.6,data$LOB.7,data$LOB.8,data$LOB.9,data$LOB.10,data$LOB.11,
                               data$LOB.12,data$LOB.13,data$LOB.14,data$LOB.15,data$LOB.16,data$LOB.17,data$LOB.18,data$LOB.19,data$LOB.20), by=list(Category=data$Source), FUN=sum)
  
}


#data <- data.frame(do.call("rbind",  lapply(file_list, 
#function(x) ldply(paste(folder,"/",x,sep = ""), read_csv_filename))))
# 
# print(data)
# datasum<-aggregate(data$LOB.1+data$LOB.2+data$LOB.3+data$LOB.4+data$LOB.5+data$LOB.6+data$LOB.7+data$LOB.8+data$LOB.9+data$LOB.10+data$LOB.11, by=list(Category=data$Source), FUN=sum)
colnames(datasum)<-c('ModelType','LOB1','LOB2','LOB3','LOB4','LOB5','LOB6','LOB7',
                     'LOB8','LOB9','LOB10','LOB11','LOB12','LOB13','LOB14','LOB15','LOB16','LOB17','LOB18','LOB19','LOB20'
)
datasum$ModelType<-gsub(ValID, "", datasum$ModelType)
datasum$ModelType<-gsub(folder, "", datasum$ModelType)
datasum$ModelType<-gsub(".*LOSS2CLF", "", datasum$ModelType)
datasum$ModelType<-gsub(".*__", "", datasum$ModelType)
datasum$ModelType<-gsub(".txt.*", "", datasum$ModelType)
y<-as.data.frame(str_match(datasum$ModelType, "^(.*)_(.*)_(.*)$")[,-1],stringsAsFactors=FALSE)
datasum<-cbind(y,datasum[,c(2:21)])
colnames(datasum)<-c('CatalogtypeCode','Modelcode','FinancialPerspective','LOB1','LOB2','LOB3','LOB4','LOB5','LOB6','LOB7',
                     'LOB8','LOB9','LOB10','LOB11','LOB12','LOB13','LOB14','LOB15','LOB16','LOB17','LOB18','LOB19','LOB20'
)
datasum$CatalogtypeCode<-ifelse(substr(datasum$CatalogtypeCode, 3,3)=='R', 'RDS', ifelse(substr(datasum$CatalogtypeCode, 3,3)=='H', 'HIST', 'STC')) 

write.table(datasum, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE, qmethod = "double")
FilesCLFSummer<-datasum

DatabasevsFilescompare<-merge(DatabaseCLFSummer, FilesCLFSummer, by="ModelType", all.x=TRUE)
colnames(DatabasevsFilescompare)<-c('ModelTypeDBtoFIlecompare','LOBSuminDatabaseDBvsfile','LOBSuminFilesDBvsfile')

DatabasevsFilescompare$RelativePercentageDifference2<-abs((DatabasevsFilescompare$LOBSuminDatabase-DatabasevsFilescompare$LOBSuminFiles)*100/DatabasevsFilescompare$LOBSuminFiles)
DatabasevsFilescompare$ResultDBvsFile<- ifelse(DatabasevsFilescompare$RelativePercentageDifference>1|is.na(DatabasevsFilescompare$RelativePercentageDifference), "FAIL", "PASS")


FilesvsDatabasecompare<-merge(DatabaseCLFSummer, FilesCLFSummer, by="ModelType", all.x=TRUE)
colnames(FilesvsDatabasecompare)<-c('ModelTypeFiletoDBcompare','LOBSuminFilesFilevsDB','LOBSuminDatabaseFilevsDB')

FilesvsDatabasecompare$RelativePercentageDifference1<-abs((FilesvsDatabasecompare$LOBSuminDatabase-FilesvsDatabasecompare$LOBSuminFiles)*100/FilesvsDatabasecompare$LOBSuminFiles)
FilesvsDatabasecompare$ResultFilevsDB<- ifelse(FilesvsDatabasecompare$RelativePercentageDifference>1|is.na(FilesvsDatabasecompare$RelativePercentageDifference), "FAIL", "PASS")
FinalCRFCLFDBcompare<-data.frame(rbind.fill(FilesvsDatabasecompare,DatabasevsFilescompare))

FinalCRFCLFDBcompare[,c(1,6)]<- data.frame(lapply(FinalCRFCLFDBcompare[,c(1,6)], as.character), stringsAsFactors=FALSE)

FinalCRFCLFDBcompare[is.na(FinalCRFCLFDBcompare)] <- ' '
write.table(FinalCRFCLFDBcompare, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE, qmethod = "double")
