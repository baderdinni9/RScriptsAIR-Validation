########################################BY LOB CLF Validation#########################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
#############################################Vasista##################################################################################

list.of.packages <- c("futile.logger","RODBC", "getopt", "stringr","optparse","plyr","ggplot2","rCharts","slidify", "slidifyLibraries","bit64", "rCharts","plotly","googleVis","data.table")
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
folder <- inputs$args[4]      # path to folder that holds multiple .csv files
tcid<-  inputs$args[5]
tpid<-  inputs$args[6]

sqlString<-paste('select * from ',resultdb,'..[MastertemptableCLFLOBVal_',ValID,'_',tcid,'_',tpid,']',collapse="",sep="") 
#sqlconn <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s;UID=sa;PWD=Clasic22", server))

driver <- "DRIVER={SQL Server}"
server <- paste0("SERVER=",server)
trustedConn <- "trusted_connection=TRUE"

sqlconn <- odbcDriverConnect(paste(driver,server,trustedConn,sep=";"))

DatabaseCLFSummer<-data.frame(sqlQuery(sqlconn,sqlString))
DatabaseCLFSummer[,c(1,2,3)]<- data.frame(lapply(DatabaseCLFSummer[,c(1,2,3)], as.character), stringsAsFactors=FALSE)
DatabaseCLFSummer$FinancialPerspective<-as.character(DatabaseCLFSummer$FinancialPerspective)


                  trim <- function( x ) {
                    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
                  }
                  
                  
                  DatabaseCLFSummer[,c(1)]<- trim(DatabaseCLFSummer[,1])
                  DatabaseCLFSummer[,c(3)]<- trim(DatabaseCLFSummer[,3])
                  DatabaseCLFSummer[,c(2)]<- trim(DatabaseCLFSummer[,2])

####################################################File Summer#######################################################################


library(data.table)
library(plyr)
library(bit64)
library(stringr)

file_list <- list.files(path=folder, pattern=paste(tcid,"_LOSS2CLF_",ValID,".*",".txt",sep="")) # create list of all .csv files in folder

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
folder <- gsub("\\", "/", folder,fixed=TRUE)
datasum$ModelType<-gsub(folder, "", datasum$ModelType)
datasum$ModelType<-gsub(".*LOSS2CLF", "", datasum$ModelType)
datasum$ModelType<-gsub(".*__", "", datasum$ModelType)
datasum$ModelType<-gsub(".txt.*", "", datasum$ModelType)

y<-strsplit(datasum$ModelType, "_")
y <- ldply(y)
datasum<-cbind(y,datasum[,c(2:21)])

colnames(datasum)<-c('CatalogtypeCode','Modelcode','FinancialPerspective','LOB1','LOB2','LOB3','LOB4','LOB5','LOB6','LOB7',
                     'LOB8','LOB9','LOB10','LOB11','LOB12','LOB13','LOB14','LOB15','LOB16','LOB17','LOB18','LOB19','LOB20'
)
datasum$CatalogtypeCode<-ifelse(substr(datasum$CatalogtypeCode, 3,3)=='R', 'RDS', ifelse(substr(datasum$CatalogtypeCode, 3,3)=='H', 'HIST', 'STC')) 


FilesCLFSummer<-datasum


DatabasevsFilescompare<-merge(DatabaseCLFSummer, FilesCLFSummer, by=c("CatalogtypeCode","Modelcode","FinancialPerspective"), all.x=TRUE)
colnames(DatabasevsFilescompare)<-c('CatalogtypeCode',	'Modelcode',	'FinancialPerspective',	'LOB1AllDBLosses',	'LOB2AllDBLosses',	'LOB3AllDBLosses',	'LOB4AllDBLosses',	'LOB5AllDBLosses',	'LOB6AllDBLosses',	'LOB7AllDBLosses',	'LOB8AllDBLosses',	'LOB9AllDBLosses',	'LOB10AllDBLosses',	'LOB11AllDBLosses',	'LOB12AllDBLosses',	'LOB13AllDBLosses',	'LOB14AllDBLosses',	'LOB15AllDBLosses',	'LOB16AllDBLosses',	'LOB17AllDBLosses',	'LOB18AllDBLosses',	'LOB19AllDBLosses',	'LOB20AllDBLosses',	'LOB1AllFilesLosses',	'LOB2AllFilesLosses',	'LOB3AllFilesLosses',	'LOB4AllFilesLosses',	'LOB5AllFilesLosses',	'LOB6AllFilesLosses',	'LOB7AllFilesLosses',	'LOB8AllFilesLosses',	'LOB9AllFilesLosses',	'LOB10AllFilesLosses',	'LOB11AllFilesLosses',	'LOB12AllFilesLosses',	'LOB13AllFilesLosses',	'LOB14AllFilesLosses',	'LOB15AllFilesLosses',	'LOB16AllFilesLosses',	'LOB17AllFilesLosses',	'LOB18AllFilesLosses',	'LOB19AllFilesLosses',	'LOB20AllFilesLosses')

RelativeDifferenceDBFIle<-data.frame(matrix(ncol=10, nrow=nrow(DatabasevsFilescompare)))
DatabasevsFilescompare[is.na(DatabasevsFilescompare)] <-0

for (j in 1:20)
{
  
  for(i in 1:nrow(DatabasevsFilescompare))
  {
    if(DatabasevsFilescompare[i,(j+3)]==0){
      data <- abs((DatabasevsFilescompare[i,(j+3)]-DatabasevsFilescompare[i,(j+23)])*100);
    }else
    {
      data <- abs((DatabasevsFilescompare[i,(j+3)]-DatabasevsFilescompare[i,(j+23)])*100/DatabasevsFilescompare[i,(j+3)]);
    }
    
    RelativeDifferenceDBFIle[i,j]<-ifelse(data>1|is.na(data), "FAIL", "PASS")
    
  }
}

colnames(RelativeDifferenceDBFIle)<-c('Diff1',	'Diff2',	'Diff3',	'Diff4',	'Diff5',	'Diff6',	'Diff7',	'Diff8',	'Diff9',	'Diff10',	'Diff11',	'Diff12',	'Diff13',	'Diff14',	'Diff15',	'Diff16',	'Diff17',	'Diff18',	'Diff19',	'Diff20')

FilesvsDatabasecompare<-merge(DatabaseCLFSummer, FilesCLFSummer, by=c("CatalogtypeCode","Modelcode","FinancialPerspective"), all.y=TRUE)
colnames(FilesvsDatabasecompare)<-c('CatalogtypeCode',	'Modelcode',	'FinancialPerspective',	'LOB1AllDBLosses',	'LOB2AllDBLosses',	'LOB3AllDBLosses',	'LOB4AllDBLosses',	'LOB5AllDBLosses',	'LOB6AllDBLosses',	'LOB7AllDBLosses',	'LOB8AllDBLosses',	'LOB9AllDBLosses',	'LOB10AllDBLosses',	'LOB11AllDBLosses',	'LOB12AllDBLosses',	'LOB13AllDBLosses',	'LOB14AllDBLosses',	'LOB15AllDBLosses',	'LOB16AllDBLosses',	'LOB17AllDBLosses',	'LOB18AllDBLosses',	'LOB19AllDBLosses',	'LOB20AllDBLosses',	'LOB1AllFilesLosses',	'LOB2AllFilesLosses',	'LOB3AllFilesLosses',	'LOB4AllFilesLosses',	'LOB5AllFilesLosses',	'LOB6AllFilesLosses',	'LOB7AllFilesLosses',	'LOB8AllFilesLosses',	'LOB9AllFilesLosses',	'LOB10AllFilesLosses',	'LOB11AllFilesLosses',	'LOB12AllFilesLosses',	'LOB13AllFilesLosses',	'LOB14AllFilesLosses',	'LOB15AllFilesLosses',	'LOB16AllFilesLosses',	'LOB17AllFilesLosses',	'LOB18AllFilesLosses',	'LOB19AllFilesLosses',	'LOB20AllFilesLosses')

RelativeDifferenceFIleDB<-data.frame(matrix(ncol=10, nrow=nrow(FilesvsDatabasecompare)))
FilesvsDatabasecompare[is.na(FilesvsDatabasecompare)] <-0

for (j in 1:20)
{
  
  for(i in 1:nrow(FilesvsDatabasecompare))
  {
    if(FilesvsDatabasecompare[i,(j+3)]==0)
    {
      data <- abs((FilesvsDatabasecompare[i,(j+3)]-FilesvsDatabasecompare[i,(j+23)])*100) 
    }
    # FilesvsDatabasecompare[i,(j+3)][is.na(FilesvsDatabasecompare[i,(j+3)])] <- ' '
    else{
      data <- abs((FilesvsDatabasecompare[i,(j+3)]-FilesvsDatabasecompare[i,(j+23)])*100/FilesvsDatabasecompare[i,(j+3)]) 
    }
    
    RelativeDifferenceFIleDB[i,j]<-ifelse(data>1|is.na(data), "FAIL", "PASS")
    
  }
}


colnames(RelativeDifferenceFIleDB)<-c('Diff1',	'Diff2',	'Diff3',	'Diff4',	'Diff5',	'Diff6',	'Diff7',	'Diff8',	'Diff9',	'Diff10',	'Diff11',	'Diff12',	'Diff13',	'Diff14',	'Diff15',	'Diff16',	'Diff17',	'Diff18',	'Diff19',	'Diff20')

FilesvsDatabasecompare<-cbind(FilesvsDatabasecompare,RelativeDifferenceFIleDB)
DatabasevsFilescompare<-cbind(DatabasevsFilescompare,RelativeDifferenceDBFIle)


FinalCRFCLFDBcompare<-data.frame(rbind.fill(FilesvsDatabasecompare,DatabasevsFilescompare))

FinalCRFCLFDBcompare[,c(1,43)]<- data.frame(lapply(FinalCRFCLFDBcompare[,c(1,43)], as.character), stringsAsFactors=FALSE)

FinalCRFCLFDBcompare[is.na(FinalCRFCLFDBcompare)] <- ' '
colnames(FinalCRFCLFDBcompare)
write.table(FinalCRFCLFDBcompare, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE, qmethod = "double")
a<- as.character(flog.info(print(paste("My result path:",folder,"My ValidationReport:",outfile,"List of CLF's:",file_list,sep='\n'))))  
writeLines(a, paste(folder,'/ValidationLogs','_',ValID,'.txt',sep = ''))
