
folder <- "//qafile2/Leonardo/Feature Data/CLF Exports/Testresults/rc-2/M60rerunfinal/"      # path to folder that holds multiple .csv files
folder1<-"//qafile2/Leonardo/Feature Data/CLF Exports/Testresults/rc-2/M60rerunfinal/CRFHIST.csv"
CRFSTC<-function(folder,folder1)
{
library(xlsx)
file_list <- list.files(path=folder, pattern="*Validation_HIST") # create list of all .csv files in folder

# read in each .csv file in file_list and rbind them into a data frame called data 
data <- 
  data.frame(do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   read.csv(paste(folder, x, sep=''), 
                            stringsAsFactors = FALSE))))
#data[data==""] <- NA
#data <- na.omit(data)

write.table(data,folder1,row.names=F) 
}

CRFSTC(folder,folder1)


folder <- "//qafile2/Leonardo/Feature Data/CLF Exports/Testresults/rc-2/M60rerunfinal/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*Validation_STC") # create list of all .csv files in folder


#read in each .csv file in file_list and rbind them into a data frame called data 
data <- 
  data.frame(do.call("rbind", 
                     lapply(file_list, 
                            function(x) 
                              read.csv(paste(folder, x, sep=''), 
                                       stringsAsFactors = FALSE))))
# data[data==""] <- NA
# data <- na.omit(data)

write.table(data,"//qafile2/Leonardo/Feature Data/CLF Exports/Testresults/rc-2/M60rerunfinal/CRFSTC.csv",row.names=F) 


# 
# library(xlsx)
# library("RODBC")
# library(compare)
# library(gdata)   
# library(XLConnect)  
# require(xlsx)  
# library("openxlsx")
# library(data.table)
# require(bit64)
# 
# folder <- "//qafile2/Leonardo/Feature Data/Geo&Injury/TestResults/RC2/"      # path to folder that holds multiple .csv files
# file_list <- list.files(path=folder, pattern="*GeographyInjury") # create list of all .csv files in folder
# 
# # read in each .csv file in file_list and rbind them into a data frame called data 
# data <- 
#   data.frame(do.call("rbind", 
#                      lapply(file_list, 
#                             function(x) 
#                               read.csv(paste(folder, x, sep='') 
#                                        ,fileEncoding="UTF-16LE",fill=TRUE,header=T,stringsAsFactors = FALSE,row.names=NULL))))
# 
# 
# write.table(data,"CRF.csv",row.names=F) 
