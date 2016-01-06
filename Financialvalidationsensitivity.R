################################# LIST of All Packages required to Install ###########################################################
list.of.packages <- c("RODBC", "getopt", "optparse","ggplot2","rCharts","slidify", "slidifyLibraries", "rCharts","plotly","googleVis")
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






###### Required Inputs Passed through automation into the script- For automation use###############################################
resultdb<-inputs$args[1]
analysisname<-inputs$args[2]
server<-inputs$args[3]
ResultsPathnew<-inputs$args[4]
TestcaseIDnew <-inputs$args[5]
ValidationTypeName<-inputs$args[6]
TSVernew <-inputs$args[7]
TSIRnew <-inputs$args[8]
ValidationIDnew = inputs$args[9]
# 
# resultdb<-'Layer_Validation_Loss'
# analysisname<-'Layer_validation_v - Loss Analysis'
# server<-'qa-ngp-a-db1'
# ResultsPathnew<-'//qafile2/Leonardo/Feature Data/ValidationUnitetest_vasista/FinancialSensitivity'
# TestcaseIDnew <-1
# ValidationTypeName<-2
# TSVernew <-3
# TSIRnew <-4
# ValidationIDnew = 5

########################################Connection to database server##################################################################

dbhandle <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s;UID=sa;PWD=Clasic22", server))

########################################Dynamical use of result SID from database related to the analysis user ran#####################
sql_query2 <-paste("SELECT convert(varchar(100),resultsid)  from [",resultdb,"].[dbo].[tAnalysisResult] where analysisname='",analysisname,"'",sep='')
print(sql_query2)

#########################################Assigning result SID to an object#############################################################
resultsid<- as.character(sqlQuery(dbhandle,sql_query2) )
print(resultsid)

#################################### Financial Columns in the analysis to be used in the charts dynamically by passing to object ###################################


FinanP1<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedLoss'",sep=""), stringsAsFactors=FALSE)
FinanP2<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='Grounduploss'",sep=""), stringsAsFactors=FALSE)
FinanP3<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss'",sep=""), stringsAsFactors=FALSE)
FinanP4<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss    '",sep=""), stringsAsFactors=FALSE )
FinanP5<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PostCATNetLoss'",sep=""), stringsAsFactors=FALSE )
FinanP6<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossLoss'",sep=""), stringsAsFactors=FALSE )
FinanP7<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='TotalPerRiskReRecoveryLoss'",sep=""), stringsAsFactors=FALSE )
FinanP8<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpSD'",sep=""), stringsAsFactors=FALSE )
FinanP9<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedSD'",sep=""), stringsAsFactors=FALSE)
FinanP10<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossSD'",sep=""), stringsAsFactors=FALSE )
FinanP11<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossSD'",sep=""), stringsAsFactors=FALSE )
FinanP12<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpMaxLoss'",sep=""), stringsAsFactors=FALSE)
FinanP13<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedMaxLoss'",sep=""), stringsAsFactors=FALSE )
FinanP14<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossMaxLoss'",sep=""), stringsAsFactors=FALSE)
FinanP15<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossMaxLoss'",sep=""), stringsAsFactors=FALSE)
FinanP16<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_Minor'",sep=""), stringsAsFactors=FALSE )
FinanP17<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_Moderate'",sep=""), stringsAsFactors=FALSE )
FinanP18<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_Major'",sep=""), stringsAsFactors=FALSE )
FinanP19<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_Fatal'",sep=""), stringsAsFactors=FALSE )
FinanP20<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_Minor'",sep=""), stringsAsFactors=FALSE )
FinanP21<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_Moderate'",sep=""), stringsAsFactors=FALSE )
FinanP22<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_Major'",sep=""), stringsAsFactors=FALSE )
FinanP23<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_Fatal'",sep=""), stringsAsFactors=FALSE )
FinanP24<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_Minor'",sep=""), stringsAsFactors=FALSE )
FinanP25<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_Moderate'",sep=""), stringsAsFactors=FALSE )
FinanP26<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_Major'",sep=""), stringsAsFactors=FALSE )
FinanP27<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_Fatal'",sep=""), stringsAsFactors=FALSE )
FinanP28<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PostCATNetLoss_Minor'",sep=""), stringsAsFactors=FALSE )
FinanP29<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PostCATNetLoss_Moderate'",sep=""), stringsAsFactors=FALSE )
FinanP30<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PostCATNetLoss_Major'",sep=""), stringsAsFactors=FALSE )
FinanP31<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PostCATNetLoss_Fatal'",sep=""), stringsAsFactors=FALSE )
FinanP32<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='InjuryCount_Minor'",sep=""), stringsAsFactors=FALSE )
FinanP33<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='InjuryCount_Moderate'",sep=""), stringsAsFactors=FALSE )
FinanP34<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='InjuryCount_Major'",sep=""), stringsAsFactors=FALSE )
FinanP35<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='InjuryCount_Fatal'",sep=""), stringsAsFactors=FALSE )
FinanP36<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_A'",sep=""), stringsAsFactors=FALSE )
FinanP37<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP38<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP39<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP40<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedLoss_A'",sep=""), stringsAsFactors=FALSE )
FinanP41<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP42<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP43<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP44<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossLoss_A'",sep=""), stringsAsFactors=FALSE )
FinanP45<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP46<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP47<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP48<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_A'",sep=""), stringsAsFactors=FALSE )
FinanP49<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP50<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP51<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP52<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_A'",sep=""), stringsAsFactors=FALSE)
FinanP53<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP54<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP55<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='NetOfPreCATLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP56<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpSD_A'",sep=""), stringsAsFactors=FALSE )
FinanP57<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpSD_B'",sep=""), stringsAsFactors=FALSE)
FinanP58<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpSD_C'",sep=""), stringsAsFactors=FALSE )
FinanP59<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpSD_D'",sep=""), stringsAsFactors=FALSE )
FinanP60<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedSD_A'",sep=""), stringsAsFactors=FALSE )
FinanP61<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedSD_B'",sep=""), stringsAsFactors=FALSE )
FinanP62<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedSD_C'",sep=""), stringsAsFactors=FALSE )
FinanP63<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedSD_D'",sep=""), stringsAsFactors=FALSE )
FinanP64<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossSD_A'",sep=""), stringsAsFactors=FALSE )
FinanP65<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossSD_B'",sep=""), stringsAsFactors=FALSE )
FinanP66<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossSD_C'",sep=""), stringsAsFactors=FALSE )
FinanP67<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossSD_D'",sep=""), stringsAsFactors=FALSE )
FinanP68<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossSD_A'",sep=""), stringsAsFactors=FALSE )
FinanP69<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossSD_B'",sep=""), stringsAsFactors=FALSE)
FinanP70<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossSD_C'",sep=""), stringsAsFactors=FALSE )
FinanP71<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossSD_D'",sep=""), stringsAsFactors=FALSE )
FinanP72<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpMaxLoss_A'",sep=""), stringsAsFactors=FALSE )
FinanP73<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpMaxLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP74<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpMaxLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP75<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GroundUpMaxLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP76<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedMaxLoss_A'",sep=""), stringsAsFactors=FALSE)
FinanP77<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedMaxLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP78<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedMaxLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP79<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='RetainedMaxLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP80<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossMaxLoss_A'",sep=""), stringsAsFactors=FALSE )
FinanP81<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossMaxLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP82<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossMaxLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP83<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='PreLayerGrossMaxLoss_D'",sep=""), stringsAsFactors=FALSE )
FinanP84<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossMaxLoss_A'",sep=""), stringsAsFactors=FALSE )
FinanP85<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossMaxLoss_B'",sep=""), stringsAsFactors=FALSE )
FinanP86<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossMaxLoss_C'",sep=""), stringsAsFactors=FALSE )
FinanP87<-sqlQuery(dbhandle, paste("SELECT COLUMN_NAME FROM ",resultdb,".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 't",resultsid,"_LOSS_ByContract' and TABLE_CATALOG='",resultdb,"' and COLUMN_NAME='GrossMaxLoss_D'",sep=""), stringsAsFactors=FALSE )

newFinanP1='NULL'; try(if((FinanP1)==''){newFinanP1<-NA;}else{ newFinanP1<-paste("a.",(FinanP1),sep='');},silent = TRUE)
newFinanP2='NULL'; try(if((FinanP2)==''){newFinanP2<-NA;}else{ newFinanP2<-paste("a.",(FinanP2),sep='');},silent = TRUE)
newFinanP3='NULL'; try(if((FinanP3)==''){newFinanP3<-NA;}else{ newFinanP3<-paste("a.",(FinanP3),sep='');},silent = TRUE)
newFinanP4='NULL'; try(if((FinanP4)==''){newFinanP4<-NA;}else{ newFinanP4<-paste("a.",(FinanP4),sep='');},silent = TRUE)
newFinanP5='NULL'; try(if((FinanP5)==''){newFinanP5<-NA;}else{ newFinanP5<-paste("a.",(FinanP5),sep='');},silent = TRUE)
newFinanP6='NULL'; try(if((FinanP6)==''){newFinanP6<-NA;}else{ newFinanP6<-paste("a.",(FinanP6),sep='');},silent = TRUE)
newFinanP7='NULL'; try(if((FinanP7)==''){newFinanP7<-NA;}else{ newFinanP7<-paste("a.",(FinanP7),sep='');},silent = TRUE)
newFinanP8='NULL'; try(if((FinanP8)==''){newFinanP8<-NA;}else{ newFinanP8<-paste("a.",(FinanP8),sep='');},silent = TRUE)
newFinanP9='NULL'; try(if((FinanP9)==''){newFinanP9<-NA;}else{ newFinanP9<-paste("a.",(FinanP9),sep='');},silent = TRUE)
newFinanP10='NULL'; try(if((FinanP10)==''){newFinanP10<-NA;}else{ newFinanP10<-paste("a.",(FinanP10),sep='');},silent = TRUE)
newFinanP11='NULL'; try(if((FinanP11)==''){newFinanP11<-NA;}else{ newFinanP11<-paste("a.",(FinanP11),sep='');},silent = TRUE)
newFinanP12='NULL'; try(if((FinanP12)==''){newFinanP12<-NA;}else{ newFinanP12<-paste("a.",(FinanP12),sep='');},silent = TRUE)
newFinanP13='NULL'; try(if((FinanP13)==''){newFinanP13<-NA;}else{ newFinanP13<-paste("a.",(FinanP13),sep='');},silent = TRUE)
newFinanP14='NULL'; try(if((FinanP14)==''){newFinanP14<-NA;}else{ newFinanP14<-paste("a.",(FinanP14),sep='');},silent = TRUE)
newFinanP15='NULL'; try(if((FinanP15)==''){newFinanP15<-NA;}else{ newFinanP15<-paste("a.",(FinanP15),sep='');},silent = TRUE)
newFinanP16='NULL'; try(if((FinanP16)==''){newFinanP16<-NA;}else{ newFinanP16<-paste("a.",(FinanP16),sep='');},silent = TRUE)
newFinanP17='NULL'; try(if((FinanP17)==''){newFinanP17<-NA;}else{ newFinanP17<-paste("a.",(FinanP17),sep='');},silent = TRUE)
newFinanP18='NULL'; try(if((FinanP18)==''){newFinanP18<-NA;}else{ newFinanP18<-paste("a.",(FinanP18),sep='');},silent = TRUE)
newFinanP19='NULL'; try(if((FinanP19)==''){newFinanP19<-NA;}else{ newFinanP19<-paste("a.",(FinanP19),sep='');},silent = TRUE)
newFinanP20='NULL'; try(if((FinanP20)==''){newFinanP20<-NA;}else{ newFinanP20<-paste("a.",(FinanP20),sep='');},silent = TRUE)
newFinanP21='NULL'; try(if((FinanP21)==''){newFinanP21<-NA;}else{ newFinanP21<-paste("a.",(FinanP21),sep='');},silent = TRUE)
newFinanP22='NULL'; try(if((FinanP22)==''){newFinanP22<-NA;}else{ newFinanP22<-paste("a.",(FinanP22),sep='');},silent = TRUE)
newFinanP23='NULL'; try(if((FinanP23)==''){newFinanP23<-NA;}else{ newFinanP23<-paste("a.",(FinanP23),sep='');},silent = TRUE)
newFinanP24='NULL'; try(if((FinanP24)==''){newFinanP24<-NA;}else{ newFinanP24<-paste("a.",(FinanP24),sep='');},silent = TRUE)
newFinanP25='NULL'; try(if((FinanP25)==''){newFinanP25<-NA;}else{ newFinanP25<-paste("a.",(FinanP25),sep='');},silent = TRUE)
newFinanP26='NULL'; try(if((FinanP26)==''){newFinanP26<-NA;}else{ newFinanP26<-paste("a.",(FinanP26),sep='');},silent = TRUE)
newFinanP27='NULL'; try(if((FinanP27)==''){newFinanP27<-NA;}else{ newFinanP27<-paste("a.",(FinanP27),sep='');},silent = TRUE)
newFinanP28='NULL'; try(if((FinanP28)==''){newFinanP28<-NA;}else{ newFinanP28<-paste("a.",(FinanP28),sep='');},silent = TRUE)
newFinanP29='NULL'; try(if((FinanP29)==''){newFinanP29<-NA;}else{ newFinanP29<-paste("a.",(FinanP29),sep='');},silent = TRUE)
newFinanP30='NULL'; try(if((FinanP30)==''){newFinanP30<-NA;}else{ newFinanP30<-paste("a.",(FinanP30),sep='');},silent = TRUE)
newFinanP31='NULL'; try(if((FinanP31)==''){newFinanP31<-NA;}else{ newFinanP31<-paste("a.",(FinanP31),sep='');},silent = TRUE)
newFinanP32='NULL'; try(if((FinanP32)==''){newFinanP32<-NA;}else{ newFinanP32<-paste("a.",(FinanP32),sep='');},silent = TRUE)
newFinanP33='NULL'; try(if((FinanP33)==''){newFinanP33<-NA;}else{ newFinanP33<-paste("a.",(FinanP33),sep='');},silent = TRUE)
newFinanP34='NULL'; try(if((FinanP34)==''){newFinanP34<-NA;}else{ newFinanP34<-paste("a.",(FinanP34),sep='');},silent = TRUE)
newFinanP35='NULL'; try(if((FinanP35)==''){newFinanP35<-NA;}else{ newFinanP35<-paste("a.",(FinanP35),sep='');},silent = TRUE)
newFinanP36='NULL'; try(if((FinanP36)==''){newFinanP36<-NA;}else{ newFinanP36<-paste("a.",(FinanP36),sep='');},silent = TRUE)
newFinanP37='NULL'; try(if((FinanP37)==''){newFinanP37<-NA;}else{ newFinanP37<-paste("a.",(FinanP37),sep='');},silent = TRUE)
newFinanP38='NULL'; try(if((FinanP38)==''){newFinanP38<-NA;}else{ newFinanP38<-paste("a.",(FinanP38),sep='');},silent = TRUE)
newFinanP39='NULL'; try(if((FinanP39)==''){newFinanP39<-NA;}else{ newFinanP39<-paste("a.",(FinanP39),sep='');},silent = TRUE)
newFinanP40='NULL'; try(if((FinanP40)==''){newFinanP40<-NA;}else{ newFinanP40<-paste("a.",(FinanP40),sep='');},silent = TRUE)
newFinanP41='NULL'; try(if((FinanP41)==''){newFinanP41<-NA;}else{ newFinanP41<-paste("a.",(FinanP41),sep='');},silent = TRUE)
newFinanP42='NULL'; try(if((FinanP42)==''){newFinanP42<-NA;}else{ newFinanP42<-paste("a.",(FinanP42),sep='');},silent = TRUE)
newFinanP43='NULL'; try(if((FinanP43)==''){newFinanP43<-NA;}else{ newFinanP43<-paste("a.",(FinanP43),sep='');},silent = TRUE)
newFinanP44='NULL'; try(if((FinanP44)==''){newFinanP44<-NA;}else{ newFinanP44<-paste("a.",(FinanP44),sep='');},silent = TRUE)
newFinanP45='NULL'; try(if((FinanP45)==''){newFinanP45<-NA;}else{ newFinanP45<-paste("a.",(FinanP45),sep='');},silent = TRUE)
newFinanP46='NULL'; try(if((FinanP46)==''){newFinanP46<-NA;}else{ newFinanP46<-paste("a.",(FinanP46),sep='');},silent = TRUE)
newFinanP47='NULL'; try(if((FinanP47)==''){newFinanP47<-NA;}else{ newFinanP47<-paste("a.",(FinanP47),sep='');},silent = TRUE)
newFinanP48='NULL'; try(if((FinanP48)==''){newFinanP48<-NA;}else{ newFinanP48<-paste("a.",(FinanP48),sep='');},silent = TRUE)
newFinanP49='NULL'; try(if((FinanP49)==''){newFinanP49<-NA;}else{ newFinanP49<-paste("a.",(FinanP49),sep='');},silent = TRUE)
newFinanP50='NULL'; try(if((FinanP50)==''){newFinanP50<-NA;}else{ newFinanP50<-paste("a.",(FinanP50),sep='');},silent = TRUE)
newFinanP51='NULL'; try(if((FinanP51)==''){newFinanP51<-NA;}else{ newFinanP51<-paste("a.",(FinanP51),sep='');},silent = TRUE)
newFinanP52='NULL'; try(if((FinanP52)==''){newFinanP52<-NA;}else{ newFinanP52<-paste("a.",(FinanP52),sep='');},silent = TRUE)
newFinanP53='NULL'; try(if((FinanP53)==''){newFinanP53<-NA;}else{ newFinanP53<-paste("a.",(FinanP53),sep='');},silent = TRUE)
newFinanP54='NULL'; try(if((FinanP54)==''){newFinanP54<-NA;}else{ newFinanP54<-paste("a.",(FinanP54),sep='');},silent = TRUE)
newFinanP55='NULL'; try(if((FinanP55)==''){newFinanP55<-NA;}else{ newFinanP55<-paste("a.",(FinanP55),sep='');},silent = TRUE)
newFinanP56='NULL'; try(if((FinanP56)==''){newFinanP56<-NA;}else{ newFinanP56<-paste("a.",(FinanP56),sep='');},silent = TRUE)
newFinanP57='NULL'; try(if((FinanP57)==''){newFinanP57<-NA;}else{ newFinanP57<-paste("a.",(FinanP57),sep='');},silent = TRUE)
newFinanP58='NULL'; try(if((FinanP58)==''){newFinanP58<-NA;}else{ newFinanP58<-paste("a.",(FinanP58),sep='');},silent = TRUE)
newFinanP59='NULL'; try(if((FinanP59)==''){newFinanP59<-NA;}else{ newFinanP59<-paste("a.",(FinanP59),sep='');},silent = TRUE)
newFinanP60='NULL'; try(if((FinanP60)==''){newFinanP60<-NA;}else{ newFinanP60<-paste("a.",(FinanP60),sep='');},silent = TRUE)
newFinanP61='NULL'; try(if((FinanP61)==''){newFinanP61<-NA;}else{ newFinanP61<-paste("a.",(FinanP61),sep='');},silent = TRUE)
newFinanP62='NULL'; try(if((FinanP62)==''){newFinanP62<-NA;}else{ newFinanP62<-paste("a.",(FinanP62),sep='');},silent = TRUE)
newFinanP63='NULL'; try(if((FinanP63)==''){newFinanP63<-NA;}else{ newFinanP63<-paste("a.",(FinanP63),sep='');},silent = TRUE)
newFinanP64='NULL'; try(if((FinanP64)==''){newFinanP64<-NA;}else{ newFinanP64<-paste("a.",(FinanP64),sep='');},silent = TRUE)
newFinanP65='NULL'; try(if((FinanP65)==''){newFinanP65<-NA;}else{ newFinanP65<-paste("a.",(FinanP65),sep='');},silent = TRUE)
newFinanP66='NULL'; try(if((FinanP66)==''){newFinanP66<-NA;}else{ newFinanP66<-paste("a.",(FinanP66),sep='');},silent = TRUE)
newFinanP67='NULL'; try(if((FinanP67)==''){newFinanP67<-NA;}else{ newFinanP67<-paste("a.",(FinanP67),sep='');},silent = TRUE)
newFinanP68='NULL'; try(if((FinanP68)==''){newFinanP68<-NA;}else{ newFinanP68<-paste("a.",(FinanP68),sep='');},silent = TRUE)
newFinanP69='NULL'; try(if((FinanP69)==''){newFinanP69<-NA;}else{ newFinanP69<-paste("a.",(FinanP69),sep='');},silent = TRUE)
newFinanP70='NULL'; try(if((FinanP70)==''){newFinanP70<-NA;}else{ newFinanP70<-paste("a.",(FinanP70),sep='');},silent = TRUE)
newFinanP71='NULL'; try(if((FinanP71)==''){newFinanP71<-NA;}else{ newFinanP71<-paste("a.",(FinanP71),sep='');},silent = TRUE)
newFinanP72='NULL'; try(if((FinanP72)==''){newFinanP72<-NA;}else{ newFinanP72<-paste("a.",(FinanP72),sep='');},silent = TRUE)
newFinanP73='NULL'; try(if((FinanP73)==''){newFinanP73<-NA;}else{ newFinanP73<-paste("a.",(FinanP73),sep='');},silent = TRUE)
newFinanP74='NULL'; try(if((FinanP74)==''){newFinanP74<-NA;}else{ newFinanP74<-paste("a.",(FinanP74),sep='');},silent = TRUE)
newFinanP75='NULL'; try(if((FinanP75)==''){newFinanP75<-NA;}else{ newFinanP75<-paste("a.",(FinanP75),sep='');},silent = TRUE)
newFinanP76='NULL'; try(if((FinanP76)==''){newFinanP76<-NA;}else{ newFinanP76<-paste("a.",(FinanP76),sep='');},silent = TRUE)
newFinanP77='NULL'; try(if((FinanP77)==''){newFinanP77<-NA;}else{ newFinanP77<-paste("a.",(FinanP77),sep='');},silent = TRUE)
newFinanP78='NULL'; try(if((FinanP78)==''){newFinanP78<-NA;}else{ newFinanP78<-paste("a.",(FinanP78),sep='');},silent = TRUE)
newFinanP79='NULL'; try(if((FinanP79)==''){newFinanP79<-NA;}else{ newFinanP79<-paste("a.",(FinanP79),sep='');},silent = TRUE)
newFinanP80='NULL'; try(if((FinanP80)==''){newFinanP80<-NA;}else{ newFinanP80<-paste("a.",(FinanP80),sep='');},silent = TRUE)
newFinanP81='NULL'; try(if((FinanP81)==''){newFinanP81<-NA;}else{ newFinanP81<-paste("a.",(FinanP81),sep='');},silent = TRUE)
newFinanP82='NULL'; try(if((FinanP82)==''){newFinanP82<-NA;}else{ newFinanP82<-paste("a.",(FinanP82),sep='');},silent = TRUE)
newFinanP83='NULL'; try(if((FinanP83)==''){newFinanP83<-NA;}else{ newFinanP83<-paste("a.",(FinanP83),sep='');},silent = TRUE)
newFinanP84='NULL'; try(if((FinanP84)==''){newFinanP84<-NA;}else{ newFinanP84<-paste("a.",(FinanP84),sep='');},silent = TRUE)
newFinanP85='NULL'; try(if((FinanP85)==''){newFinanP85<-NA;}else{ newFinanP85<-paste("a.",(FinanP85),sep='');},silent = TRUE)
newFinanP86='NULL'; try(if((FinanP86)==''){newFinanP86<-NA;}else{ newFinanP86<-paste("a.",(FinanP86),sep='');},silent = TRUE)
newFinanP87<-'NULL' ;try(if((FinanP87)==''){newFinanP87<-NA;}else{ newFinanP87<-paste("a.",(FinanP87),sep='');},silent = TRUE)


##################################### SQL Query to create a data frame with the contractID and the finacials in the result Database################################################################


sql_query1 <-paste("SELECT distinct b.Contractid,",newFinanP1,",",newFinanP2,",", newFinanP3 ,",",newFinanP4 ,",",
                   newFinanP5 ,",",
                   newFinanP6 ,",",
                   newFinanP7,",",
                   newFinanP8,",",
                   newFinanP9,",",
                   newFinanP10,",",
                   newFinanP11,",",
                   newFinanP12,",",
                   newFinanP13,",",
                   newFinanP14,",",
                   newFinanP15,",",
                   newFinanP16,",",
                   newFinanP17,",",
                   newFinanP18,",",
                   newFinanP19,",",
                   newFinanP20,",",
                   newFinanP21,",",
                   newFinanP22,",",
                   newFinanP23,",",
                   newFinanP24,",",
                   newFinanP25,",",
                   newFinanP26,",",
                   newFinanP27,",",
                   newFinanP28,",",
                   newFinanP29,",",
                   newFinanP30,",",
                   newFinanP31,",",
                   newFinanP32,",",
                   newFinanP33,",",
                   newFinanP34,",",
                   newFinanP35,",",
                   newFinanP36,",",
                   newFinanP37,",",
                   newFinanP38,",",
                   newFinanP39,",",
                   newFinanP40,",",
                   newFinanP41,",",
                   newFinanP42,",",
                   newFinanP43,",",
                   newFinanP44,",",
                   newFinanP45,",",
                   newFinanP46,",",
                   newFinanP47,",",
                   newFinanP48,",",
                   newFinanP49,",",
                   newFinanP50,",",
                   newFinanP51,",",
                   newFinanP52,",",
                   newFinanP53,",",
                   newFinanP54,",",
                   newFinanP55,",",
                   newFinanP56,",",
                   newFinanP57,",",
                   newFinanP58,",",
                   newFinanP59,",",
                   newFinanP60,",",
                   newFinanP61,",",
                   newFinanP62,",",
                   newFinanP63,",",
                   newFinanP64,",",
                   newFinanP65,",",
                   newFinanP66,",",
                   newFinanP67,",",
                   newFinanP68,",",
                   newFinanP69,",",
                   newFinanP70,",",
                   newFinanP71,",",
                   newFinanP72,",",
                   newFinanP73,",",
                   newFinanP74,",",
                   newFinanP75,",",
                   newFinanP76,",",
                   newFinanP77,",",
                   newFinanP78,",",
                   newFinanP79,",",
                   newFinanP80,",",
                   newFinanP81,",",
                   newFinanP82,",",
                   newFinanP83,",",
                   newFinanP84,",",
                   newFinanP85,",",
                   newFinanP86,",", newFinanP87, " FROM [",resultdb,"].[dbo].[t",resultsid,"_LOSS_ByContract] a join [",resultdb,"]..[t",resultsid,"_LOSS_DimContract] b  on a.contractsid=b.contractsid   WHERE EventID = (SELECT TOP 1 EventID FROM [",resultdb,"].[dbo].[t",resultsid,"_LOSS_ByEvent] ORDER BY GroundUpLoss asc )",sep='')



###############################################Passing the dataFrame to an object#######################################################
LowerLossEvent_FinacialSensitivity_Layer_1 <- data.frame(sqlQuery(dbhandle,sql_query1, stringsAsFactors = FALSE))  

###############################################Assigning columns to the dataframe#######################################################
colnames(LowerLossEvent_FinacialSensitivity_Layer_1)<-c('ContractID','RetainedLoss',	'Grounduploss',	'GrossLoss',	'NetOfPreCATLoss',	'PostCATNetLoss',	'PreLayerGrossLoss',	'TotalPerRiskReRecoveryLoss',	'GroundUpSD',	'RetainedSD',	'PreLayerGrossSD',	'GrossSD',	'GroundUpMaxLoss',	'RetainedMaxLoss',	'PreLayerGrossMaxLoss',	'GrossMaxLoss',	'GroundUpLoss_Minor',	'GroundUpLoss_Moderate',	'GroundUpLoss_Major',	'GroundUpLoss_Fatal',	'GrossLoss_Minor',	'GrossLoss_Moderate',	'GrossLoss_Major',	'GrossLoss_Fatal',	'NetOfPreCATLoss_Minor',	'NetOfPreCATLoss_Moderate',	'NetOfPreCATLoss_Major',	'NetOfPreCATLoss_Fatal',	'PostCATNetLoss_Minor',	'PostCATNetLoss_Moderate',	'PostCATNetLoss_Major',	'PostCATNetLoss_Fatal',	'InjuryCount_Minor',	'InjuryCount_Moderate',	'InjuryCount_Major',	'InjuryCount_Fatal',	'GroundUpLoss_A',	'GroundUpLoss_B',	'GroundUpLoss_C',	'GroundUpLoss_D',	'RetainedLoss_A',	'RetainedLoss_B',	'RetainedLoss_C',	'RetainedLoss_D',	'PreLayerGrossLoss_A',	'PreLayerGrossLoss_B',	'PreLayerGrossLoss_C',	'PreLayerGrossLoss_D',	'GrossLoss_A',	'GrossLoss_B',	'GrossLoss_C',	'GrossLoss_D',	'NetOfPreCATLoss_A',	'NetOfPreCATLoss_B',	'NetOfPreCATLoss_C',	'NetOfPreCATLoss_D',	'GroundUpSD_A',	'GroundUpSD_B',	'GroundUpSD_C',	'GroundUpSD_D',	'RetainedSD_A',	'RetainedSD_B',	'RetainedSD_C',	'RetainedSD_D',	'PreLayerGrossSD_A',	'PreLayerGrossSD_B',	'PreLayerGrossSD_C',	'PreLayerGrossSD_D',	'GrossSD_A',	'GrossSD_B',	'GrossSD_C',	'GrossSD_D',	'GroundUpMaxLoss_A',	'GroundUpMaxLoss_B',	'GroundUpMaxLoss_C',	'GroundUpMaxLoss_D',	'RetainedMaxLoss_A',	'RetainedMaxLoss_B',	'RetainedMaxLoss_C',	'RetainedMaxLoss_D',	'PreLayerGrossMaxLoss_A',	'PreLayerGrossMaxLoss_B',	'PreLayerGrossMaxLoss_C',	'PreLayerGrossMaxLoss_D',	'GrossMaxLoss_A',	'GrossMaxLoss_B',	'GrossMaxLoss_C',	'GrossMaxLoss_D')

########################################### Data Manipulation ########################################################################
c<-data.frame(sub(".*_", "",LowerLossEvent_FinacialSensitivity_Layer_1$ContractID),stringsAsFactors = FALSE)
colnames(c)<-('OrderID')#dat
c<-data.frame(lapply(c, as.numeric))
c <-cbind(LowerLossEvent_FinacialSensitivity_Layer_1,c)
LowerLossEvent_FinacialSensitivity_Layer_1<-c[order(c$OrderID),c(1:88)]
LowerLossEvent_FinacialSensitivity_Layer_1<-data.frame(LowerLossEvent_FinacialSensitivity_Layer_1,row.names=NULL)
LowerLossEvent_FinacialSensitivity_Layer_1[is.na(LowerLossEvent_FinacialSensitivity_Layer_1)]<-""
LowerLossEvent_FinacialSensitivity_Layer_1[,c(2:88)]<-as.numeric(unlist(LowerLossEvent_FinacialSensitivity_Layer_1[,c(2:88)]))
LowerLossEvent_FinacialSensitivity_Layer_1<-data.frame(LowerLossEvent_FinacialSensitivity_Layer_1, stringsAsFactors = FALSE)


############################################ Length to calculate number of rows in dataframe#########################################
n<-nrow(LowerLossEvent_FinacialSensitivity_Layer_1)

############################################# Manipulating the ContractID as a string#################################################
LowerLossEvent_FinacialSensitivity_Layer_1[,1]<-as.character(LowerLossEvent_FinacialSensitivity_Layer_1[,1])



#############################################Looping through all the financial columns to plot the graph with ContractID###############
########Graph is dumped by using certain format ( Resultpath+Testcaseid+validationname+TSversion+IRversion+ValID) to support automation########################

for (i in 2:88)
{
  z<-data.frame(LowerLossEvent_FinacialSensitivity_Layer_1[,c(1,i)]);
  
  if(is.na(z[1,2]))
  {
    
    print("Columns not available in the analysis")
    #write.table(paste("Columns not available in the analysis_",sep=""),file=paste(ResultsPathnew,"/","ErrorLog",".txt",sep=""));
    
    
  } else{ 
    value <- colnames(z);
    
    Line <- gvisLineChart(z,xvar='ContractID',yvar=value[2],
                          options=list(title=paste("Financial Sensitivity","-",value[1],"vs",value[2]),
                                       titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
                                       curveType='function',hAxis.viewWindowMode='explicit',
                                       vAxis.viewWindowMode='explicit', width=800, height=200))
    plot(Line)
    cat(Line$html$chart,file=paste(ResultsPathnew,"/",TestcaseIDnew,"_",ValidationTypeName,"_",TSVernew,"_",TSIRnew,"_",ValidationIDnew,
                                   "_",value[1],"vs",value[2],'.html',sep=""))
  }
  
  
}



#########################Removing the NA and replacing by blanks and dumping the data in a file to be used as base data in QAautodev#########
#####################################################Data is dumped by using certain format to support automation########################

LowerLossEvent_FinacialSensitivity_Layer_1[is.na(LowerLossEvent_FinacialSensitivity_Layer_1)] <- ' '
write.table(LowerLossEvent_FinacialSensitivity_Layer_1, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE, qmethod = "double")
close(dbhandle)



