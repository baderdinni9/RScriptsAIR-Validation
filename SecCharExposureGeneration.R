library(RODBC)
library(reshape2)
resultdb<-'SecCharDB'
server<-'qa-ts-153-db1\\sql2014'

#########################################################################################################################
sqlString<-paste('select * from SecCharDB..CON_OCC',collapse="",sep="") 
#sqlconn <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s;UID=sa;PWD=Clasic22", server))

driver <- "DRIVER={SQL Server}"
server <- paste0("SERVER=",server)
trustedConn <- "trusted_connection=TRUE"

sqlconn <- odbcDriverConnect(paste(driver,server,trustedConn,sep=";"))
conocc<-data.frame(sqlQuery(sqlconn,sqlString))

#########################################################################################################################
server1<-'qa-ts-153-db1\\sql2014'
sqlString<-paste('select * from SecCharDB..Sec_cHAR',collapse="",sep="") 
#sqlconn <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s;UID=sa;PWD=Clasic22", server))

driver <- "DRIVER={SQL Server}"
server1 <- paste0("SERVER=",server1)
trustedConn <- "trusted_connection=TRUE"

sqlconn <- odbcDriverConnect(paste(driver,server1,trustedConn,sep=";"))
sechar<-data.frame(sqlQuery(sqlconn,sqlString))
conoccnew <- data.frame(paste (conocc[,1],',',conocc[,2]),stringsAsFactors = FALSE)

#########################################################################################################################
sqlString<-paste('select * from SecCharDB..AgeBand',collapse="",sep="") 
#sqlconn <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s;UID=sa;PWD=Clasic22", server))
server2<-'qa-ts-153-db1\\sql2014'
driver <- "DRIVER={SQL Server}"
server2 <- paste0("SERVER=",server2)
trustedConn <- "trusted_connection=TRUE"

sqlconn <- odbcDriverConnect(paste(driver,server2,trustedConn,sep=";"))
AgeBand<-data.frame(sqlQuery(sqlconn,sqlString))



#########################################################################################################################
sqlString<-paste('select * from SecCharDB..HeightBand',collapse="",sep="") 
#sqlconn <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s;UID=sa;PWD=Clasic22", server))
server3<-'qa-ts-153-db1\\sql2014'
driver <- "DRIVER={SQL Server}"
server3 <- paste0("SERVER=",server3)
trustedConn <- "trusted_connection=TRUE"

sqlconn <- odbcDriverConnect(paste(driver,server3,trustedConn,sep=";"))
HeightBand<-data.frame(sqlQuery(sqlconn,sqlString))


#########################################################################################################################
tmp4 <- data.frame(expand.grid(sapply(conoccnew, as.character),sapply(AgeBand, as.character)))
tmp4 <- data.frame(apply(tmp4, 1, paste, collapse=","))
tmp4 <- data.frame(expand.grid(sapply(tmp4, as.character),sapply(HeightBand, as.character)))
tmp4 <- data.frame(apply(tmp4, 1, paste, collapse=","),stringsAsFactors = FALSE)

#########################################################################################################################

tmp3 <- data.frame(apply(sechar, 1, paste, collapse=","),stringsAsFactors = FALSE)
names(tmp4) <- names(tmp3)

d<-data.frame(expand.grid(sapply(tmp3, as.character),sapply(tmp4, as.character)))
d <- data.frame(apply(d, 1, paste, collapse=","))

colnames(d) <- c("ID,FloorsOccupied,CustomFloodSOP,CustomFloodZone,FloorOfInterest,BuildingCondition,BuildingShape,Torsion,SoftStory,ShapeIrregularity,SpecialConstruction,Retrofit,ShortColumn,Ornamentation,WaterHeater,Redundancy,TallOneStory,Equipment,SealofApproval,RoofGeometry,RoofPitch,RoofCover,RoofDeck,RoofCoverAttachment,RoofDeckAttachment,RoofAnchorage,RoofAttachedStructure,RoofYearBuilt,Tank,RoofHailImpactResistance,Chimney,WallType,WallSiding,GlassType,GlassPercent,WindowProtection,ExternalDoors,BuildingExteriorOpening,BrickVeneer,FoundationConnection,FoundationType,InternalPartition,Welding,TransitionInSRC,MultiStoryHallType,LatticeType,IsValueType,ColumnBasement,ColdFormedTube,AppurtenantStructures,Pounding,TreeExposure,SmallDebris,LargeMissile,TerrainRoughness,AdjacentBuildingHeight,ProjectCompletion,ProjectPhaseCode,BasementLevelCount,BasementFinishType,CustomElevation,BaseFloodElevation,FirstFloorHeight,ServiceEquipmentProtection,FIRMCompliance,ContentVulnerability,CertifiedStructuresIBHS,FireSprinklers,CON,OCC,Age,Height")
d[,1]<-d[Con]

write.table(d,file="c:/conoccsecchar.csv",row.names = FALSE)