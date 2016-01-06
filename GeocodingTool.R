install.packages("ggplot2")
library(RODBC)
library(xlsx)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=qa-ts-153-db1\\sql2014;trusted_connection=true')



########################################valid case#########################################################
sql_query <- paste("select * from 
((select t4.City,t4.Crestcode as CRESTACode,AreaCode,SubareaCode,PostalCode,Countrycode,latitude,longitude from (select *, ROW_NUMBER() over (PARTITION by t3.cityid,t3.crestaid,t3.areaid,t3.subareaid,t3.postalid,t3.rankid,t3.countrycode order by rankid desc) as rn from 
(select * from 
                   (select *,cityid+crestaid+areaid+subareaid+postalid as RankID from 
                   (select *,case when City='' then 0 else 1  end cityid,
                   case when Crestcode='' then 0 else 1  end crestaid,
                   case when Areacode='' then 0 else 1  end areaid,
                   case when Subareacode='' then 0 else 1  end subareaid,
                   case when Postalcode='' then 0 else 1  end postalid
                   from
                   (SELECT isnull(b.strCityName,'') City,
                   isnull(cast(CRESTACode as nvarchar),'') Crestcode,isnull(cast(AreaCode as nvarchar),'') AreaCode,isnull(cast(SubareaCode as nvarchar),'') SubareaCode,
                   isnull(cast(PostalCode as nvarchar),'') PostalCode,
                   isnull(countrycode,'') countrycode,isnull(cast(latitude as nvarchar),'') latitude,isnull(cast(longitude as nvarchar),'') longitude
                   
                   FROM [AIRGeography].[dbo].[tGeography] a 
                   full join [AIRGeography].[dbo].[TblAreaCity] b on a.GuidExternal=b.guidExternal and RemappedGeographySID is  NULL  and countrycode != 'US'
                   ) t ) t1) t2  )t3 ) t4 where  latitude not like ('0.%') and longitude not like ('0.%') and t4.rn=1   )
                   
                   union
                   
                   (
                   
                   
                   select t4.City,t4.Crestcode as CRESTACode,AreaCode,SubareaCode,PostalCode,Countrycode,latitude,longitude from (select *, ROW_NUMBER() over (PARTITION by t3.cityid,t3.crestaid,t3.areaid,t3.subareaid,t3.postalid,t3.rankid,t3.countrycode order by rankid desc) as rn from 
                   (select * from 
                   (select *,cityid+crestaid+areaid+subareaid+postalid as RankID from 
                   (select *,case when City='' then 0 else 1  end cityid,
                   case when Crestcode='' then 0 else 1  end crestaid,
                   case when Areacode='' then 0 else 1  end areaid,
                   case when Subareacode='' then 0 else 1  end subareaid,
                   case when Postalcode='' then 0 else 1  end postalid
                   from
                   (SELECT isnull(b.CityName,'') City,
                   isnull(cast(CRESTACode as nvarchar),'') Crestcode,isnull(cast(a.AreaCode as nvarchar),'') AreaCode,isnull(cast(a.SubareaCode as nvarchar),'') SubareaCode,
                   isnull(cast(a.PostalCode as nvarchar),'') PostalCode,
                   isnull(a.countrycode,'') countrycode,isnull(cast(latitude as nvarchar),'') latitude,isnull(cast(longitude as nvarchar),'') longitude
                   
                   FROM [AIRGeography].[dbo].[tGeography] a 
                   full join [AIRGeography].[dbo].[tUSCity] b on a.GeographySID=b.GeographySID and 
                   RemappedGeographySID is  NULL
                   ) t ) t1) t2  )t3 ) t4 where  countrycode='US' and latitude not like ('0.%') and longitude not like ('0.%') and t4.rn=1  )) y
                   
                   --where y.countrycode='GB'
                   
                   
                   
                   
                   ")





########################################Invalid case#########################################################
sql_query1 <- paste("-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------Invalid NON US---------------------------------------------------------------------------------------------------------------------
(
                    select  City, case when city= ''  then '' else (SELECT TOP 1 isnull(p.strCityName,'')-- +'_'+o.CountryCode 
                    FROM [AIRGeography].[dbo].[tGeography] o
                    full join [AIRGeography].[dbo].[TblAreaCity] p on o.guidExternal=p.guidExternal and 
                    RemappedGeographySID is  NULL where strCityName is not NULL and strCityName!=t.City and o.AreaCode!=t.AreaCode and o.countrycode!=t.countrycode 
                    and o.CountryCode!='US') end 
                    Cityinvalid,
                    
                    
                    Crestacode,case when Crestacode= ''  then '' else (SELECT TOP 1 isnull(q.CRESTACode,'')--+ '_'+ q.CountryCode
                    FROM [AIRGeography].[dbo].[tGeography] q 
                    full join [AIRGeography].[dbo].[TblAreaCity] r on q.guidExternal=r.guidExternal and 
                    RemappedGeographySID is  NULL where q.CRESTACode is not NULL and q.CRESTACode!=t.Crestacode and q.countrycode!=t.countrycode and q.CountryCode!='US') end 
                    Crestainvalid ,
                    
                    AreaCode,case when AreaCode= ''  then '' else (SELECT TOP 1 isnull(s.AreaCode,'')--+ '_'+ s.CountryCode
                    FROM [AIRGeography].[dbo].[tGeography] s 
                    full join [AIRGeography].[dbo].[TblAreaCity] t9 on s.guidExternal=t9.guidExternal and 
                    
                    RemappedGeographySID is  NULL where s.AreaCode is not NULL and s.AreaCode!=t.AreaCode and s.CountryCode!=t.countrycode and s.CountryCode!='US')
                    end 
                    AreaCodeinvalid,
                    
                    
                    SubareaCode,case when SubareaCode= ''  then '' else (SELECT TOP 1 isnull(u.SubareaCode,'')-- + '_'+ u.countrycode
                    FROM [AIRGeography].[dbo].[tGeography] u 
                    full join [AIRGeography].[dbo].[TblAreaCity] v on u.guidExternal=v.guidExternal and 
                    RemappedGeographySID is  NULL where u.SubareaCode is not NULL and u.SubareaCode!=t.SubareaCode and u.AreaCode!=t.AreaCode  and u.countrycode!=t.countrycode
                    and u.CountryCode!='US'
                    order by u.GeographySID asc) end 
                    SubareaCodeinvalid,
                    
                    PostalCode,case when PostalCode= ''  then '' else (SELECT TOP 1 isnull(w.PostalCode,'')--+ '_'+ w.CountryCode
                    FROM [AIRGeography].[dbo].[tGeography] w
                    full join [AIRGeography].[dbo].[TblAreaCity] x on w.guidExternal=x.guidExternal and 
                    RemappedGeographySID is  NULL where w.PostalCode is not NULL and w.PostalCode!=t.PostalCode and w.AreaCode!=t.AreaCode and w.countrycode!=t.countrycode 
                    and w.CountryCode!='US'
                    order by w.GeographySID desc)  end 
                    PostalCodeinvalid,countrycode,latitude,longitude from 
                    
                    
                    
                    
                    
                    (select t4.City,t4.Crestcode as CRESTACode,AreaCode,SubareaCode,PostalCode,Countrycode,latitude,longitude from (select *, ROW_NUMBER() over (PARTITION by t3.cityid,t3.crestaid,t3.areaid,t3.subareaid,t3.postalid,t3.rankid,t3.countrycode order by rankid desc) as rn from 
                    (select * from 
                    (select *,cityid+crestaid+areaid+subareaid+postalid as RankID from 
                    (select *,case when City='' then 0 else 1  end cityid,
                    case when Crestcode='' then 0 else 1  end crestaid,
                    case when Areacode='' then 0 else 1  end areaid,
                    case when Subareacode='' then 0 else 1  end subareaid,
                    case when Postalcode='' then 0 else 1  end postalid
                    from
                    (SELECT isnull(b.strCityName,'') City,
                    isnull(cast(CRESTACode as nvarchar),'') Crestcode,isnull(cast(AreaCode as nvarchar),'') AreaCode,isnull(cast(SubareaCode as nvarchar),'') SubareaCode,
                    isnull(cast(PostalCode as nvarchar),'') PostalCode,
                    isnull(countrycode,'') countrycode,isnull(cast(latitude as nvarchar),'') latitude,isnull(cast(longitude as nvarchar),'') longitude
                    
                    FROM [AIRGeography].[dbo].[tGeography] a 
                    full join [AIRGeography].[dbo].[TblAreaCity] b on a.GuidExternal=b.guidExternal and RemappedGeographySID is  NULL  and countrycode != 'US'
                    ) t ) t1) t2  )t3 ) t4 where  latitude not like ('0.%') and longitude not like ('0.%') and t4.rn=1  ) t) union
                    
                    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                    -----------------------------------------------------Invalid NON US---------------------------------------------------------------------------------------------------------------------
                    
                    
                    
                    
                    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                    -----------------------------------------------------Invalid  US---------------------------------------------------------------------------------------------------------------------
                    
                    
                    (
                    select  City, case when city= ''  then '' else (SELECT TOP 1 isnull(p.strCityName,'') --+ '_'+o.CountryCode 
                    FROM [AIRGeography].[dbo].[tGeography] o
                    full join [AIRGeography].[dbo].[TblAreaCity] p on o.guidExternal=p.guidExternal and 
                    RemappedGeographySID is  NULL where strCityName is not NULL and strCityName!=t.City and o.AreaCode!=t.AreaCode and o.countrycode!=t.countrycode 
                    and o.CountryCode!='US') end 
                    Cityinvalid,
                    
                    
                    Crestacode,case when Crestacode= ''  then '' else (SELECT TOP 1 isnull(q.CRESTACode,'')-- + '_'+ q.CountryCode
                    FROM [AIRGeography].[dbo].[tGeography] q 
                    full join [AIRGeography].[dbo].[TblAreaCity] r on q.guidExternal=r.guidExternal and 
                    RemappedGeographySID is  NULL where q.CRESTACode is not NULL and q.CRESTACode!=t.Crestacode and q.countrycode!=t.countrycode and q.CountryCode!='US') end 
                    Crestainvalid ,
                    
                    AreaCode,case when AreaCode= ''  then '' else (SELECT TOP 1 isnull(s.AreaCode,'') --+ '_'+ s.CountryCode
                    FROM [AIRGeography].[dbo].[tGeography] s 
                    full join [AIRGeography].[dbo].[TblAreaCity] t9 on s.guidExternal=t9.guidExternal and 
                    
                    RemappedGeographySID is  NULL where s.AreaCode is not NULL and s.AreaCode!=t.AreaCode and s.CountryCode!=t.countrycode and s.CountryCode!='US')
                    end 
                    AreaCodeinvalid,
                    
                    
                    SubareaCode,case when SubareaCode= ''  then '' else (SELECT TOP 1 isnull(u.SubareaCode,'')  --+ '_'+ u.countrycode
                    FROM [AIRGeography].[dbo].[tGeography] u 
                    full join [AIRGeography].[dbo].[TblAreaCity] v on u.guidExternal=v.guidExternal and 
                    RemappedGeographySID is  NULL where u.SubareaCode is not NULL and u.SubareaCode!=t.SubareaCode and u.AreaCode!=t.AreaCode  and u.countrycode!=t.countrycode
                    and u.CountryCode!='US'
                    order by u.GeographySID asc) end 
                    SubareaCodeinvalid,
                    
                    PostalCode,case when PostalCode= ''  then '' else (SELECT TOP 1 isnull(w.PostalCode,'')-- + '_'+ w.CountryCode
                    FROM [AIRGeography].[dbo].[tGeography] w
                    full join [AIRGeography].[dbo].[TblAreaCity] x on w.guidExternal=x.guidExternal and 
                    RemappedGeographySID is  NULL where w.PostalCode is not NULL and w.PostalCode!=t.PostalCode and w.AreaCode!=t.AreaCode and w.countrycode!=t.countrycode 
                    and w.CountryCode!='US'
                    order by w.GeographySID desc)  end 
                    PostalCodeinvalid,countrycode,latitude,longitude from 
                    
                    
                    
                    
                    
                    
                    
                    ( select t4.City,t4.Crestcode as CRESTACode,AreaCode,SubareaCode,PostalCode,Countrycode,latitude,longitude from (select *, ROW_NUMBER() over (PARTITION by t3.cityid,t3.crestaid,t3.areaid,t3.subareaid,t3.postalid,t3.rankid,t3.countrycode order by rankid desc) as rn from 
                    (select * from 
                    (select *,cityid+crestaid+areaid+subareaid+postalid as RankID from 
                    (select *,case when City='' then 0 else 1  end cityid,
                    case when Crestcode='' then 0 else 1  end crestaid,
                    case when Areacode='' then 0 else 1  end areaid,
                    case when Subareacode='' then 0 else 1  end subareaid,
                    case when Postalcode='' then 0 else 1  end postalid
                    from
                    (SELECT isnull(b.CityName,'') City,
                    isnull(cast(CRESTACode as nvarchar),'') Crestcode,isnull(cast(a.AreaCode as nvarchar),'') AreaCode,isnull(cast(a.SubareaCode as nvarchar),'') SubareaCode,
                    isnull(cast(a.PostalCode as nvarchar),'') PostalCode,
                    isnull(a.countrycode,'') countrycode,isnull(cast(latitude as nvarchar),'') latitude,isnull(cast(longitude as nvarchar),'') longitude
                    
                    FROM [AIRGeography].[dbo].[tGeography] a 
                    full join [AIRGeography].[dbo].[tUSCity] b on a.GeographySID=b.GeographySID and 
                    RemappedGeographySID is  NULL
                    ) t ) t1) t2  )t3 ) t4 where  countrycode='US' and latitude not like ('0.%') and longitude not like ('0.%') and t4.rn=1  )t) 
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                    -----------------------------------------------------invalid  US---------------------------------------------------------------------------------------------------------------------
                   
                   
                   
                   ")



Airgeodata <- data.frame(sqlQuery(dbhandle, sql_query1, stringsAsFactors = FALSE))           #Gives you a specific Query
Airgeodata[is.na(Airgeodata)] <- ""
#Useful Commands for another time...
#Query_Results <- sqlFetch(dbhandle, "Area_Codes", stringsAsFactors = FALSE)     #Gives you a specific Table
#Not sure which is better...
#close(dbhandle)

Airgeodata2 <- data.frame(sqlQuery(dbhandle, sql_query1, stringsAsFactors = FALSE))           #Gives you a specific Query
Airgeodata2[is.na(Airgeodata2)] <- ""

Airgeodata3<-Airgeodata2
#Airgeodata<-Airgeodata[,c(1:5)]


Airgeodata1<-Airgeodata2[,c(1,3,5,7,9,11,12,13)]
Airgeodata<-Airgeodata2
Airgeodata<-Airgeodata[,c(1,3,5,7,9)]
Airgeodata2<-Airgeodata2[,c(2,4,6,8,10,11,12,13)]
remove(dbhandle)
remove(sql_query)
odbcCloseAll()



subtype <- c("City","CRESTACode","AreaCode","SubareaCode","PostalCode")

height <- c("Valid","Invalid","Missing")

d<-cbind.data.frame(Var1=subtype , Var2=c(t(as.matrix(expand.grid(height,height,height,height,height)))))
v<-data.frame(t(d[,2]))
v<-data.frame(t(v), stringsAsFactors=FALSE)
v<-data.frame(v)
row.names(v) <- NULL
v<-data.frame(v)

Combinations<-data.frame(matrix(v[1, 1:length(v)], ncol = 5, byrow = TRUE) )
colnames(Combinations)<-c("City","CRESTACode","AreaCode","SubareaCode","PostalCode")


###############################Logic for creating combinations###########################################

x=0;
y=1;
z=1;
i=1;
m <- as.data.frame(matrix(0, ncol = ncol(Airgeodata1), nrow = nrow(Airgeodata)*nrow(Combinations)))
n <- as.data.frame(matrix(0, ncol = ncol(Airgeodata), nrow = nrow(Airgeodata)*nrow(Combinations)))


for (z in 1:nrow(Airgeodata))
{
for (x in 1:nrow(Combinations)) 
    {
  
  for (y in 1:ncol(Combinations)) { 
if (Combinations[x,y]=="Valid")
                        {
        if(Airgeodata[z,y]!=''){
          
          m[i,y]=as.character(Airgeodata[z,y]);
          m[i,6]=as.character(Airgeodata1[z,6])
          m[i,7]=as.character(Airgeodata1[z,7])
          m[i,8]=as.character(Airgeodata1[z,8])
          n[i,y]=as.character(Combinations[x,y]);
        
          #Valid
        }
        
        else{
          m[i,y]=("not valid")
          m[i,6]=as.character(Airgeodata1[z,6])
          m[i,7]=as.character(Airgeodata1[z,7])
          m[i,8]=as.character(Airgeodata1[z,8])
          n[i,y]=as.character(Combinations[x,y]);
        
        }
          }         

else{
                      
                      if (Combinations[x,y]=="Invalid")
                    {
                      if(Airgeodata2[z,y]!=''){
                        m[i,y]=as.character(Airgeodata2[z,y]);
                        m[i,6]=as.character(Airgeodata2[z,6])
                        m[i,7]=as.character(Airgeodata2[z,7])
                        m[i,8]=as.character(Airgeodata2[z,8])
                        n[i,y]=as.character(Combinations[x,y]);
                    
                        #Invalid
                      }
                      
                      else{
                        m[i,y]=("not valid")  
                        m[i,6]=as.character(Airgeodata1[z,6])
                        m[i,7]=as.character(Airgeodata1[z,7])
                        m[i,8]=as.character(Airgeodata1[z,8])
                        n[i,y]=as.character(Combinations[x,y]);
                 
                      }
                      }  
                    

                    
                    else 
                                          {
                                            if(Airgeodata[z,y]!=''){
                                              
                                              m[i,y]='';
                                              m[i,6]=as.character(Airgeodata1[z,6])
                                              m[i,7]=as.character(Airgeodata1[z,7])
                                              m[i,8]=as.character(Airgeodata1[z,8])
                                              n[i,y]=as.character(Combinations[x,y]);
                                            
                                              #Missing
                                            }
                                            
                                            else{
                                              m[i,y]='';
                                              m[i,6]=as.character(Airgeodata1[z,6])
                                              m[i,7]=as.character(Airgeodata1[z,7])
                                              m[i,8]=as.character(Airgeodata1[z,8])
                                              n[i,y]=as.character(Combinations[x,y]);
                                        
                                            }
                                          }
                    }

   }
i=i+1;
}

}

  
m<-cbind(m,n)
k<-m[!(m[,1]=="not valid" | m[,2]=="not valid" | m[,3]=="not valid" | m[,4]=="not valid"| 
         m[,5]=="not valid"),]
k<-data.frame(k,stringsAsFactors=FALSE)
colnames(k) <- c("City","CRESTACode","AreaCode","SubareaCode","PostalCode","Countrycode","latitude","longitude",
                 "Citytype","CRESTACodetype","AreaCodetype","SubareaCodetype","PostalCodetype")

# colnames(k1) <- c("City","CRESTACode","AreaCode","SubareaCode","PostalCode","Countrycode","latitude","longitude",
#                  "Citytype","CRESTACodetype","AreaCodetype","SubareaCodetype","PostalCodetype")
# colnames(k2) <- c("City","CRESTACode","AreaCode","SubareaCode","PostalCode","Countrycode","latitude","longitude",
#                   "Citytype","CRESTACodetype","AreaCodetype","SubareaCodetype","PostalCodetype")
# 
# colnames(k3) <- c("City","CRESTACode","AreaCode","SubareaCode","PostalCode","Countrycode","latitude","longitude",
#                   "Citytype","CRESTACodetype","AreaCodetype","SubareaCodetype","PostalCodetype")


k1<-k
k$latitude<-(k$latitude=0)
k$longitude<-(k$longitude=0)
k2<-k
k3<-k1
k3$latitude<-(as.numeric(k3$latitude)+0.002)
k3$longitude<-(as.numeric(k3$longitude)+0.002)



h<-data.frame(read.csv("c:/forgeocoding.csv"))

write.xlsx(k1, "c:/geocodingwithcentroid.xlsx",row.names=F)
write.xlsx(k2, "c:/geocodingnoll.xlsx",row.names=F)
write.xlsx(k3, "c:/geocodinguserll.xlsx",row.names=F)

       
