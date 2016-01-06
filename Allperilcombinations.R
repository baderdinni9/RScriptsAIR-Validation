
#load library
library(gtools)
library(plyr)
#urn with 3 balls
x <- (c( 'PAL',  'PEA'  ,'PES',	'PFL',	'PPH',	'PSH',	'PSL',	'PTR',	'PTS',	'PWA',	'PWF',	'PWH',	'PWT',	'PWW',	'PWX',	'PEF',	'PNC'))
#x <- (c( 'PAL',  'PEA'  ,'PES','PWA','PSH')) 
for (j in 1:17)
                {
            dat<-(data.frame(combinations(n=17,r=j,v=x,repeats.allowed=F)))
          
            dat.sort = t(apply(dat, 1, sort))
            dat<-data.frame(dat[!duplicated(dat.sort),])
            Buffer<-data.frame(assign(paste0("b", j),dat ));
                }

h<-data.frame(rbind.fill(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17))
h <- data.frame(lapply(h, as.character), stringsAsFactors=FALSE)
h[is.na(h)] <- ' '
#write.table(h, "c:/mydata.txt", sep="\t")
g<-data.frame(apply(h, 1, paste, collapse="+"))
g <- data.frame(lapply(g, as.character), stringsAsFactors=FALSE)
 g<-data.frame(as.character(gsub('^[+]*|[+ ]*$', '', g[,1]),quote=FALSE),stringsAsFactors=FALSE)


# write.table(g, "c:/mydata.csv", sep="\t",col.names=FALSE,row.names=FALSE,quote=FALSE)
# g<-read.csv('c:/mydata.csv',stringsAsFactors=FALSE,header=FALSE)
# newg<-g

g<-data.frame(as.character(gsub('^[ +]*|[+ ]*$', '', g[,1]),quote=FALSE),stringsAsFactors=FALSE)
m<-g

#l<-data.frame(gsub("PAL","PAL",g[,1]), stringsAsFactors=FALSE)
l<-data.frame(ifelse(grepl('PAL', g[,1]), gsub('PEA|PEF|PES|PFL|PNC|PPH|PSH|PSL|PTR|PTS|PWA|PWF|PWH|PWT|PWW|PWX', "", g[,1]), g[,1]), stringsAsFactors=FALSE)
l<-data.frame(ifelse(grepl('PEA', l[,1]), gsub('PES|PSL|PEF|PTS', "", l[,1]), l[,1]), stringsAsFactors=FALSE)
l<-data.frame(ifelse(grepl('PWA', l[,1]), gsub('PWH|PSH|PPH', "", l[,1]), l[,1]), stringsAsFactors=FALSE)
l<-data.frame(ifelse(grepl('PWX', l[,1]), gsub('PWT|PWW', "", l[,1]), l[,1]), stringsAsFactors=FALSE)


  l<-data.frame(as.character(gsub('^[+]*|[+ ]*$', '', l[,1]),quote=FALSE), stringsAsFactors=FALSE)


# write.table(g, "c:/mydata.csv", sep="\t",col.names=FALSE,row.names=FALSE,quote=FALSE)
# g<-read.csv('c:/mydata.csv',stringsAsFactors=FALSE,header=FALSE)
# newg<-g

  l<-data.frame(as.character(gsub('^[ +]*|[+ ]*$', '', l[,1]),quote=FALSE), stringsAsFactors=FALSE)




l<-data.frame(unique(l))
#l<-data.frame(ifelse(grepl('$++$', l[,1]), gsub('PWT|PWW', "", l[,1]), l[,1]), stringsAsFactors=FALSE)
#l<-data.frame(gsub("$++$","+",l[,1]), stringsAsFactors=FALSE,row.names=F)
l<-data.frame(gsub('\\++','+',l[,1]), stringsAsFactors=FALSE)

write.table(m, "c:/mydata.txt", sep=",",row.names=F)
write.table(l, "c:/mydata1.txt", sep=",",row.names=F)



