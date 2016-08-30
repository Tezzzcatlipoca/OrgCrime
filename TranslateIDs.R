

library(raster)
library(tmap)

municsMex<-read.csv("ListaMunicMex.csv")
map<-getData('GADM',country='MEX',level=2)
municsMex$key<-paste0(municsMex$CVE_ENT,"-",municsMex$NOM_MUN)
map$key<-paste0(map$ID_1,"-",map$NAME_2)
mapcomp<-merge(municsMex,map,by="key",all.x=TRUE)
ver<-select(mapcomp,key,CVE_ENT,NOM_ENT,CVE_MUN,NOM_MUN,NAME_2)



save_tmap(objectname,'name.svg')
