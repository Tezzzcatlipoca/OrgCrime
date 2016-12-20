
library(xlsx)
library(foreign)

cdel<-read.xlsx("CDEL2012.xlsx",1) # Claves de delitos (Seleccionados los útiles)
co.del<-cdel[cdel$EstÃ.n.>0,]

# Identificar delitos importantes
delitos.imp<-co.del$CVE_DEL

# Abrir base de delitos 
sdel<-read.dbf("sdel2009.dbf")
sdel.imp<-sdel[sdel$B_DELITO %in% delitos.imp,]
sdel.imp$ENTMUN<-paste0(sdel.imp$B_ENTOC,"-",sdel.imp$B_MUNOC)

# Identificar todos los implicados
id_ps<-unique(sdel.imp$ID_PS)

# Abrir archivo de registros
sreg<-read.dbf("sreg2009.dbf")
sreg.imp<-sreg[sreg$ID_PS %in% id_ps,] # De
sreg.imp$ENTMUN_NAC<-paste0(sreg.imp$B_ENTNAC,"-",sreg.imp$B_MUNNAC) # Lugar de nacimiento
sreg.imp$ENTMUN_RES<-paste0(sreg.imp$B_ENTRH,"-",sreg.imp$B_MUNRH)  # Lugar de residencia hab.

# Carga resultados de censo de población 2010
pop<-read.table("C:/Users/Roberto/Documents/Proyectos/Blog Narco-CO/CO/2016/CensoPob2010.txt",sep="\t",quote = "",header=T)
pop$key<-gsub(" ","-",pop$ENTMUN)

# Municipios con más delitos registrados
munic.del<-sdel.imp$ENTMUN
tabla.munic.del<-table(munic.del) # Frecuencias
ratio.munic.del<-data.frame(entmun=names(tabla.munic.del),ocurr=as.integer(tabla.munic.del))
ratio.munic.del<-merge(ratio.munic.del,pop,by.x="entmun",by.y="key",all.x=T)
ratio.munic.del$ratio<-ratio.munic.del$ocurr/ratio.munic.del$pop
ratio.munic.del.sorted<-ratio.munic.del[order(ratio.munic.del$ratio,decreasing = T),] # Ordenar municipios por ratio
tabla.munic.del.ord<-tabla.munic.del[order(tabla.munic.del,decreasing = TRUE)] # Ordenadas
munics.del<-rep(0,dim(tabla.munic.del.ord)[1])
for(i in 1:(dim(tabla.munic.del.ord)[1])){        # Genera tabla con porcentajes de ocurrencia
     munics.del[i]<-sum(tabla.munic.del.ord[1:i])/sum(tabla.munic.del.ord)
}
plot(munics.del) # Graficar número de municipios que representan total de observaciones (del.)

# Municipios con más delincuentes registrados - Por Lugar de Nacimiento
munic.regnac<-sreg.imp$ENTMUN_NAC
tabla.munic.regnac<-table(munic.regnac) # Frecuencias
ratio.munic.regnac<-data.frame(entmun=names(tabla.munic.regnac),ocurr=as.integer(tabla.munic.regnac))
ratio.munic.regnac<-merge(ratio.munic.regnac,pop,by.x="entmun",by.y="key",all.x=T)
ratio.munic.regnac$ratio<-ratio.munic.regnac$ocurr/ratio.munic.regnac$pop
ratio.munic.regnac.sorted<-ratio.munic.regnac[order(ratio.munic.regnac$ratio,decreasing = T),]# Ordernar municipios por ratio
tabla.munic.regnac.ord<-tabla.munic.regnac[order(tabla.munic.regnac,decreasing = TRUE)] # Ordenadas
munics.regnac<-rep(0,dim(tabla.munic.regnac.ord)[1])
for(i in 1:(dim(tabla.munic.regnac.ord)[1])){ # Genera tabla con porcentajes de ocurrencia
     munics.regnac[i]<-sum(tabla.munic.regnac.ord[1:i])/sum(tabla.munic.regnac.ord)
}
plot(munics.regnac) # Graficar número de municipios que representan total de observaciones (del.)

# Municipios con más delincuentes registrados - Por Lugar de Residencia Habitual
munic.regres<-sreg.imp$ENTMUN_RES
tabla.munic.regres<-table(munic.regres) # Frecuencias
ratio.munic.regres<-data.frame(entmun=names(tabla.munic.regres),ocurr=as.integer(tabla.munic.regres))
ratio.munic.regres<-merge(ratio.munic.regres,pop,by.x="entmun",by.y="key",all.x=T)
ratio.munic.regres$ratio<-ratio.munic.regres$ocurr/ratio.munic.regres$pop
ratio.munic.regres.sorted<-ratio.munic.regres[order(ratio.munic.regres$ratio,decreasing = T),]# Ordernar municipios por ratio

tabla.munic.regres.ord<-tabla.munic.regres[order(tabla.munic.regres,decreasing = TRUE)] # Ordenadas
munics.regres<-rep(0,dim(tabla.munic.regres.ord)[1])
for(i in 1:(dim(tabla.munic.regres.ord)[1])){ #Genera tabla con porcentaje de delitos realizados
                                             # en cada municipio (del total)
     munics.regres[i]<-sum(tabla.munic.regres.ord[1:i])/sum(tabla.munic.regres.ord)
}
plot(munics.regres) # Graficar número de municipios que representan total de observaciones (del.)

# Comparar los lugares de ocurrencia con nacimiento y residencia habitual
munic_del_100<-ratio.munic.del.sorted[ratio.munic.del.sorted$ocurr>7,][1:100,] # 7= 3rd Qu.
munic_regnac_100<-ratio.munic.regnac.sorted[ratio.munic.regnac.sorted$ocurr>6,][1:100,] # 6=3rd Qu.
munic_regres_100<-ratio.munic.regres.sorted[ratio.munic.regres.sorted$ocurr>7,][1:100,] # 7=3rd Qu.
sum(munic_regnac_100$entmun %in% munic_regres_100$entmun) #71
sum(munic_del_100$entmun %in% munic_regnac_100$entmun) # 55
sum(munic_del_100$entmun %in% munic_regres_100$entmun) # 64


# Sacar nombre y estado de los 100 municipios
nazwy<-read.dbf("C:/Users/Roberto/Documents/Proyectos/Blog Narco-CO/CO/2016/MPIOS2012.dbf")
nazwy$key<-paste0(nazwy$CVE_ENT,"-",nazwy$CVE_MUN)

# Obtener los nombres de los municipios con mayores ocurrencias
nombres_del_100<-nazwy$DESCRIP[nazwy$key %in% munic_del_100]
nombres_regnac_100<-nazwy$DESCRIP[nazwy$key %in% munic_regnac_100]
nombres_regres_100<-nazwy$DESCRIP[nazwy$key %in% munic_regres_100]


# Imprimirlos en un mapa
# Diferenciar por tipo de crimen (producción, tráfico, violencia, lavado de dinero)


# Definir la línea de corte para observar los municipios con más actividad
# para ser comparados con los municipios derivados de las personas

# Para identificar municipios, hacer llave EST-MUN

plot(xtabs(B_NACION~B_SEXO,data=sreg.imp))
plot(xtabs(B_NACION~B_EDAD,data=sreg.imp)) # Mal hechos, porque suman los códigos de nacionalidad

# Ocupación por edad
# Ocupación por estado civil
# Sexo 
# Mujeres por ocupación y estado civil
# 
library(mosaic)
mosaic::tally(~B_OCUPA+B_SEXO,data=sreg.imp)
mosaic::tally(~B_EDOCIVIL+B_SEXO,data=sreg.imp)
mosaic::tally(~B_EDOCIVIL+B_REINCIDE,data=sreg.imp)

