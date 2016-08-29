
library(dplyr)

estados<-c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima","Chiapas","Chihuahua","Distrito Federal","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico","Michoacan","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatan","Zacatecas")
abrev<-c("Ags","BC","BCS","CAM","COA","Col","Chs","Chi","DF","Dur","Gto","Gro","Hgo","Jal","Mex","Mch","Mor","Nay","NL","Oax","Pbl","Qro","Qoo","SLP","Sin","Son","Tab","Tam","Tlx","Ver","Yuc","Zac")
cols<-dim(all)[2]
nombres<-names(all)

# Hacer graficas con municipios más significativos solamente
for (i in 1:cols) {
     for (est in 1:32) {
          #columnas<-c('ratio',nombres[i])
          #selec<-all[all$ratio>.34 & all$ratio<2.8 & all$estado==estado,columnas]
          selec1<-all[all$estado==est & !is.na(all$estado),]
          rats<-selec1$ratio[!is.na(selec1$ratio)]
          prom<-mean(rats)
          selec2<-selec1[selec1$ratio>prom & !is.na(selec1$ratio),]
          dos<-selec2[,c('ratio',nombres[i])]
          out<-clean(dos)
          if (dim(out)[1]>1) {
               nombre.png<-paste0("plot_",nombres[i],"-",abrev[est],".png")
               png(filename=nombre.png,width = 400, height = 400, units = "px")
               plot(out[,1],out[,2],xlab = "Ratio",ylab = nombres[i],main = estados[est])
               linea<-lm(out[,1]~out[,2])
               try({
               abline(linea)
               })
               dev.off()
               # Agregar curva de tendencia y corr
               # Grabar cada gráfica en un archivo (usando nombre de var y)
          }
     }

}


# Hacer graficas con todos los municipios
for (i in 1:cols) {
     for (est in 1:32) {
          #columnas<-c('ratio',nombres[i])
          #selec<-all[all$ratio>.34 & all$ratio<2.8 & all$estado==estado,columnas]
          selec1<-all[all$estado==est & !is.na(all$estado),]
          rats<-selec1$ratio[!is.na(selec1$ratio)]
          prom<-mean(rats)
          #selec2<-selec1[selec1$ratio>prom & !is.na(selec1$ratio),]
          dos<-selec1[,c('ratio',nombres[i])]
          out<-clean(dos)
          if (dim(out)[1]>1) {
               nombre.png<-paste0("Todos-plot_",nombres[i],"-",abrev[est],".png")
               png(filename=nombre.png,width = 400, height = 400, units = "px")
               plot(out[,1],out[,2],xlab = "Ratio",ylab = nombres[i],main = estados[est])
               linea<-lm(out[,1]~out[,2])
               try({
                    abline(linea)
               })
               dev.off()
          }
     }
     
}

#
# Hacer mapas geográficos de los municipios más representativos (por variable "ratio")
#




clean<-function(x){
     not.x<-is.na(x[,1])
     y<-x[!not.x,]
     not.y<-is.na(y[,2])  
     y[!not.y,]
}

plot(all$ratio[all$ratio>.34 & all$ratio<2.8 & !is.na(all[,nombres[1]]) & all$estado==1],
     all[all$ratio>.34 & all$ratio<2.8 & all$estado==1 & !is.na(all[,nombres[1]])
         ,nombres[1]])

View(all$ratio[all$ratio>.34 & all$ratio<2.8 & !is.na(all[,nombres[1]]) & all$estado==1])

