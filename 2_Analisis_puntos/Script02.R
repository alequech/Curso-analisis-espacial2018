
library(sf)
library(sp)
library(mapview)
library(tidyverse)
library(plotrix)
library(spatstat)
library(maptools)
library(raster)

setwd("C:/Users/Rocket/Google Drive/1.Materias/Analisis_espacial/2_Analisis_puntos")
data<-read.csv("input/crime-lat-long.csv")
str(data)
summary(data$lat)
summary(data$long)

data<-data[!is.na(data$long)&!is.na(data$lat),]
data<-data[data$year==2013,]

summary(data$lat)
summary(data$long)

#https://hoyodecrimen.com/

#zero <- zerodist(data)

border <- shapefile("input/DF_Delegaciones.shp")

#crime_sf = st_as_sf(data, coords = c("long", "lat"), crs = 4326)

#rs<-st_crs(border)

#st_crs(c)<-rs

coordinates(data)=~long+lat

plot(data,pch="+",cex=0.5,main="",col=data$crime)
plot(border,add=T)
legend(x=-0.53,y=51.41,pch="+",legend=unique(data$crime),cex=0.4)


projection(data)=projection(border)

#crime_utm<-st_transform(crime_sf,crs=6369)
#border_utm<-st_transform(border,crs=6369)



data_utm<-spTransform(data, CRS("+init=epsg:6369"))
border_utm<-spTransform(border, CRS("+init=epsg:6369"))

x_coord<-data_utm@coords[,1]
y_coord<-data_utm@coords[,2]


mean_centerX<-mean(data_utm@coords[,1])
mean_centerY<-mean(data_utm@coords[,2])

standard_deviationX <- sd(x_coord)
standard_deviationY <- sd(y_coord)


standard_distance <- sqrt(sum(((x_coord-mean_centerX)^2+(y_coord-mean_centerY)^2))/(nrow(data_utm)))


plot(data_utm,pch="+",cex=0.05,main="")
plot(border,add=T,cex=0.001)
points(mean_centerX,mean_centerY,col="red",pch=16)
draw.circle(mean_centerX,mean_centerY,radius=standard_distance,border="red",lwd=2)


plot(data_utm,pch="+",cex=0.05,main="")
plot(border,add=T,cex=0.001)
points(mean_centerX,mean_centerY,col="red",pch=16)
draw.ellipse(mean_centerX,mean_centerY,a=standard_deviationX,b=standard_deviationY,border="red",lwd=2)



#Homicidios 
homicides<-data_utm[data_utm$crime=="HOMICIDIO DOLOSO",]
homicides <- remove.duplicates(homicides)

w<-as(border_utm, "owin")

homicides.ppp <- ppp(x=homicides@coords[,1],y=homicides@coords[,2],window=w)

homicides.ppp$n/sum(sapply(slot(border_utm, "polygons"), slot, "area"))

q<-quadratcount(homicides.ppp, nx = 8, ny = 8)


plot(homicides.ppp,pch=20, cols="grey70",cex=0.05,main="Homicidios")
plot(q,add=TRUE,col="red")




Local.Intensity <- data.frame(Mun=factor(),Number=numeric())
for(i in unique(border_utm$Name)){
  sub.pol <- border_utm[border_utm$Name==i,]
  sub.ppp <- ppp(x=homicides.ppp$x,y=homicides.ppp$y,window=as.owin(sub.pol))
  Local.Intensity <- rbind(Local.Intensity,data.frame(Mun=factor(i,levels=border_utm$Name),Number=sub.ppp$n))
}

colorScale <- color.scale(Local.Intensity[order(Local.Intensity[,2]),2],color.spec="rgb",extremes=c("green","red"),alpha=0.8)
barplot(Local.Intensity[order(Local.Intensity[,2]),2],names.arg=Local.Intensity[order(Local.Intensity[,2]),1],horiz=T,las=2,space=1,col=colorScale)

sigma1<- bw.diggle(homicides.ppp)
sigma2<- bw.ppl(homicides.ppp)
sigma3<- bw.scott(homicides.ppp)[1]
sigma4<- bw.scott(homicides.ppp)[2]

d1<-density.ppp(homicides.ppp, sigma =sigma1,edge=T)
d2<-density.ppp(homicides.ppp, sigma =sigma2,edge=T)
d3<-density.ppp(homicides.ppp, sigma =sigma3,edge=T)
d4<-density.ppp(homicides.ppp, sigma =sigma4,edge=T)

plot(d1,main=paste("h =",round(sigma1,2)))
plot(d2,main=paste("h =",round(sigma2,2)))
plot(d3,main=paste("h =",round(sigma2,2)))
plot(d3,main=paste("h =",round(sigma2,2)))




plot(Gest(homicides.ppp),main="Homicidios")
