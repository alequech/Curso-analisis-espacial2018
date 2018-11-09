#https://hoyodecrimen.com/

library(sf)
library(sp)
library(mapview)
library(tidyverse)
library(plotrix)
library(spatstat)
library(maptools)


setwd("C:/Users/Rocket/Google Drive/1.Materias/Analisis_espacial/2_Analisis_puntos")
data<-read.csv("input/crime-lat-long.csv")
str(data)
summary(data$lat)
summary(data$long)
zero<-spatstat
data<-data[!is.na(data$long)&!is.na(data$lat),]
data<-data[data$year==2013,]

summary(data$lat)
summary(data$long)


#zero <- zerodist(data)

border <- st_read("input/DF_Delegaciones.shp")

crime_sf = st_as_sf(data, coords = c("long", "lat"), crs = 4326)

rs<-st_crs(border)

#st_crs(c)<-rs

#coordinates(data)=~lat+long

plot(crime_sf["crime"],pch="+",cex=0.5,main="",col=crime_sf$crime)
plot(st_geometry(border),add=T)
legend(x=-0.53,y=51.41,pch="+",col=unique(crime_sf$crime),legend=unique(crime_sf$crime),cex=0.4)

crime_utm<-st_transform(crime_sf,crs=6369)
border_utm<-st_transform(border,crs=6369)

x_coord<-st_coordinates(crime_utm)[,1]
y_coord<-st_coordinates(crime_utm)[,2]


mean_centerX<-mean(x_coord)
mean_centerY<-mean(y_coord)

standard_deviationX <- sd(x_coord)
standard_deviationY <- sd(y_coord)


standard_distance <- sqrt(sum(((x_coord-mean_centerX)^2+(y_coord-mean_centerY)^2))/(nrow(crime_utm)))


plot(st_geometry(crime_utm),pch="+",cex=0.05,main="")
plot(st_geometry(border_utm),add=T,cex=0.001)
points(mean_centerX,mean_centerY,col="red",pch=16)
draw.circle(mean_centerX,mean_centerY,radius=standard_distance,border="red",lwd=2)


plot(st_geometry(crime_utm),pch="+",cex=0.05,main="")
plot(st_geometry(border_utm),add=T,cex=0.001)
points(mean_centerX,mean_centerY,col="red",pch=16)
draw.ellipse(mean_centerX,mean_centerY,a=standard_deviationX,b=standard_deviationY,border="red",lwd=2)

#Homicidios 
homicides<-crime_utm[crime_utm$crime==" HOMICIDIO DOLOSO"]
plot(st_geometry(homicides),pch="+",cex=0.05)

border.sp<-as(border_utm, 'Spatial')

nc_sp <- sf:::as_Spatial(border_utm$geometry) # This works

w <- as.owin(a)

homicides.ppp <- ppp(x=x_coord,y=y_coord,window=w)

nc_sp <- sf:::as_Spatial(border_utm$geometry)
