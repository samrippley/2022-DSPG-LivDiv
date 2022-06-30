library(rgdal)
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)
library(raster)
library(sp)


village_shp<-readOGR('./shapefiles', layer = 'Village, GP coordinates')

village_names<- unique(village_shp$Village.Na)
villnames <- as.data.frame(village_names) 
                          
save(village_names, file="villnames.rda")             #                                 #remove from envir                                     #gone : (
load("villnames.rda")   

ourvill <- subset(village_shp, Village.Na %in% c('Amrabati','Beguakhali', 
                                               'Bijoynagar', 'Birajnagar', 
                                               'Haridaskati Samsernagar', 
                                               'Lakshmi Janardanpur', 'Parghumti',
                                               'Purba Dwarokapur', 'Sagar Madhabpur', 'Shibpur'))
head(ourvill)


gadm_shp<-readOGR('./GADM', layer = 'gadm36_IND_3',)
plot(gadm_shp)



sunshp <- subset(gadm_shp, NAME_2 %in% c('South 24 Parganas', 'North 24 Parganas'))

leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lat = 22.3, lng = 88.7, zoom = 8) %>%
  addPolygons(data=sunshp,weight=1,col = 'black') %>% 
  addMarkers(data=ourvill)


    
  



