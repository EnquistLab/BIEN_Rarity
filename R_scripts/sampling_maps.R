#4.1 plot vs specimen maps

source("F:/Maitner/RtodoBIEN/BIEN_files/password_and_username.R")
library(todoBIEN)
library(sp)
library(maptools)
library(raster)
plot_points <- BIEN_sql(query = "SELECT observation_type, latitude,longitude,is_geovalid 
                                FROM view_full_occurrence_individual 
                                WHERE scrubbed_species_binomial IS NOT NULL
                                AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')
                                AND (is_introduced = 0 OR is_introduced IS NULL)
                                AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL)
                                AND is_location_cultivated IS NULL
                                AND observation_type = 'plot' ;",user=user,password=password)

specimen_points <- BIEN_sql(query = "SELECT observation_type, latitude,longitude,is_geovalid 
                                FROM view_full_occurrence_individual 
                                WHERE scrubbed_species_binomial IS NOT NULL
                                AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')
                                AND (is_introduced = 0 OR is_introduced IS NULL)
                                AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL)
                                AND is_location_cultivated IS NULL
                                AND observation_type = 'specimen' ;",user=user,password=password)




plot_geovalid <- plot_points[which(plot_points$is_geovalid==1),]  

specimen_geovalid <- specimen_points[which(specimen_points$is_geovalid==1),]  

#Convert current points to spdf
plot_sp <- SpatialPoints(coords = na.omit(plot_geovalid[c('longitude','latitude')]),proj4string = sp::CRS(projargs = "+init=epsg:4326"))

specimen_sp <- SpatialPoints(coords = na.omit(specimen_geovalid[c('longitude','latitude')]),proj4string = sp::CRS(projargs = "+init=epsg:4326"))

bgMap <- raster(resolution=1)


plot_raster <- rasterize(x = plot_sp,y = bgMap,fun="count")
specimen_raster <- rasterize(x = specimen_sp,y = bgMap,fun="count")

#plot_occs<-rasterize(x = BIEN41_sp_ea,y = template,fun="count")
#specimen_occs<-rasterize(x = BIEN41_sp_ea,y = template,fun="count")
#BIEN41_sp_ea<-spTransform(x = bien41_sp,CRSobj = template@crs)

plot_robinson<-projectRaster(from = log10(plot_raster),crs = crs("+init=ESRI:54030"))
plot(plot_robinson)

specimen_robinson<-projectRaster(from = log10(specimen_raster),crs = crs("+init=ESRI:54030"))
plot(specimen_robinson)

writeRaster(x = plot_raster,filename ="maps/plot_raster_wgs84.tif" )
writeRaster(x = specimen_raster,filename ="maps/specimen_raster_wgs84.tif" )

writeRaster(x = plot_robinson,filename ="maps/plot_raster_robinson.tif" )
writeRaster(x = specimen_robinson,filename ="maps/specimen_raster_robinson.tif" )



