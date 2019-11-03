library(raster)
PATH_DATA <- "data_20181211"
path_freq <- paste0(PATH_DATA,"/1_freq/")
load(paste0(path_freq,"sppfreq_combine_update1","all",".rdata")) 
load(paste0(PATH_DATA,"/0_raw/df_spen_all.rdata") ) 
load(paste0(PATH_DATA,"/0_raw/df_plotUNQ_all.rdata") )
df_both <- rbind(df_plotUNQ,df_spen)
table(df_both$is_geovalid)
df_both <- subset(df_both,is_geovalid==1)
length(sppfreq_combine$sci_name[sppfreq_combine$flag_rare])
raw_combined_geoVad_rare <- df_both[df_both$scrubbed_species_binomial %in% sppfreq_combine$sci_name[sppfreq_combine$flag_rare],     ]
raw_combined_geoVad_all <- df_both[df_both$scrubbed_species_binomial %in% sppfreq_combine$sci_name,     ]
nrow(raw_combined_geoVad_rare)
nrow(raw_combined_geoVad_all)
nrow(df_both)
occTomap <- function(inputocc,
                     bgMapRes=1,
                     TypeOfMap="rawNum"){
  bgMap <- raster(resolution=bgMapRes)
  sp::coordinates(inputocc) <- ~ longitude + latitude
  pos <- raster::cellFromXY(bgMap,inputocc)
  inputocc$raster_id <- pos  
  if(TypeOfMap == "rawNum"){    
  }
  if(TypeOfMap == "sppNum"){
    dups <- duplicated(inputocc@data[c("scrubbed_species_binomial","raster_id")])
    inputocc <- inputocc[!dups,]
  }  
  occfreq <- table(inputocc$raster_id)
  bgValue <- values(bgMap)
  pos_bg <- as.numeric(names(occfreq))
  bgValue[pos_bg] <- occfreq
  values(bgMap) <- bgValue  
  return(bgMap)
}
rawNum_rare <- occTomap(inputocc= raw_combined_geoVad_rare,TypeOfMap="rawNum" )
rawNum_all  <- occTomap(inputocc= raw_combined_geoVad_all ,TypeOfMap="rawNum" )
sppNum_rare <- occTomap(inputocc= raw_combined_geoVad_rare,TypeOfMap="sppNum")
sppNum_all <- occTomap(inputocc= raw_combined_geoVad_all,TypeOfMap="sppNum")
finalRes=1
alt <- raster("wc_alt_10m.tif")
alt <- crop(alt,extent(c(-180, 180, -90, 90))  )
alt <- raster::extend(alt,extent(c(-180, 180, -90, 90)))
alt_mean <- aggregate(alt,fact=finalRes/res(alt)[1],fun=mean)
sppNum_rare[!is.na(alt_mean) & is.na(sppNum_rare)] <- 0
rawNum_all[!is.na(alt_mean) & is.na(rawNum_all)] <- 0
Margalef_rare <- (sppNum_rare-1)/log(rawNum_all)
names(Margalef_rare) <- "Margalef"
Menhinick_rare <- sppNum_rare/sqrt(rawNum_all)
names(Menhinick_rare) <- "Menhinick"
Margalef_rare[rawNum_all==1 ] <- 0
Margalef_rare[sppNum_rare==0] <- 0
Margalef_rare[rawNum_all==0] <- NA
Menhinick_rare[rawNum_all==0] <- NA
plot(stack(Margalef_rare,Menhinick_rare))
writeRaster(sppNum_rare,"sppNum_rare_1DD_20190620.tif",overwrite=TRUE)
writeRaster(sppNum_all,"sppNum_all_1DD_20190620.tif",overwrite=TRUE)
writeRaster(rawNum_rare,"rawNum_rare_1DD_20190620.tif",overwrite=TRUE)
writeRaster(rawNum_all,"rawNum_all_1DD_20190620.tif",overwrite=TRUE)
writeRaster(Margalef_rare,"Margalef_rare_1DD_20190620.tif",overwrite=TRUE)
writeRaster(Menhinick_rare,"Menhinick_rare_1DD_20190620.tif",overwrite=TRUE)