findNeigb <- function(yy,xx,ddd,center){
  if(xx <1 |xx> ncol(ddd) | yy <1 | yy>nrow(ddd)  ) {
    zz = FALSE 
  } else if (is.na(ddd [yy,xx]) ){
    zz= FALSE
  } else {
    zz= TRUE 
  }  
  if(zz){zz = ddd[yy,xx]  } else{zz=center}
  return(zz)
}

calculateVelocity <- function(bio_now,bio_lgm,
                              flag_noise =TRUE,
                              flag_rmvSmall=TRUE){
  bio_diff <- bio_now - bio_lgm
    if(flag_noise){
    noise_values <- runif(ncell(bio_now),min=-0.05,max=0.05)
    noise_ly <- bio_now
    values(noise_ly) <- noise_values
    bio_now_update <- bio_now +  noise_ly
  } else {
    bio_now_update <- bio_now
  }  
  bio_m <- as.matrix(bio_now_update) 
  bio_sp_layer <-  bio_now_update
  bio_sp <- bio_m 
  bio_sp[!is.na(bio_sp)] <- -9999
  for(ii in 1:ncol(bio_m)  ){  
    print(ii/ncol(bio_m)  )
    for (jj in 1:nrow(bio_m)) {
      focus <- bio_m[jj,ii]
      if(!is.na(focus) ){
        a <- findNeigb(jj-1,ii-1,bio_m,focus)
        b <- findNeigb(jj-1,ii,bio_m,focus)
        c <- findNeigb(jj-1,ii+1,bio_m,focus)
        d <- findNeigb(jj,  ii-1,bio_m,focus)
        f <- findNeigb(jj,  ii+1,bio_m,focus)
        g <- findNeigb(jj+1,ii-1,bio_m,focus)
        h <- findNeigb(jj+1,ii,bio_m,focus)
        i <- findNeigb(jj+1,ii+1,bio_m,focus)        
        x_cellsize= res(bio_now)[1]*111.325 
        cells <- cellFromRowColCombine(bio_now, rownr=jj, colnr=ii)
        xy <- xyFromCell(bio_now, cells)
        y_cellsize=  cos(pi * xy[1,2] /180)*111.325         
        dz_dx <- ((c + 2*f + i) - (a + 2*d + g)) / (8 * x_cellsize)
        dz_dy <- ((g + 2*h + i) - (a + 2*b + c)) / (8 * y_cellsize)
        rise_run <- sqrt(dz_dx^2 + dz_dy^2)
        slope_degrees <- atan (rise_run) * 57.29578        
        bio_sp[jj,ii] <- rise_run
      } else {
        bio_sp[jj,ii] <- NA
      }
    }
  }
  bio_sp_update <- bio_sp
  if (flag_rmvSmall){
    bio_sp_update[bio_sp_update < 0.01] <- 0.01
  } 
  values(bio_sp_layer) <- bio_sp_update
  bio_velocity <- bio_diff/bio_sp_layer  
  return(bio_velocity)
}
library("raster")
bio1_now <- raster("wc1/10/bio1.bil")
bio1_lgm <- raster("wc_lgm/10m/cclgmbi1.tif")
bio1_now <- bio1_now/10
bio1_lgm <- bio1_lgm/10
bio12_now <- raster("wc1/10/bio12.bil")
bio12_lgm <- raster("wc_lgm/10m/cclgmbi12.tif")
bio12_velocity <- calculateVelocity(bio_now = bio12_now,
                                    bio_lgm = bio12_lgm)
bio1_velocity <- calculateVelocity(bio_now = bio1_now,
                                    bio_lgm = bio1_lgm)
plot(stack(bio1_now,  bio1_velocity,abs(bio1_velocity),
           bio12_now,bio12_velocity,abs(bio12_velocity)))
writeRaster(bio1_velocity,"/bio1_velocity_10m.tif")
writeRaster(bio12_velocity,"/bio12_velocity_10m.tif")
bio1_now <- raster("data_20181211/10_futureRare/cc85bi701.tif")
bio1_lgm <- raster("wc1/10/bio1.bil")
bio1_now <- bio1_now/10
bio1_lgm <- bio1_lgm/10
bio12_now <- raster("cc85bi7012.tif")
bio12_lgm <- raster("wc1/10/bio12.bil")
bio12_velocity <- calculateVelocity(bio_now = bio12_now,
                                    bio_lgm = bio12_lgm)
bio1_velocity <- calculateVelocity(bio_now = bio1_now,
                                   bio_lgm = bio1_lgm)
writeRaster(bio1_velocity,"/bio1_velocity_10m.tif")
writeRaster(bio12_velocity,"/bio12_velocity_10m.tif")