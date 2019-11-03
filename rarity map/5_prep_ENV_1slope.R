library("raster")
alt <- raster("WC_alt/wc_alt_10m.tif")

alt_m <- as.matrix(alt)
alt_sp <- alt_m
alt_sp[!is.na(alt_sp)] <- -9999
alt_sp_layer <-  alt
findNeigb <- function(yy,xx,ddd,center){
  if(xx <1 |xx> ncol(ddd) | yy <1 | yy>nrow(ddd)  ) {
    zz = FALSE # use the value from the center
  } else if (is.na(ddd [yy,xx]) ){
    zz= FALSE
  } else {
    zz= TRUE 
  }
  
  if(zz){zz = ddd[yy,xx]  } else{zz=center}
  return(zz)
}
for(ii in 1:ncol(alt_m)  ){    
  for (jj in 1:nrow(alt_m)) { 
    print(paste(ii,"in",ncol(alt_m),";",jj,"in",nrow(alt_m))  )
    focus <- alt_m[jj,ii]
    if(!is.na(focus) ){
      a <- findNeigb(jj-1,ii-1,alt_m,focus)
      b <- findNeigb(jj-1,ii,alt_m,focus)
      c <- findNeigb(jj-1,ii+1,alt_m,focus)
      d <- findNeigb(jj,  ii-1,alt_m,focus)
      f <- findNeigb(jj,  ii+1,alt_m,focus)
      g <- findNeigb(jj+1,ii-1,alt_m,focus)
      h <- findNeigb(jj+1,ii,alt_m,focus)
      i <- findNeigb(jj+1,ii+1,alt_m,focus)            
      x_cellsize= res(alt)[1]*111.325 
      cells <- cellFromRowColCombine(alt, rownr=jj, colnr=ii)
      xy <- xyFromCell(alt, cells)
      y_cellsize=  cos(pi * xy[1,2] /180)*111.325        
      dz_dx <- ((c + 2*f + i) - (a + 2*d + g)) / (8 * x_cellsize)
      dz_dy <- ((g + 2*h + i) - (a + 2*b + c)) / (8 * y_cellsize)
      rise_run <- sqrt(dz_dx^2 + dz_dy^2)
      slope_degrees <- atan (rise_run) * 57.29578      
      alt_sp[jj,ii] <- rise_run
    } else {
      alt_sp[jj,ii] <- NA
    }
  }
}
alt_sp_update <- alt_sp
alt_sp_update[alt_sp_update < 0.01] <- 0.01
values(alt_sp_layer) <- alt_sp_update
plot(stack(alt,alt_sp_layer))
writeRaster(alt_sp_layer,"/alt_sp_10m.tif"))
