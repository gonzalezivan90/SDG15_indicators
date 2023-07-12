library(gdalUtils)
library(rgdal)
library(raster)


setwd('I:/Mi unidad/Datos-NASA-ODS15/Talleres/Conectividad/04_resultados')

(tifs <- list.files(path = 'pec5km/', pattern = 'PEC_.+crk.+tif$', recursive = TRUE, full.names = TRUE))
outDir <- 'mask_pec5km'; dir.create(outDir)
rast_example <- raster(tifs[1])
load('C:/temp/SDG_fragmentation_PEC/01_datos-originales/peco-ap.RData') # pe1 co1 ec1 peco1 ap apco1 apec1 appe1 aoi  
aoi <- readOGR('C:/temp/SDG_riparian_PEC/01_datos-originales/aoi_peco_ap.shp')
plot(aoi)

aoi.i <- spTransform(aoi[is.na(aoi$type), ], CRSobj = rast_example@crs)


## Ajustar ruta total o relativa

ans <- NULL
ans2 <- NULL
for (i in 1:length(tifs)){ # i = 1
  (tif.i <-   tifs[i])
  (rn <- paste0(outDir, '/', basename(tif.i)))
  (tn <- gsub('tif$', 'csv', rn))
  (yy <- gsub('.+Fecha_|_crk.+','', tif.i))
  
  if(!file.exists(rn)) { 
    r.i <- raster(tif.i)
    # plot(r.i)
    crop.i <- crop(r.i, aoi.i) # # plot(crop.i)
    mask.i <- mask(crop.i, aoi.i)
    # plot(aoi)
    # plot(mask.i)
    writeRaster(mask.i, file = rn) 
  } else{
    mask.i <- raster(rn) 
  }
  
  #plot(mask.i, zlim = c(-100, 1000))
  ans[[i]]  <- data.frame(yy = yy, x = mask.i[mask.i[] != 0 & !is.na(mask.i[])])
  # aoi_rn <- paste0(outDir, '/', poligono.i$cod, '.tif')
  # if(!file.exists(aoi_rn)){
  #   aoi <- rasterize(poligono.i, mask.i, field = 1, bg = NA, filename = aoi_rn)
  # } else {
  #   aoi <- raster(aoi_rn)
  # }
  
  (stat.sum <- cellStats(mask.i, sum))
  (stat.mean <- cellStats(mask.i, mean))
  (stat.med <- cellStats(mask.i, median))
  # plot(mask.i)
  # plot(aoi)
  (aoi.pix <- length(which(!is.na(mask.i[])))) # == cellStats(aoi, sum)
  (for.pix <- length(which(mask.i[] != 0))) # == cellStats(aoi, sum)
  # mask.i2 <- mask.i
  #mask.i2[is.na(mask.i2[])] <- (-999); plot(mask.i2)
  #ncell(mask.i)
  
  (anio <- gsub('.+bosqueFecha_|_crk.+', '', basename(tif.i)))
  (stat.file <- data.frame(id = basename(tif.i),
                           yyyy = anio, 
                           sum = stat.sum,
                           av = stat.mean,
                           med = stat.med,
                           mcelltot= aoi.pix,
                           mcellfor= for.pix))
  ans2[[i]] <- stat.file
  write.csv(stat.file, file = tn)
  print(i)
}

library(ggplot2)
ansDF <- do.call(rbind, ans)
ansDF2 <- do.call(rbind, ans2)

write.csv(ansDF2, 'peco5km_results.csv')

head(ansDF)
ggplot(ansDF, aes(x = x, group = yy, color = yy))  + geom_density()


t(t(tapply(ansDF$x, INDEX = ansDF$yy, max)))
