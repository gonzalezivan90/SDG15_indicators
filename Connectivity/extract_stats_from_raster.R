library(gdalUtils)
library(rgdal)
library(raster)


setwd('I:/Mi unidad/Datos-NASA-ODS15/Talleres/Conectividad/04_resultados')

tifs <- list.files(pattern = '.tif$', recursive = TRUE)


co <- read


ans <- NULL
for (i in 1:length(tifs)){
  tif.i <-   tifs[i]
  info <- gdalUtils::gdalinfo(tif.i)
}
