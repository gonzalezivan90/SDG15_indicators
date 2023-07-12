# for(i in 1:100) {source('N:/Mi unidad/git/SDG15_indicators/SDG_riparian_PEC/SDG_riparian-forest_PEC_Hansen_AP_ms.R')}
# 
## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(sp) # Operaciones geográficas


## Definir Ruta de trabajo ----
root <- 'I:/Mi unidad/Datos-NASA-ODS15/Talleres/Conectividad/' # Usar / o \\. Cada uno cambia su ruta
setwd( root ) # asignar ruta de trabajo

# Asegurarnos que estamos en la carpeta de trabajo
list.dirs(path = '.', full.names = TRUE, recursive = FALSE)

## Crear carpeta con resultados de poligonos
outDir <- c('07_poligonos_AP_HIF/')
#outDir <- c('07_poligonos_AP_PEC/')
dir.create(outDir, recursive = TRUE) # Algunos warnings. No es grave



# pecoec_FSII_at2018_990m_50k_moll_crk_col
## Bosques -----
## Listar archivos TIF riparios nacionales finales -----
 (archivos_bosques <- list.files(path = '04_resultados/HIF/', pattern = 'pecoec.+moll_crk.+.tif$',  full.names = TRUE) )
# (archivos_bosques <- list.files(path = '04_resultados/pec5km/', pattern = 'PEC.+_crk.tif$',  full.names = TRUE) )
tif_example <- raster(archivos_bosques[1])

## Cargar poligono -----
load(file = '01_datos-originales/peco-ap.RData') #  "pe1"   "co1"   "ec1"   "peco1" "ap"    "apco1" "apec1" "appe1" "aoi"

## Ajustar ruta total o relativa
ruta_poligonos <- '01_datos-originales/aoi_peco_ap.shp'
file.exists( ruta_poligonos ) # Debe resultar en TRUE

## Cargar poligonos en R como objeto espacial
poligonosCompletos <- readOGR(ruta_poligonos, encoding = 'utf8')
poligonosCompletos$cod <- paste0(poligonosCompletos$id, poligonosCompletos$type)
tif_example@crs@projargs
if(poligonosCompletos@proj4string@projargs != tif_example@crs@projargs){
  poligonosCompletos <- spTransform(poligonosCompletos, CRSobj = tif_example@crs)
}

## Iteramos sobre todas las poligonos
poligonos <- poligonosCompletos
# plot(poligonos, add = TRUE)

borrar_capas_raster <- FALSE

## ITERAR ----
for (i in sample(1:nrow(poligonos))){ # } # i = 1
  
  ## Extraer poligono ----
  (id_poligono <- poligonos$OBJECTID[i])
  poligono.i <- poligonos[i, ]
  print( paste0(' --- poligono ', i, '-', nrow(poligonos),  '  ID: ', id_poligono))
  # plot(poligono.i, add = TRUE, col = 'red', border = 'blue')
  # plot(poligono.i, add = F, col = 'red', border = 'blue')
  
  poligono_extent <- c(poligono.i@bbox[c(1, 2, 3, 4)]) # xmin ymin xmax ymax
  
  country.code <- gsub('.+_crk_|.tif', '', poligonos$cou[i])
  archivos_bosques.i <- grep(paste0('crk_',country.code, '.+.tif'), archivos_bosques, value = TRUE)
  #archivos_bosques.i <- archivos_bosques
  
  ## Iterar para cada poligono con las capas de bosque disponible
  for ( l  in sample(1:length(archivos_bosques.i))){ # l = 1
    (archivo_bosque <- archivos_bosques.i[l])
    (anio <- gsub('.tif|.+bosqueFecha_|_crk.+', '', basename(archivo_bosque)))
    print(anio)
    tn <- paste0(outDir, '/', poligono.i$cod, '_', anio,'.csv')
    if ( !file.exists(tn)){
      
      (rn <- paste0(outDir, '/', poligono.i$cod, '_', anio,'.tif'))
      if(!file.exists(rn)) { 
      tif.l <- raster(archivo_bosque)
      crop.l <- crop(tif.l, poligono.i)
      mask.l <- mask(crop.l, poligono.i)
        writeRaster(mask.l, file = rn) 
      } else{
        mask.l <- raster(rn) 
      }
      
      aoi_rn <- paste0(outDir, '/', poligono.i$cod, '.tif')
      if(!file.exists(aoi_rn)){
      aoi <- rasterize(poligono.i, mask.l, field = 1, bg = NA, filename = aoi_rn)
      } else {
        aoi <- raster(aoi_rn)
      }
      
      stat.sum <- cellStats(mask.l, sum)
      stat.mean <- cellStats(mask.l, mean)
      stat.med <- cellStats(mask.l, median)
      # plot(mask.l)
      # plot(aoi)
      aoi.pix <- length(which(!is.na(aoi[]))) # == cellStats(aoi, sum)
      for.pix <- length(which(!is.na(mask.l[]))) # == cellStats(aoi, sum)
      # mask.l2 <- mask.l
      #mask.l2[is.na(mask.l2[])] <- (-999); plot(mask.l2)
      #ncell(mask.l)
      
      
      (stat.file <- data.frame(id = poligono.i$cod,
                               cou= poligono.i$cou,
                               ap = poligono.i$type,
                               yyyy = anio, 
                               sum = stat.sum,
                               av = stat.mean,
                               med = stat.med,
                               mcelltot= aoi.pix,
                               mcellfor= for.pix))
      write.csv(stat.file, file = tn)
    }
  }
}



#### Compilar resultados ------

(archivos_cifras <- list.files(pattern = '.csv$',
                               path = outDir, 
                               full.names = TRUE))
resultados_poligonos <- NULL
system.time({ 
  for (i in 1:length(archivos_cifras)){ # i = 1
    tabla <- read.csv(archivos_cifras[i])
    colnames(tabla)[colnames(tabla) == 'poligono'] <- 'poligonoid'
    resultados_poligonos <- rbind(resultados_poligonos, tabla)
  }
})


head(resultados_poligonos)


print(getwd())
write.csv(x = resultados_poligonos , 
          file = paste0(outDir, '/Compilado_indicadores_', 'poligonos' ,'_', nrow(poligonos) ,'pols_', Sys.Date(), '.csv'), 
          row.names = FALSE)
