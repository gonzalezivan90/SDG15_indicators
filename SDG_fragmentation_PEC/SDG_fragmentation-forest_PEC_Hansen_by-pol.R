# for(i in 1:100) {source('N:/Mi unidad/git/SDG15_indicators/SDG_fragmentation_PEC/SDG_fragmentation-forest_PEC_Hansen_byplo.R')}
## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(sp) # Operaciones geográficas


## Definir Ruta de trabajo ----
root <- 'C:/temp/SDG_fragmentation_PEC/' # Usar / o \\. Cada uno cambia su ruta
setwd( root ) # asignar ruta de trabajo

# Asegurarnos que estamos en la carpeta de trabajo
list.dirs(path = '.', full.names = TRUE, recursive = FALSE)

## Crear carpeta con resultados de poligonos
outDir <- c('07_poligonos_AP')
dir.create(outDir, recursive = TRUE) # Algunos warnings. No es grave

## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
root2 <- paste0(paste0(root2, collapse = '/'), '/')

## Aca la configuración para QGIS3.14, pero abajo hay instrucciones para otras versiones. #CAMBIAR ---
(execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.28.3"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.28.3"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.28.3"\\bin\\o4w_env.bat && gdal_calc '))

## Mirar la respuesta del sistema a conexion a gdal_calc
(gdalcalc <- (system(paste0(execGDAL, ' --help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
(GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))



## Bosques -----
## Listar archivos TIF riparios nacionales finales -----
(archivos_bosques <- list.files(path = '05_resultados-fragmentacion/', 
                                pattern = 'frag.+[0-9].tif$', 
                                full.names = TRUE) )
tif_example <- raster(archivos_bosques[1])

## Cargar poligono -----
# co <- readOGR('../SDG_fragmentation_PEC/01_datos-originales/CO_MGN_ANM_DPTOS.shp')
# ec <- readOGR('../SDG_fragmentation_PEC/01_datos-originales/EC_PROVINCIAS_2019.shp')
# pe <- readOGR('../SDG_fragmentation_PEC/01_datos-originales/PE_Areas_Administrativas.shp')
#
# cowgs <- spTransform(co, CRSobj = tif_example@crs)
# ecwgs <- spTransform(ec, CRSobj = tif_example@crs)
# pewgs <- spTransform(pe, CRSobj = tif_example@crs)
# 
# cowgs$one <- 1
# ecwgs$one <- 1
# pewgs$one <- 1
# 
# co1 <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(cowgs), data = data.frame(aoi = 1))
# ec1 <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(ecwgs), data = data.frame(aoi = 1))
# pe1 <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(pewgs), data = data.frame(aoi = 1))
# co1$cou <- 'co'
# ec1$cou <- 'ec'
# pe1$cou <- 'pe'
# peco1 <- bind(co1, ec1, pe1)
# writeOGR(peco1, '01_datos-originales', 'peco3pols', driver = 'ESRI Shapefile')

# ap1 <- readOGR('01_datos-originales/Ap1.shp')
# ap1 <- ap1[grep('Gal.+pagos', ap1$NAME, invert = TRUE), ]
# ap2 <- readOGR('01_datos-originales/Ap2.shp')
# ap3 <- readOGR('01_datos-originales/Ap3.shp')
# apFull <- rbind(ap1, ap2, ap3, makeUniqueIDs = TRUE)
# head(apFull)
# ap <- apFull[apFull$IUCN_CAT %in% c('I', 'II', 'III', 'IV'), ]
# ap <- ap[grep('Gal.+pagos|Old Providence|The Peak|Jhonny Cay', ap$NAME, invert = TRUE), ]
# plot(ap)
# writeOGR(ap, '01_datos-originales', 'ap_peco', driver = 'ESRI Shapefile')

# plot(peco1)
# plot(ap, add = T, col = 2)
# plot(intap, add = T, col = rgb(.5, .1, 1., .3))
# 
# intap <- raster::intersect(ap, peco1)
# coPolsAp <- intap[intap$cou == 'co', 1]
# coAp <- rbind( coPolsAp[1, ], coPolsAp[2:nrow(coPolsAp), ], makeUniqueIDs = TRUE)
# apco1 <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(coAp), data = data.frame(cou = 'co', type = 'ap'))
# apec1 <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(intap[intap$cou == 'ec', ]), data = data.frame(cou = 'ec', type = 'ap'))
# appe1 <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(intap[intap$cou == 'pe', ]), data = data.frame(cou = 'pe', type = 'ap'))
# aoi <- raster::bind(peco1, apco1, apec1, appe1)
# aoi$id <- 1:nrow(aoi)
# aoi$OBJECTID <- paste0(aoi$cou, 'x', aoi$id)

# writeOGR(aoi, '01_datos-originales', 'aoi_peco_ap', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
# save(pe1, co1, ec1, peco1, ap, apco1, apec1, appe1, aoi, file = '01_datos-originales/peco-ap.RData')
load(file = '01_datos-originales/peco-ap.RData') # pe1, co1, ec1, peco1

## Ajustar ruta total o relativa
ruta_poligonos <- '01_datos-originales/aoi_peco_ap2.shp'
file.exists( ruta_poligonos ) # Debe resultar en TRUE

## Cargar poligonos en R como objeto espacial
poligonosCompletos <- readOGR(ruta_poligonos, encoding = 'utf8')
poligonosCompletos@proj4string@projargs

head(poligonosCompletos@data, 10) # Revisar tabla de atributos
tail(poligonosCompletos@data, 10) # Revisar tabla de atributos 

# plot(poligonosCompletos, axes = TRUE, main = 'poligonos') # Graficar sencillo la capa

length(poligonosCompletos$OBJECTID) # Validar cantidad de objetos
nrow(poligonosCompletos) # Validar cantidad de objetos
length( unique(poligonosCompletos$OBJECTID) ) # Validar número de valores únicos en campo ID
# if( length( unique(poligonosCompletos$OBJECTID) ) == 0){
#   poligonosCompletos$OBJECTID <- 1:nrow(poligonosCompletos) # creamos un ID consecutivo
#   ## Escribir la tabla con el ID
#   write.dbf(poligonosCompletos@data, file = '01_datos-originales/aoi_peco_ap.shp')
# }

## Iteramos sobre todas las poligonos
poligonos <- poligonosCompletos
# plot(poligonos, add = TRUE)


borrar_capas_raster <- FALSE

for (i in sample(1:nrow(poligonos))){ # } # i = 5
  # i = which(poligonos$OBJECTID == 17)
  
  (id_poligono <- poligonos$OBJECTID[i])
  print( paste0(' --- poligono ', i, '-', nrow(poligonos),  '  ID: ', id_poligono))
  
  
  ## Extraer poligono ----
  poligono_poligono <- poligonos[i, ]
  # plot(poligono_poligono, add = TRUE, col = 'red', border = 'blue')
  # plot(poligono_poligono, add = F, col = 'red', border = 'blue')
  
  poligono_extent <- c(poligono_poligono@bbox[c(1, 2, 3, 4)]) # # xmin ymin xmax ymax
  names(poligono_extent) <- c('xmin', 'ymin', 'xmax', 'ymax')
  
  
  ## Iterar para cada poligono con las capas de bosque disponible
  for ( l  in 1:length(archivos_bosques)){ # l = 5
    (archivo_bosques <- archivos_bosques[l])
    (anio <- gsub('.tif|frag.+_', '', basename(archivo_bosques)))
    
    ## Archivos de salida
    
    (archivo_area_estudio <- paste0(outDir, '/poligono_', id_poligono, '.tif' ))
    (archivo_cifras_poligono <- paste0(outDir, '/poligono_', id_poligono, '_', anio, '.csv' ))
    (archivo_corte_poligono <- paste0(outDir, '/poligono_', id_poligono, '_', anio, '.tif' ))
    
    # Cortar el buffer con bosque por poligono
    if(! file.exists(archivo_area_estudio) ){
      print(archivo_area_estudio)
      corte_aoi <- tryCatch(
        gdalUtilities::gdal_rasterize(src_datasource = ruta_poligonos,
                                      dst_filename = archivo_area_estudio,
                                      tr = c(res(tif_example)), 
                                      te = poligono_extent, # xmin ymin xmax ymax
                                      ot = 'Byte',
                                      init = 0, burn = 1,
                                      where = paste0('"OBJECTID"=', id_poligono, ''),
                                      co = c("NBITS=1", "COMPRESS=DEFLATE")),
        error = function(e) NULL)
      
      if( is.null (corte_aoi)){
        
        archivo_temp_A <- gsub('.tif', '_tempA.tif', archivo_area_estudio)
        if(! file.exists(archivo_temp_A) ){
          gdalUtilities::gdal_rasterize(dst_filename = archivo_temp_A,
                                        src_datasource = ruta_poligonos, 
                                        tr = c(res(tif_example)), 
                                        te = poligono_extent, # xmin ymin xmax ymax
                                        ot = 'Byte',
                                        init = 0, 
                                        burn = 1,where = paste0('"OBJECTID"=', id_poligono, ''),
                                        co = c("NBITS=1", "COMPRESS=DEFLATE"))
        }
        
        
        archivo_temp_B <- gsub('.tif', '_tempB.tif', archivo_area_estudio)
        if(! file.exists(archivo_temp_B) ){
          gdalUtilities::gdalwarp(dstfile = archivo_temp_B,
                                  srcfile = capa_buff_nacional, 
                                  tr = c(res(tif_example)), 
                                  te = poligono_extent, # xmin ymin xmax ymax
                                  ot = 'Byte',
                                  co = c("NBITS=1", "COMPRESS=DEFLATE"))
        }
        
        if(! file.exists(archivo_area_estudio) ){
          print(system.time(
            system(intern = TRUE,
                   paste0(execGDAL,
                          #' --calc="0 + (1*(logical_and(A==1,B==1)))" ',
                          ' --calc="A*B" ',
                          '-A ', root2, '\\', archivo_temp_A,
                          ' -B ', root2, '\\',archivo_temp_B,
                          ' --outfile=', root2, '\\', archivo_area_estudio,
                          ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                   )))) # 1713.92s - 2000s
        }
        
        print( paste0(' Sin datos para la poligono con ID OBJECTID:', id_poligono, 
                      ' - capa:', archivo_bosques))
        next()
        
        
      }
    }
    
    
    ## Ejecutar si no existen las capas o archivos de estadisticas
    if( !file.exists(archivo_corte_poligono)  ){ #
      
      # Cortar el buffer con bosque por poligono
      if(! file.exists(archivo_corte_poligono) ){
        print(archivo_corte_poligono)
        corte_bos <- tryCatch(
          gdalUtilities::gdalwarp(srcfile = archivo_bosques,
                                  dstfile = archivo_corte_poligono,
                                  cutline = ruta_poligonos,
                                  cwhere = paste('"OBJECTID"=', id_poligono),
                                  #dstnodata = 999,
                                  srcnodata = 'None',
                                  nomd = TRUE,
                                  crop_to_cutline = TRUE,
                                  co = c("NBITS=1", "COMPRESS=DEFLATE"),
                                  overwrite = TRUE),
          error = function(e) NULL)

        if( is.null ( corte_bos )){
          
          if(! file.exists(archivo_area_estudio) ){
            
            archivo_temp_A <- gsub('.tif', '_tempA.tif', archivo_area_estudio)
            if(! file.exists(archivo_temp_A) ){
              gdalUtilities::gdal_rasterize(dst_filename = archivo_temp_A,
                                            src_datasource = ruta_poligonos, 
                                            tr = c(res(tif_example)), 
                                            te = poligono_extent, # xmin ymin xmax ymax
                                            ot = 'Byte',
                                            init = 0, 
                                            burn = 1,where = paste0('"OBJECTID"=', id_poligono, ''),
                                            co = c("NBITS=1", "COMPRESS=DEFLATE"))
            }
          }
          
          
          archivo_temp_B <- gsub('.tif', '_temp.tif', archivo_corte_poligono)
          if(! file.exists(archivo_temp_B) ){
            print(system.time(
              gdalUtilities::gdalwarp(dstfile = archivo_temp_B,
                                    srcfile = archivo_bosques, 
                                    tr = c(res(tif_example)), 
                                    te = poligono_extent, # xmin ymin xmax ymax
                                    ot = 'Byte',
                                    co = c("NBITS=1", "COMPRESS=DEFLATE"))
            ))
          } # 74
          
          if(! file.exists(archivo_corte_poligono) ){
            print(system.time(
              system(intern = TRUE,
                     paste0(execGDAL,
                            #' --calc="0 + (1*(logical_and(A==1,B==1)))" ',
                            ' --calc="A*B" ',
                            '-A ', root2, '\\', archivo_area_estudio,
                            ' -B ', root2, '\\',archivo_temp_B,
                            ' --outfile=', root2, '\\', archivo_corte_poligono,
                            ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                     )))) #137
            # ' --extent=intersect ', 600
          } 
          
          print( paste0(' Sin datos para la poligono con ID OBJECTID:', id_poligono, 
                        ' - capa:', archivo_bosques))
          next()
        }
      }
    }
    
    ## Calcular estadisticas si no hay archivo escrito
    if( ! file.exists(archivo_cifras_poligono) ){
      
      ## Estadistica zona riparia total -----
      estadisticas_poligono <- capture.output(
        gdalUtilities::gdalinfo(archivo_corte_poligono, 
                                stats = TRUE, hist = TRUE) )
      
      dimensiones_poligono <- as.numeric(
        strsplit(gsub('[[:alpha:]]| ','', 
                      grep('Size is', estadisticas_poligono, value = TRUE) ),
                 ",")[[1]])
      
      celdas_poligono <- Reduce(f = '*', x = dimensiones_poligono)
      (promedio_poligono <- as.numeric(
        gsub(  " | STATISTICS_MEAN=", "",
               grep('STATISTICS_MEAN', 
                    estadisticas_poligono, value = TRUE)) ) )
      
      (pixeles_poligono <- celdas_poligono * promedio_poligono)
      
      
      ## Estadistica zona riparia con bosque ------------
      
      estadisticas_aoi <- capture.output(
        gdalUtilities::gdalinfo(archivo_area_estudio, 
                                stats = TRUE, hist = TRUE) )
      
      dimensiones_bosrip <- as.numeric(
        strsplit(gsub('[[:alpha:]]| ','', 
                      grep('Size is', estadisticas_aoi, value = TRUE) ),
                 ",")[[1]])
      
      celdas_bosrip <- Reduce(f = '*', x = dimensiones_bosrip)
      
      (promedio_bosrip <- as.numeric(
        gsub(  " | STATISTICS_MEAN=", "",
               grep('STATISTICS_MEAN', 
                    estadisticas_aoi, value = TRUE)) ) )
      
      (pixeles_aoi <- celdas_bosrip * promedio_bosrip)
      
      
      tabla_poligono <- data.frame(poligonoid = id_poligono,
                                   anio = anio,
                                   totpix = pixeles_aoi,
                                   totpixbos = pixeles_poligono,
                                   propbos = pixeles_poligono/pixeles_aoi,
                                   porcpbos = pixeles_poligono/pixeles_aoi * 100
      )
      write.csv2(x = tabla_poligono, file = archivo_cifras_poligono, row.names = FALSE)
      # write.csv(x = tabla_poligono, file = archivo_cifras_poligono, row.names = FALSE)
    }
    
    
    ## Borar corte raster para la poligono en cada anio particular
    if (borrar_capas_raster){
      file.remove(archivo_corte_poligono,
                  paste0(archivo_corte_poligono, '.aux.xml'))
    }
    
  }
  
  buffers_raster <-  paste0(outDir, '/poligono', id_poligono, '_buff', c(30, 100), '.tif' )
  
  ## Borar corte raster de zona riparia en cada por poligono
  if (borrar_capas_raster){
    file.remove(buffers_raster,
                paste0(buffers_raster, '.aux.xml'))
  }
}



# #### Compilar resultados ------
# 
(archivos_cifras <- list.files(pattern = 'poligono.+[0-9].csv$',
                              path = outDir, full.names = TRUE))
resultados_poligonos <- NULL
system.time({
  for (i in 1:length(archivos_cifras)){ # i = 1
    tabla <- read.csv2(archivos_cifras[i])
    colnames(tabla)[colnames(tabla) == 'poligono'] <- 'poligonoid'
    resultados_poligonos <- rbind(resultados_poligonos, tabla)
  }
})
# 
# 
head(resultados_poligonos)
head(archivos_cifras)
#colnames(resultados_poligonos) <- gsub('','', resultados_poligonos)

## Borrar archivos incorrectos ---
(filas_incorr <- resultados_poligonos[which(resultados_poligonos$propbos > 1 | 
                                              resultados_poligonos$propbos == 0),])
nrow(filas_incorr) # Debe ser 0

if(nrow(filas_incorr) != 0){
  filas_incorr$tifs_borrar <- paste0(outDir, '/poligono_', filas_incorr$poligono,  
                                     '_', filas_incorr$anio, '.tif')
  filas_incorr$csv_borrar <- paste0(outDir, '/poligono_', filas_incorr$poligono, 
                                    '_', filas_incorr$anio, '.csv')

  sapply(filas_incorr$tifs_borrar, file.remove)
  sapply(paste0(filas_incorr$tifs_borrar, '.aux.xml'), file.remove)
  sapply(filas_incorr$csv_borrar, file.remove)
}

print(getwd())
write.csv(x = resultados_poligonos ,
          file = paste0('Compilado_indicadores_', 'poligonos' ,'_', nrow(poligonos) ,'pols_', Sys.Date(), '.csv'),
          row.names = FALSE)


##
pivotTable <- as.data.frame.matrix(
  xtabs(data = resultados_poligonos,
        porcpbos ~ poligonoid + anio
  ))

## Agregar prefijo a columnas
colnames(pivotTable) <- paste0('i_', colnames(pivotTable))

nrow(pivotTable)
head(pivotTable)

write.csv2(x = pivotTable ,
           file = paste0('Compilado2_indicadores_', 'ANP_', nrow(poligonos), 'pols_', Sys.Date(), '_bufferm.csv'),
           row.names = FALSE)


## Crear campos en tabla de shapefile
poligonos@data[, c(colnames(pivotTable))] <- NA

posicion_merge <- match(poligonos$OBJECTID, rownames(pivotTable))
poligonos@data[which(!is.na(posicion_merge)), colnames(pivotTable)] <- pivotTable[na.omit(posicion_merge), colnames(pivotTable)]


head(poligonos)
writeOGR(obj = poligonos, dsn = '.', overwrite_layer = TRUE,
         driver = 'ESRI Shapefile',
         layer = paste0('Indicador_fragmentacion', 'ANP_', nrow(poligonos), 'pols_', Sys.Date() ))
