# for(i in 1:100) {source('N:/Mi unidad/git/SDG15_indicators/SDG_riparian_PEC/SDG_riparian-forest_PEC_Hansen_AP_ms.R')}
# 
## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(sp) # Operaciones geográficas


## Definir Ruta de trabajo ----
root <- 'C:/temp/SDG_riparian_PEC/' # Usar / o \\. Cada uno cambia su ruta
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
(archivos_riparios <- list.files(path = '06_cruce-bosques-riparios', 
                                 pattern = 'bosque_ripario[0-9].+.tif$', 
                                 full.names = TRUE) )
tif_example <- raster(archivos_riparios[1])
## Zonas riparias -----

archivo_raster_buffer_30 <- paste0('03_rios-raster/', 'buffer30.tif')
archivo_raster_buffer_100 <- paste0('03_rios-raster/', 'buffer100.tif')


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
ruta_poligonos <- '01_datos-originales/aoi_peco_ap.shp'
file.exists( ruta_poligonos ) # Debe resultar en TRUE

## Cargar poligonos en R como objeto espacial
poligonosCompletos <- readOGR(ruta_poligonos, encoding = 'utf8')
poligonosCompletos@proj4string@projargs

head(poligonosCompletos@data, 10) # Revisar tabla de atributos
tail(poligonosCompletos@data, 10) # Revisar tabla de atributos 

# plot(poligonosCompletos, axes = TRUE, main = 'poligonos') # Graficar sencillo la capa

# length(poligonosCompletos$OBJECTID) # Validar cantidad de objetos
# nrow(poligonosCompletos) # Validar cantidad de objetos
# length( unique(poligonosCompletos$OBJECTID) ) # Validar número de valores únicos en campo ID
# if( length( unique(poligonosCompletos$OBJECTID) ) == 0){
#   poligonosCompletos$OBJECTID <- 1:nrow(poligonosCompletos) # creamos un ID consecutivo
#   ## Escribir la tabla con el ID
#   write.dbf(poligonosCompletos@data, file = '01_datos-originales/aoi_peco_ap.shp')
# }

## Iteramos sobre todas las poligonos
poligonos <- poligonosCompletos
# plot(poligonos, add = TRUE)


borrar_capas_raster <- FALSE

## ITERAR ----
for (i in (1:nrow(poligonos))){ # } # i = 4
  
  (id_poligono <- poligonos$OBJECTID[i])
  print( paste0(' --- poligono ', i, '-', nrow(poligonos),  '  ID: ', id_poligono))
  
  
  ## Extraer poligono ----
  poligono_poligono <- poligonos[i, ]
  # plot(poligono_poligono, add = TRUE, col = 'red', border = 'blue')
  # plot(poligono_poligono, add = F, col = 'red', border = 'blue')
  
  poligono_extent <- c(poligono_poligono@bbox[c(1, 2, 3, 4)]) # xmin ymin xmax ymax
  
  
  ## Iterar para cada poligono con las capas de bosque disponible
  for ( l  in sample(1:length(archivos_riparios))){ # l = 12
    (archivo_ripario <- archivos_riparios[l])
    (anio <- gsub('.tif|bos.+m_', '', basename(archivo_ripario)))
    (buffer <- gsub('m_.+|bos.+rio', '', basename(archivo_ripario)))
    
    ## Archivo de salida
    (archivo_corte_poligono_zona_rip <- paste0(outDir, '/poligono_', 
                                               id_poligono, '_buff', buffer, '.tif' ))
    
    ## Seleccionar buffer a cortar dependiendo del buffer del bosque
    capa_buff_nacional <- ifelse(buffer == 30,
                                 archivo_raster_buffer_30,
                                 archivo_raster_buffer_100)
    
    archivo_corte_poligono <- paste0(outDir, '/poligono_', 
                                     id_poligono, '_buff', buffer,'_', anio, '.tif' )
    archivo_cifras_poligono <- paste0(outDir, '/poligono_', 
                                      id_poligono, '_buff', buffer,'_', anio, '.csv' )
    
    
    ## Ejecutar si no existen las capas o archivos de estadisticas
    #if( ! file.exists(archivo_corte_poligono) & !file.exists(archivo_cifras_poligono) ){
    
    # Cortar el buffer completo por poligono
    if(! file.exists(archivo_corte_poligono_zona_rip) ){
      
      # file.exists(capa_buff_nacional)
      # file.exists(ruta_poligonos)
      print(archivo_corte_poligono_zona_rip)
      corte_rip_poligono <-  tryCatch(
        gdalUtilities::gdalwarp(srcfile = capa_buff_nacional,
                                dstfile = archivo_corte_poligono_zona_rip,
                                cutline = ruta_poligonos,
                                cwhere = paste0('"OBJECTID"=', id_poligono, ''),
                                #srcnodata = 0,
                                srcnodata = 'None',
                                #dstnodata = 999,
                                crop_to_cutline = TRUE,
                                nomd = TRUE,
                                overwrite = TRUE,
                                co = c("NBITS=1", "COMPRESS=DEFLATE")),
        error = function(e) NULL)
      # rt = raster(corte_rip_poligono); plot(rt, axes = TRUE); summary(rt)
      
      if( is.null ( corte_rip_poligono ) ){
        
        archivo_temp_A <- gsub('.tif', '_tempA.tif', archivo_corte_poligono_zona_rip)
        if(! file.exists(archivo_temp_A) ){
          
          gdalUtilities::gdal_rasterize(dst_filename = archivo_temp_A,
                                        src_datasource = ruta_poligonos, 
                                        tr = c(res(tif_example)), 
                                        te = poligono_extent, # xmin ymin xmax ymax
                                        ot = 'Byte', init = 0, burn = 1,
                                        where = paste0('"OBJECTID"=', id_poligono, ''),
                                        co = c("NBITS=1", "COMPRESS=DEFLATE"))
        }
        
        
        archivo_temp_B <- gsub('.tif', '_tempB.tif', archivo_corte_poligono_zona_rip)
        if(! file.exists(archivo_temp_B) ){
          gdalUtilities::gdalwarp(dstfile = archivo_temp_B,
                                  srcfile = capa_buff_nacional, 
                                  tr = c(res(tif_example)), 
                                  te = poligono_extent, # xmin ymin xmax ymax
                                  ot = 'Byte',
                                  co = c("NBITS=1", "COMPRESS=DEFLATE"))
        }
        
        if(! file.exists(archivo_corte_poligono_zona_rip) ){
          print(system.time(
            system(intern = TRUE,
                   paste0(execGDAL,
                          #' --calc="0 + (1*(logical_and(A==1,B==1)))" ',
                          ' --calc="A*B" ',
                          '-A ', root2, '\\', archivo_temp_A,
                          ' -B ', root2, '\\',archivo_temp_B,
                          ' --outfile=', root2, '\\', archivo_corte_poligono_zona_rip,
                          ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                   )))) # 1713.92s - 2000s
        }
        
        
        # raster1 <- raster(archivo_corte_poligono_zona_rip_temp)
        # raster2 <- raster(capa_buff_nacional)
        # mult <- raster1 * raster2 
        
        
        print( paste0(' Sin datos para la poligono con ID OBJECTID:', id_poligono, 
                      ' - capa:', archivo_ripario))
        next()
      }
    }
    
    
    
    
    # Cortar el buffer con bosque por poligono
    if(! file.exists(archivo_corte_poligono) ){
      print(archivo_corte_poligono)
      print(system.time(
        corte_bos_rip <- tryCatch(
        gdalUtilities::gdalwarp(srcfile = archivo_ripario,
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
      ))
      # rt2 = raster(archivo_corte_poligono, legend = TRUE, main =  Sys.time()); plot(rt2, axes = TRUE); summary(rt2)
      # (estadisticas_bosrip <- capture.output(gdalUtilities::gdalinfo(archivo_corte_poligono, stats = TRUE, hist = TRUE) ))
      
      if( is.null ( corte_bos_rip )){
        
        # poligono_4_buff100_tempA
        # (archivo_temp_A <- gsub('.tif', '_tempA.tif', archivo_corte_poligono))
        (archivo_temp_A <- gsub('.tif', '_tempA.tif', archivo_corte_poligono_zona_rip))
        
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
        
        archivo_temp_B <- gsub('.tif', '_temp.tif', archivo_corte_poligono)
        if(! file.exists(archivo_temp_B) ){
          print(system.time(
            gdalUtilities::gdalwarp(dstfile = archivo_temp_B,
                                    srcfile = archivo_ripario,
                                    tr = c(res(tif_example)),
                                    te = poligono_extent, # xmin ymin xmax ymax
                                    ot = 'Byte',
                                    co = c("NBITS=1", "COMPRESS=DEFLATE")) # 107
          ))
        }
        
        if(! file.exists(archivo_corte_poligono) ){
          print(system.time(
            system(intern = TRUE,
                   paste0(execGDAL,
                          #' --calc="0 + (1*(logical_and(A==1,B==1)))" ',
                          ' --calc="A*B" ',
                          '-A ', root2, '\\', archivo_temp_A,
                          ' -B ', root2, '\\',archivo_temp_B,
                          ' --outfile=', root2, '\\', archivo_corte_poligono,
                          #' --extent=intersect ', # 5727
                          ' --type=Byte --NoDataValue=999 --co="NBITS=1" ',
                          '--co="COMPRESS=DEFLATE" --quiet'
                   ))
          )) # 5776.23
        }
        
        print( paste0(' Sin datos para la poligono con ID OBJECTID:', id_poligono, 
                      ' - capa:', archivo_ripario))
        next()
      }
    }
    #}
    
    
    ## Calcular estadisticas si no hay archivo escrito
    if( ! file.exists(archivo_cifras_poligono) ){
      
      ## Estadistica zona riparia total -----
      estadisticas_poligono <- capture.output(
        gdalUtilities::gdalinfo(archivo_corte_poligono_zona_rip, 
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
      
      (pixeles_riparias_poligono <- celdas_poligono * promedio_poligono)
      
      
      ## Estadistica zona riparia con bosque ------------
      
      estadisticas_bosrip <- capture.output(
        gdalUtilities::gdalinfo(archivo_corte_poligono, 
                                stats = TRUE, hist = TRUE) )
      
      dimensiones_bosrip <- as.numeric(
        strsplit(gsub('[[:alpha:]]| ','', 
                      grep('Size is', estadisticas_bosrip, value = TRUE) ),
                 ",")[[1]])
      
      celdas_bosrip <- Reduce(f = '*', x = dimensiones_bosrip)
      # (promedio_bosrip <- as.numeric(
      #   gsub(  "[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
      #          grep('STATISTICS_MEAN=', 
      #               estadisticas_bosrip, value = TRUE)) ) )
      
      (promedio_bosrip <- as.numeric(
        gsub(  " | STATISTICS_MEAN=", "",
               grep('STATISTICS_MEAN', 
                    estadisticas_bosrip, value = TRUE)) ) )
      
      (pixeles_bosrip <- celdas_bosrip * promedio_bosrip)
      
      
      tabla_poligono <- data.frame(poligonoid = id_poligono,
                                   anio = anio,
                                   buffer = buffer,
                                   totpixrip = pixeles_riparias_poligono,
                                   totpixbos = pixeles_bosrip,
                                   propbosrip = pixeles_bosrip/pixeles_riparias_poligono
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



#### Compilar resultados ------

(archivos_cifras <- list.files(pattern = 'poligono.+[0-9].csv$',
                              path = outDir, 
                              full.names = TRUE))
resultados_poligonos <- NULL
system.time({ 
  for (i in 1:length(archivos_cifras)){ # i = 1
    tabla <- read.csv2(archivos_cifras[i])
    colnames(tabla)[colnames(tabla) == 'poligono'] <- 'poligonoid'
    resultados_poligonos <- rbind(resultados_poligonos, tabla)
  }
})


head(resultados_poligonos)
head(archivos_cifras)

## Borrar archivos incorrectos ---
(filas_incorr <- resultados_poligonos[which(resultados_poligonos$propbosrip > 1 |
                                              resultados_poligonos$propbosrip == 0),])
nrow(filas_incorr) # Debe ser 0

if(nrow(filas_incorr) != 0){
  filas_incorr$tifs_borrar <- paste0(outDir, '/poligono_', filas_incorr$poligono, '_buff', filas_incorr$buffer, 
                                     '_', filas_incorr$anio, '.tif')
  filas_incorr$csv_borrar <- paste0(outDir, '/poligono_', filas_incorr$poligono, '_buff', filas_incorr$buffer, 
                                    '_', filas_incorr$anio, '.csv')
  
  sapply(filas_incorr$tifs_borrar, file.remove)
  sapply(paste0(filas_incorr$tifs_borrar, '.aux.xml'), file.remove)
  sapply(filas_incorr$csv_borrar, file.remove)
}

print(getwd())
write.csv(x = resultados_poligonos , 
          file = paste0('Compilado_indicadores_', 'poligonos' ,'_', nrow(poligonos) ,'pols_', Sys.Date(), '.csv'), 
          row.names = FALSE)
write.csv2(x = resultados_poligonos , 
          file = paste0('Compilado2_indicadores_', 'poligonos' ,'_', nrow(poligonos) ,'pols_', Sys.Date(), '.csv'), 
          row.names = FALSE)


## 
pivotTable30 <- as.data.frame.matrix(
  xtabs(data = resultados_poligonos[resultados_poligonos$buffer == 30, ],
        propbosrip ~ poligonoid + anio
  ))

pivotTable100 <- as.data.frame.matrix(
  xtabs(data = resultados_poligonos[resultados_poligonos$buffer == 100, ],
        propbosrip ~ poligonoid + anio
  ))

## Agregar prefijo a columnas
colnames(pivotTable30) <- paste0('i30_', colnames(pivotTable30))
colnames(pivotTable100) <- paste0('i100_', colnames(pivotTable100))

nrow(pivotTable100)
nrow(pivotTable30)

write.csv2(x = pivotTable30 , 
           file = paste0('Compilado2_', 'poligonos' ,'_', nrow(poligonos) ,'pols_', Sys.Date(),  '_buffer30m.csv'), 
           row.names = FALSE)
write.csv2(x = pivotTable100 , 
           file = paste0('Compilado2_', 'poligonos' ,'_', nrow(poligonos) ,'pols_', Sys.Date(), '_buffer100m.csv'), 
           row.names = FALSE)


## Crear campos en tabla de shapefile
poligonos@data[, c(colnames(pivotTable30), colnames(pivotTable100))] <- NA

posicion_merge30 <- match(poligonos$OBJECTID, rownames(pivotTable30))
poligonos@data[which(!is.na(posicion_merge30)), colnames(pivotTable30)] <- pivotTable30[na.omit(posicion_merge30), colnames(pivotTable30)]

posicion_merge100 <- match(poligonos$OBJECTID, rownames(pivotTable100))
poligonos@data[which(!is.na(posicion_merge100)), colnames(pivotTable100)] <- 
  pivotTable100[na.omit(posicion_merge100), colnames(pivotTable100)]*100

head(poligonos)
writeOGR(obj = poligonos, dsn = '.', overwrite_layer = TRUE,
         driver = 'ESRI Shapefile', 
         layer = paste0('Indicador_ripario_', 'poligonosANP_' ,'_', nrow(poligonos) ,'pols_', Sys.Date()))
