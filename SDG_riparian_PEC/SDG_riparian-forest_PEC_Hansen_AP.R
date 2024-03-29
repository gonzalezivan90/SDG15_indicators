## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
root <- 'C:/temp/SDG_riparian_PEC/' # Usar / o \\. Cada uno cambia su ruta
setwd( root ) # asignar ruta de trabajo

# Asegurarnos que estamos en la carpeta de trabajo
list.dirs(path = '.', full.names = TRUE, recursive = FALSE)

## Crear carpeta con resultados de poligonos
outDir <- c('07_poligonos_AP')
dir.create(outDir, recursive = TRUE) # Algunos warnings. No es grave


## Bosques -----
## Listar archivos TIF riparios nacionales finales -----
(archivos_riparios <- list.files(path = '06_cruce-bosques-riparios', 
                                 pattern = 'bosque_ripario[0-9].+.tif$', 
                                 full.names = TRUE) )

## Zonas riparias -----

archivo_raster_buffer_30 <- paste0('03_rios-raster/', 'buffer30.tif')
archivo_raster_buffer_100 <- paste0('03_rios-raster/', 'buffer100.tif')


## Cargar cuenca -----
## Ajustar ruta total o relativa
ruta_poligonos <- '01_datos-originales/'
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
if( length( unique(poligonosCompletos$OBJECTID) ) == 0){
  poligonosCompletos$OBJECTID <- 1:nrow(poligonosCompletos) # creamos un ID consecutivo
  ## Escribir la tabla con el ID
  write.dbf(poligonosCompletos@data, file = '../08_SERNANP/anp_proj.dbf')
}

## Iteramos sobre todas las poligonos
poligonos <- poligonosCompletos
# plot(poligonos, add = TRUE)


borrar_capas_raster <- TRUE

for (i in (1:nrow(poligonos))){ # } # i = 95
  # i = which(poligonos$OBJECTID == 17)
  
  (id_cuenca <- poligonos$OBJECTID[i])
  print( paste0(' --- Cuenca ', i, '-', nrow(poligonos),  '  ID: ', id_cuenca))
  
  
  ## Extraer cuenca ----
  cuenca_poligono <- poligonos[i, ]
  # plot(cuenca_poligono, add = TRUE, col = 'red', border = 'blue')
  
  cuenca_extent <- c(cuenca_poligono@bbox[c(1, 2, 3, 4)])
  
  
  ## Iterar para cada cuenca con las capas de bosque disponible
  for ( l  in 1:length(archivos_riparios)){ # l = 1
    (archivo_ripario <- archivos_riparios[l])
    (anio <- gsub('.tif|bosq.+m_', '', basename(archivo_ripario)))
    (buffer <- gsub('m_.+|bos.+rio', '', basename(archivo_ripario)))
    
    ## Archivo de salida
    archivo_corte_cuenca_zona_rip <- paste0(outDir, '/cuenca', 
                                            id_cuenca, '_buff', buffer, '.tif' )
    
    ## Seleccionar buffer a cortar dependiendo del buffer del bosque
    capa_buff_nacional <- ifelse(buffer == 30,
                                 archivo_raster_buffer_30,
                                 archivo_raster_buffer_100)
    
    archivo_corte_cuenca <- paste0(outDir, '/cuenca', 
                                   id_cuenca, '_buff', buffer,'_', anio, '.tif' )
    archivo_cifras_cuenca <- paste0(outDir, '/cuenca', 
                                    id_cuenca, '_buff', buffer,'_', anio, '.csv' )
    
    
    ## Ejecutar si no existen las capas o archivos de estadisticas
    if( ! file.exists(archivo_corte_cuenca) & !file.exists(archivo_cifras_cuenca) ){
      
      # Cortar el buffer completo por cuenca
      if(! file.exists(archivo_corte_cuenca_zona_rip) ){
        
        corte_rip_cuenca <-  tryCatch(
          gdalUtilities::gdalwarp(srcfile = capa_buff_nacional,
                                  dstfile = archivo_corte_cuenca_zona_rip,
                                  cutline = ruta_poligonos,
                                  cwhere = paste('"OBJECTID"=', id_cuenca),
                                  #srcnodata = 0,
                                  srcnodata = 'None',
                                  #dstnodata = 999,
                                  crop_to_cutline = TRUE,
                                  nomd = TRUE,
                                  overwrite = TRUE,
                                  co = c("NBITS=1", "COMPRESS=DEFLATE")),
          error = function(e) NULL)
        # rt = raster(corte_rip_cuenca); plot(rt, axes = TRUE); summary(rt)
        
        if( is.null ( corte_rip_cuenca ) ){
          print( paste0(' Sin datos para la cuenca con ID OBJECTID:', id_cuenca, 
                        ' - capa:', archivo_ripario))
          next()
        }
      }
      
      
      # Cortar el buffer con bosque por cuenca
      if(! file.exists(archivo_corte_cuenca) ){
        corte_bos_rip <- tryCatch(
          gdalUtilities::gdalwarp(srcfile = archivo_ripario,
                                  dstfile = archivo_corte_cuenca,
                                  cutline = ruta_poligonos,
                                  cwhere = paste('"OBJECTID"=', id_cuenca),
                                  #dstnodata = 999,
                                  srcnodata = 'None',
                                  nomd = TRUE,
                                  crop_to_cutline = TRUE,
                                  co = c("NBITS=1", "COMPRESS=DEFLATE"),
                                  overwrite = TRUE),
          error = function(e) NULL)
        # rt2 = raster(archivo_corte_cuenca, legend = TRUE, main =  Sys.time()); plot(rt2, axes = TRUE); summary(rt2)
        # (estadisticas_bosrip <- capture.output(gdalUtilities::gdalinfo(archivo_corte_cuenca, stats = TRUE, hist = TRUE) ))
        
        if( is.null ( corte_bos_rip )){
          print( paste0(' Sin datos para la cuenca con ID OBJECTID:', id_cuenca, 
                        ' - capa:', archivo_ripario))
          next()
        }
      }
    }
    
    
    
    
    
    ## Calcular estadisticas si no hay archivo escrito
    if( ! file.exists(archivo_cifras_cuenca) ){
      
      ## Estadistica zona riparia total -----
      estadisticas_cuenca <- capture.output(
        gdalUtilities::gdalinfo(archivo_corte_cuenca_zona_rip, 
                                stats = TRUE, hist = TRUE) )
      
      dimensiones_cuenca <- as.numeric(
        strsplit(gsub('[[:alpha:]]| ','', 
                      grep('Size is', estadisticas_cuenca, value = TRUE) ),
                 ",")[[1]])
      
      celdas_cuenca <- Reduce(f = '*', x = dimensiones_cuenca)
      (promedio_cuenca <- as.numeric(
        gsub(  " | STATISTICS_MEAN=", "",
               grep('STATISTICS_MEAN', 
                    estadisticas_cuenca, value = TRUE)) ) )
      
      (pixeles_riparias_cuenca <- celdas_cuenca * promedio_cuenca)
      
      
      ## Estadistica zona riparia con bosque ------------
      
      estadisticas_bosrip <- capture.output(
        gdalUtilities::gdalinfo(archivo_corte_cuenca, 
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
      
      
      tabla_cuenca <- data.frame(cuencaid = id_cuenca,
                                 anio = anio,
                                 buffer = buffer,
                                 totpixrip = pixeles_riparias_cuenca,
                                 totpixbos = pixeles_bosrip,
                                 propbosrip = pixeles_bosrip/pixeles_riparias_cuenca
      )
      write.csv2(x = tabla_cuenca, file = archivo_cifras_cuenca, row.names = FALSE)
      # write.csv(x = tabla_cuenca, file = archivo_cifras_cuenca, row.names = FALSE)
    }
    
    
    ## Borar corte raster para la cuenca en cada anio particular
    if (borrar_capas_raster){
      file.remove(archivo_corte_cuenca,
                  paste0(archivo_corte_cuenca, '.aux.xml'))
    }
    
  }
  
  buffers_raster <-  paste0(outDir, '/cuenca', id_cuenca, '_buff', c(30, 100), '.tif' )
  
  ## Borar corte raster de zona riparia en cada por cuenca
  if (borrar_capas_raster){
    file.remove(buffers_raster,
                paste0(buffers_raster, '.aux.xml'))
  }
}



#### Compilar resultados ------

archivos_cifras <- list.files(pattern = 'cuenca.+[0-9].csv$',
                              path = outDir, 
                              full.names = TRUE)
resultados_poligonos <- NULL
system.time({ 
  for (i in 1:length(archivos_cifras)){ # i = 1
    tabla <- read.csv2(archivos_cifras[i])
    colnames(tabla)[colnames(tabla) == 'cuenca'] <- 'cuencaid'
    resultados_poligonos <- rbind(resultados_poligonos, tabla)
  }
})


head(resultados_poligonos)
head(archivos_cifras)

## Borrar archivos incorrectos ---
(filas_incorr <- resultados_poligonos[which(resultados_poligonos$propbosrip > 1),])
nrow(filas_incorr) # Debe ser 0

if(nrow(filas_incorr) != 0){
  filas_incorr$tifs_borrar <- paste0(outDir, '/cuenca', filas_incorr$cuenca, '_buff', filas_incorr$buffer, 
                                     '_', filas_incorr$anio, '.tif')
  filas_incorr$csv_borrar <- paste0(outDir, '/cuenca', filas_incorr$cuenca, '_buff', filas_incorr$buffer, 
                                    '_', filas_incorr$anio, '.csv')
  
  sapply(filas_incorr$tifs_borrar, file.remove)
  sapply(filas_incorr$csv_borrar, file.remove)
}

print(getwd())
write.csv(x = resultados_poligonos , 
          file = paste0('Compilado_indicadores_', 'poligonos' ,'_', nrow(poligonos) ,'pols_', Sys.Date(), '.csv'), 
          row.names = FALSE)
write.csv2(x = resultados_poligonos , 
           file = paste0('Compilado2_indicadores_ANP_359pols_', Sys.Date(), '.csv'), 
           row.names = FALSE)


## 
pivotTable30 <- as.data.frame.matrix(
  xtabs(data = resultados_poligonos[resultados_poligonos$buffer == 30, ],
        propbosrip ~ cuencaid + anio
  ))

pivotTable100 <- as.data.frame.matrix(
  xtabs(data = resultados_poligonos[resultados_poligonos$buffer == 100, ],
        propbosrip ~ cuencaid + anio
  ))

## Agregar prefijo a columnas
colnames(pivotTable30) <- paste0('i30_', colnames(pivotTable30))
colnames(pivotTable100) <- paste0('i100_', colnames(pivotTable100))

nrow(pivotTable100)
nrow(pivotTable30)

write.csv2(x = pivotTable30 , 
           file = paste0('Compilado2_indicadores_ANP_359pols_', Sys.Date(), '_buffer30m.csv'), 
           row.names = FALSE)
write.csv2(x = pivotTable100 , 
           file = paste0('Compilado2_indicadores_ANP_359pols_', Sys.Date(), '_buffer100m.csv'), 
           row.names = FALSE)


## Crear campos en tabla de shapefile
poligonos@data[, c(colnames(pivotTable30), colnames(pivotTable100))] <- NA

posicion_merge30 <- match(poligonos$OBJECTID, rownames(pivotTable30))
poligonos@data[which(!is.na(posicion_merge30)), colnames(pivotTable30)] <- pivotTable30[na.omit(posicion_merge30), colnames(pivotTable30)]

posicion_merge100 <- match(poligonos$OBJECTID, rownames(pivotTable100))
poligonos@data[which(!is.na(posicion_merge100)), colnames(pivotTable100)] <- 
  pivotTable100[na.omit(posicion_merge100), colnames(pivotTable100)]

head(poligonos)
writeOGR(obj = poligonos, dsn = '.', overwrite_layer = TRUE,
         driver = 'ESRI Shapefile', 
         layer = paste0('Indicador_ripario_ANP_359pols_', Sys.Date()))
