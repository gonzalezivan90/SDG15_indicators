## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
root <- 'C:/temp/Ecuador_riparios/04_CalculoNacional/' # Usar / o \\. Cada uno cambia su ruta
setwd( root ) # asignar ruta de trabajo

list.dirs(path = '.', full.names = TRUE, recursive = FALSE)


outDir <- c('07_cortes_cuencas')
dir.create(outDir, recursive = TRUE) # Algunos warnings. No es grave


## Bosques -----
## Listar archivos riparios nacionales finales -----
(archivos_riparios <- list.files(path = '06_cruce-bosques-riparios', 
                                 pattern = 'bosque_ripario[0-9].+01.tif$', 
                                 full.names = TRUE) )

## Zonas riparias -----

archivo_raster_buffer_30 <- paste0('05_rios-raster/', 'buffer30_l.tif')
archivo_raster_buffer_100 <- paste0('05_rios-raster/', 'buffer100_l.tif')


## Cargar cuenca -----
Ruta_cuencas <- '../01_DatosOriginales/Nacional/EC_un_hid_N5_50k_2020_570.shp' # /,,/ es para subir en una carpeta en la ruta
file.exists( Ruta_cuencas ) # Debe ser TRUE

cuencasCompletas <- readOGR(Ruta_cuencas, encoding = 'utf8')
cuencasCompletas@proj4string@projargs

head(cuencasCompletas@data, 10) # Revisar tabla de atributos
tail(cuencasCompletas@data, 10) # Revisar tabla de atributos 

# plot(cuencasCompletas, axes = TRUE, main = 'Cuencas') # Graficar sencillo la capa

length(cuencasCompletas$cod_pfs) # Validar cantidad de objetos
nrow(cuencasCompletas) # Validar cantidad de objetos
length( unique(cuencasCompletas$cod_pfs) ) # Validar número de valores únicos en campo ID

## Crear un ID unico
cuencasCompletas$basid <- 1:nrow(cuencasCompletas)
length(cuencasCompletas$basid) # Validar cantidad de objetos
nrow(cuencasCompletas) # Validar cantidad de objetos
length( unique(cuencasCompletas$basid) ) # Validar número de valores únicos en campo ID
write.dbf(dataframe = cuencasCompletas@data, 
          file = gsub('.shp', '.dbf', Ruta_cuencas))


#cuencasFull <- cuencas[grep(cuencas$dhnom, pattern = 'Guayas', invert = TRUE), ] # Galapagos
cuencasFull <- cuencasCompletas[cuencasCompletas$basid != 220, ] # Galapagos
#plot(cuencasFull, axes = TRUE, main = 'Cuencas sin Galapagos') # Graficar sencillo la capa

#which(cuencasFull$basid == 400)
#cuencas <- cuencasFull[which(cuencasFull$basid == 400),]
cuencas <- cuencasFull
#plot(cuencas, add = TRUE, col = 'red', border = 'blue')


borrar_capas_raster <- TRUE

for (i in (1:nrow(cuencas))){ # } # i = 1
  # i = which(cuencas$basid == 12)
  
  (id_cuenca <- cuencas$basid[i])
  print( paste0(' --- Cuenca ', i, '-', nrow(cuencas),  '  ID: ', id_cuenca))
  
  
  ## Extraer cuenca ----
  cuenca_poligono <- cuencas[i, ]
  # plot(cuenca_poligono, add = TRUE, col = 3)
  # plot(cuenca_poligono, add = FALSE, col = 3)
  cuenca_extent <- c(cuenca_poligono@bbox[c(1, 2, 3, 4)])
  #archivo_cuenca <- paste0(cuenca_dir, '/cuenca_ID', id_cuenca, '.shp')
  #writeOGR(cuenca_poligono, outDir, paste0('cuenca_ID', id_cuenca), driver = 'ESRI Shapefile')
  
  
  for ( l  in 1:length(archivos_riparios)){ # l = 2
    (archivo_ripario <- archivos_riparios[l])
    (anio <- gsub('_01.+|bosq.+m_', '', basename(archivo_ripario)))
    (buffer <- gsub('m_.+|bos.+rio', '', basename(archivo_ripario)))
    
    archivo_corte_cuenca_zona_rip <- paste0(outDir, '/cuenca', 
                                            id_cuenca, '_buff', buffer, '.tif' )
    
    capa_buff_nacional <- ifelse(buffer == 30,
                                   archivo_raster_buffer_30,
                                   archivo_raster_buffer_100)
    
    archivo_corte_cuenca <- paste0(outDir, '/cuenca', 
                                   id_cuenca, '_buff', buffer,'_', anio, '.tif' )
    archivo_cifras_cuenca <- paste0(outDir, '/cuenca', 
                                    id_cuenca, '_buff', buffer,'_', anio, '.csv' )
      
    
    
    if( ! file.exists(archivo_corte_cuenca) & !file.exists(archivo_cifras_cuenca) ){
      
      # Cortar el buffer completo por cuenca
      if(! file.exists(archivo_corte_cuenca_zona_rip) ){
          
        corte_rip_cuenca <- gdalUtilities::gdalwarp(srcfile = capa_buff_nacional,
                                                    dstfile = archivo_corte_cuenca_zona_rip,
                                                    cutline = Ruta_cuencas,
                                                    cwhere = paste('"basid"=', id_cuenca),
                                                    dstnodata = 999,
                                                    crop_to_cutline = TRUE,
                                                    overwrite = TRUE)
      }
      
      if(! file.exists(archivo_corte_cuenca) ){
        corte_bos_rip <- gdalUtilities::gdalwarp(srcfile = archivo_ripario,
                                                 dstfile = archivo_corte_cuenca,
                                                 cutline = Ruta_cuencas,
                                                 cwhere = paste('"basid"=', id_cuenca),
                                                 dstnodata = 999,
                                                 crop_to_cutline = TRUE,
                                                 overwrite = TRUE)
      }
    }
    
    
    if( ! file.exists(archivo_cifras_cuenca) ){
      
      ## Estadistica zona riparia total -----
      estadisticas_cuenca <- capture.output(
        gdalUtilities::gdalinfo(archivo_corte_cuenca_zona_rip, noct = TRUE,
                                stats = TRUE) )
      
      dimensiones_cuenca <- as.numeric(
        strsplit(gsub('[[:alpha:]]| ','', 
                      grep('Size is', estadisticas_cuenca, value = TRUE) ),
                 ",")[[1]])
      
      (celdas_cuenca <- Reduce(f = '*', x = dimensiones_cuenca))
      (promedio_cuenca <- as.numeric(
        gsub(  " | STATISTICS_MEAN=", "",
               grep('STATISTICS_MEAN', 
                    estadisticas_cuenca, value = TRUE) ) ) )
      
      (pixeles_riparias_cuenca <- celdas_cuenca * promedio_cuenca)
      
      ## Estadistica zona riparia con bosque -----
      estadisticas_bosrip <- capture.output(
        gdalUtilities::gdalinfo(archivo_corte_cuenca,  noct = TRUE,
                                stats = TRUE) )
      
      dimensiones_bosrip <- as.numeric(
        strsplit(gsub('[[:alpha:]]| ','', 
                      grep('Size is', estadisticas_bosrip, value = TRUE) ),
                 ",")[[1]])
      
      (celdas_bosrip <- Reduce(f = '*', x = dimensiones_bosrip))
      # (promedio_bosrip <- as.numeric(
      #   gsub(  "[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
      #          grep('STATISTICS_MEAN=', 
      #               estadisticas_bosrip, value = TRUE)) ) )
      
      (promedio_bosrip <- as.numeric(
        gsub(  " | STATISTICS_MEAN=", "",
               grep('STATISTICS_MEAN', 
                    estadisticas_bosrip, value = TRUE) ) ) )
      
      (pixeles_bosrip <- celdas_bosrip * promedio_bosrip)
      
      
      tabla_cuenca <- data.frame(cuencaid = id_cuenca,
                                 anio = anio,
                                 buffer = buffer,
                                 totpixrip = pixeles_riparias_cuenca,
                                 totpixbos = pixeles_bosrip,
                                 propbosrip = pixeles_bosrip/pixeles_riparias_cuenca
      )
      write.csv(x = tabla_cuenca, file = archivo_cifras_cuenca)
    }
    
    if (borrar_capas_raster){
      file.remove(archivo_corte_cuenca,
                  paste0(archivo_corte_cuenca, '.aux.xml'))
    }
  }
  
  buffers_raster <-  paste0(outDir, '/cuenca', id_cuenca, '_buff', c(30, 100), '.tif' )
  
  ## Borar corte raster de zona riparia en cada por cuenca
  if (borrar_capas_raster){
    file.remove(c(buffers_raster,
                paste0(buffers_raster, '.aux.xml')))
  }
}





#### Comppilar resultados ------

archivos_cifras <- list.files(pattern = 'cuenca.+[0-9].csv$',
                              path = outDir, 
                              full.names = TRUE)
resultados_cuencas <- NULL
for (i in 1:length(archivos_cifras)){ # i = 1
  tabla <- read.csv(archivos_cifras[i])
  colnames(tabla)[colnames(tabla) == 'cuenca'] <- 'cuencaid'
  resultados_cuencas <- rbind(resultados_cuencas, tabla)
}

head(resultados_cuencas)
head(archivos_cifras)

## Borrar archivos incorrectos ---
filas_incorr <- resultados_cuencas[which(resultados_cuencas$propbosrip > 1),]
nrow(filas_incorr)

if( nrow(filas_incorr) != 0){
  filas_incorr$tifs_borrar <- paste0(outDir, '/cuenca', filas_incorr$cuenca, '_buff', filas_incorr$buffer, 
                                     '_', filas_incorr$anio, '.tif')
  filas_incorr$csv_borrar <- paste0(outDir, '/cuenca', filas_incorr$cuenca, '_buff', filas_incorr$buffer, 
                                    '_', filas_incorr$anio, '.csv')
  
  
  sapply(filas_incorr$tifs_borrar, file.remove)
  sapply(filas_incorr$csv_borrar, file.remove)
}

print(getwd())
write.csv(x = resultados_cuencas , 
          file = paste0('Compilado_indicadores_cuencas_', Sys.Date(), '.csv'), 
          row.names = FALSE)
write.csv2(x = resultados_cuencas , 
          file = paste0('Compilado2_indicadores_cuencas_', Sys.Date(), '.csv'), 
          row.names = FALSE)


pivotTable30 <- as.data.frame.matrix(
  xtabs(data = resultados_cuencas[resultados_cuencas$buffer == 30, ],
        propbosrip ~ cuencaid + anio
  ))

pivotTable100 <- as.data.frame.matrix(
  xtabs(data = resultados_cuencas[resultados_cuencas$buffer == 100, ],
        propbosrip ~ cuencaid + anio
  ))

colnames(pivotTable30) <- paste0('i30_', colnames(pivotTable30))
colnames(pivotTable100) <- paste0('i100_', colnames(pivotTable100))

nrow(pivotTable100)
nrow(pivotTable30)

head(pivotTable30)
head(pivotTable100)

write.csv(x = pivotTable30 , 
          file = paste0('Compilado_indicadores_cuencas_', Sys.Date(), '_buffer30m.csv'), 
          row.names = FALSE)
write.csv2(x = pivotTable100 , 
           file = paste0('Compilado2_indicadores_cuencas_', Sys.Date(), '_buffer100m.csv'), 
           row.names = FALSE)


## Crear campos en tabla de shapefile
cuencas@data[, c(colnames(pivotTable30), colnames(pivotTable100))] <- NA

posicion_merge30 <- match(cuencas$basid, rownames(pivotTable30))
cuencas@data[which(!is.na(posicion_merge30)), colnames(pivotTable30)] <- pivotTable30[na.omit(posicion_merge30), colnames(pivotTable30)]

posicion_merge100 <- match(cuencas$basid, rownames(pivotTable100))
cuencas@data[which(!is.na(posicion_merge100)), colnames(pivotTable100)] <- 
  pivotTable100[na.omit(posicion_merge100), colnames(pivotTable100)]

head(cuencas)
writeOGR(obj = cuencas, dsn = '.', 
         driver = 'ESRI Shapefile', 
         layer = paste0('Indicador_ripario_cuencas_', Sys.Date()), overwrite_layer = TRUE)
