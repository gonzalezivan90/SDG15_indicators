
library(devtools)
#devtools::source_url("https://github.com/gonzalezivan90/test_inf599/blob/master/fun_raster_count.R")

### 0. Directorio de trabajo 
root <- 'C:/temp/Ecuador_fragmentacion/'
setwd(root)


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )


## Cargar funcion para calcular histogramas de rasters
check <- source('I:/Mi unidad/Datos-NASA-ODS15/Talleres/Ecuador_fragmentacion/08_scripts-R/R_03_tabuleRaster.R')

# ## Para ejecutar en R dentro de Linux
# root = '/mnt/c/temp/Ecuador_fragmentacion/03_bosques-reclasificados'
# setwd(root)
# list.files()
# tifs <- list.files(pattern = '.tif$')
# for (i in 1:length(tifs)){
#   inname = tifs[i]
#   (anio <- (gsub('[a-zA-Z]|[[:punct:]]', '', inname)))
#   (outname = paste0('FRAGMENTACION_', anio, '.tif'))
#   if(!file.exists(outname) & file.exists(inname)){
#     #cmd = paste0(root, 'mspa_lin64 -i ', root, inname,' -o ', root, outname)
#     cmd = paste0('time ./mspa_lin64 -i ', inname,' -o ', outname, '\n')
#     cat(cmd)
#     #system(cmd) # 
#   }
# }
## Copiar los comandos anteriores y pegarlos en la consola de lunix en la carpeta donde se encuentran los 
## bosques reclasificados


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )


## Renombrar --- correr con cuidado
(tifs <- list.files(path = '04_resultados-fragmentacion', 
                    pattern = '_[0-9]_[0-9].+tif$', full.names = TRUE))

## Con cuidado. Esto cambiará nombre de archivos así: 
# "04_resultados-fragmentacion/frag_2000_8_1_1_1.tif"  a "04_resultados-fragmentacion/frag_2000.tif"
sapply(tifs, function(x){
  # x = tifs[1] # Prueba
  # x = "04_resultados-fragmentacion/frag_2000_8_1_1_1.tif" # Prueba
  (new_name <- basename(tools::file_path_sans_ext(x)))
  (anio <- gsub('_[0-9]_.+|[a-zA-Z]|_', '', new_name))
  file.rename(from = x, to = paste0(dirname(x), '/FRAGMENTACION_', new_name, '.tif'))
})



## Consolidar conteos de tipos de pixeles
## Renombrar --- correr con cuidado
(tifs <- list.files(path = '04_resultados-fragmentacion', #pueden estar en"05_bosques-reclasificados"
                    pattern = 'FRAGMENTACION_[0-9].+.tif$', full.names = TRUE))

# Lista vacía
ans <- list()

## iterar y extraer conteo de pixeles por cada capa
for (f in 1:length(tifs)){ # f = 1
  rc <- tabuleRaster(tifs[f], n256 = TRUE, del0 = TRUE) # Calcular histogramas
  ss <- subset(rc, count != 0)
  (yy <- gsub(x = basename(tifs[f]), '[a-zA-Z]|[[:punct:]]', ''))
  ss[, c(paste0('cnt'), 'yy')] <- cbind(ss$count, yy)
  ans[[f]] <- ss
}

ansdf <- do.call(rbind, ans)


## Obtener el 100% de pixeles del pais
raster_nacional <- '01_datos-originales/Ecuador_raster_30m.tif'
vector_nacional <- '01_datos-originales/v_ff010_cobertura_vegetal_1990_aPolygon.shp'

## Extraer extent de la capa y rasterizar posteriormente
(info_bosques <- gdalUtils::ogrinfo(vector_nacional, so = TRUE, al = TRUE))
(extent_bosques <- grep('Extent', info_bosques, value = TRUE))
(extent_sin_texto <- gsub('[a-zA-Z]|\\)|\\(|-|:|: |,|^ ', '', extent_bosques) )
(extent_numerico <- as.numeric(strsplit( gsub('  ', ' ', 
                                              extent_sin_texto), ' ')[[1]] ))

## Por si hay error en el paso anterior
# info_bosques <- rgdal::ogrInfo(vector_nacional)
# extent_numerico <- info_bosques$extent


if(! file.exists(raster_nacional)){
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = vector_nacional, 
                                dst_filename = raster_nacional, 
                                ot = 'Byte',
                                burn = 1, tr = c(30, 30), init = 0,
                                # where = # Todo el pais, no se usa where
                                te =  extent_numerico,
                                co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 323.33  seg
}

ecuador_100porc_pixeles <- tabuleRaster(raster_nacional, n256 = TRUE, del0 = TRUE) # Calcular histogramas



## Total pixeles --- toca corregir porque recuerden que es un recorte del Peru
## Debemos contabilizar el total de la superficie
PixEc <- ecuador_100porc_pixeles$count[ecuador_100porc_pixeles$id == 1]

## Pixeles con bosques
PixBos <- with(data = subset(ansdf, id != 0 ), 
           tapply(count, yy, sum))

## Pixeles con bosque tipo nucleo (Core)
PixBosNucleo <- with(data = subset(ansdf, id %in% c(1, 17)), 
           tapply(count, yy, sum))




## Crear tabla compilando
tabla_final <- cbind.data.frame(PixEc, PixBos, PixBosNucleo)
tabla_final$anio <- as.numeric(rownames(tabla_final))

## Proporcion nacional de bosques respecto a la superficie total de Ecuador
tabla_final$propBosque <- tabla_final$PixBos/tabla_final$PixEc

## Proporcion nacional de bosques nucleo respecto a la superficie total de Ecuador
tabla_final$propBosqueNucleo <- tabla_final$PixBosNucleo/tabla_final$PixEc

## Proporcion nacional de bosques nucleo respecto a la superficie en bosques
tabla_final$propBosqueNucleo <- tabla_final$PixBosNucleo/tabla_final$PixBos

## Ver tabla
tabla_final

## Escribir tabla
write.csv(tabla_final, 'Tabla_resultados_fragmentacion.csv')
write.csv2(tabla_final, 'Tabla_resultados_fragmentacion2.csv')



#########

## Evaluar si tenemos acceso a gdal_calc ----
## Validar si tenemos la libreria gdal_calc
execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat
                   'py3_env.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
                   '&& py3_env.bat ', # Mantener espacio. Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
                   '&& gdal_calc ') # Llamado de gdal_calc

## Instrucción en QGIS 3.22.6
## execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && gdal_calc'))

## Mirar la respuesta del sistema a conexion a gdal_calc
(gdalcalc <- capture.output(system(paste0(execGDAL, '--help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
(GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))


(uVals <- unique(unlist(lapply(ans, function(x) unique(x[, 1])))))
#(1000 + (100 * uVals))
(1000 * (1 + uVals)) + sample(uVals)



(resultados <- list.files(path = '04_resultados-fragmentacion', full.names = TRUE,
                          pattern = 'FRAGMENTACION_.+.tif$'))

# Lista vacía
ansComp <- list()

for (i in 1:(length(resultados)-1)){ # i = 1
  
  (raster_a <- resultados[i])
  (raster_b <- resultados[i+1])
  (anio_a <- gsub(x = basename(raster_a), '[a-zA-Z]|[[:punct:]]', ''))
  (anio_b <- gsub(x = basename(raster_b), '[a-zA-Z]|[[:punct:]]', ''))
  
  (outFile <- paste0('05_transiciones/comparacion_', anio_a, '-', anio_b, '.tif'))
  
  if( file.exists(raster_a) & file.exists(raster_b) & !file.exists(outFile) ){
    
    ##
    
    if (! GDAL){
      ## Hecho en R
      raster_j <- (1000 * (1 + raster::raster(raster_a))) + raster::raster(raster_b)
      writeRaster(x = raster_j, filename = outFile,
                  datatype = 'INT4U', options=c("COMPRESS=DEFLATE"))
      
    } else {
      ## Heho con gdal_calc.pys
      
      (cmd <- paste0(execGDAL,
                     '--calc="(1000*(1+A))+B', '" -A ', root2, raster_a, ' -B ', root2, raster_b,
                     ' --outfile=', root2, outFile,
                     ' --quiet --co="COMPRESS=DEFLATE" --type=UInt32'
                     #' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                     # INT2U	0			65,534					UInt16	
      ))
      print(system.time( system(intern = TRUE, cmd))) # 252
    }
    
    
    print ( paste('   -- ', outFile ) )
    
  }
  
  (csvtable <- paste0('05_transiciones/comparacion_', anio_a, '-', anio_b, '.csv'))
  
  if( file.exists(outFile) & !file.exists(csvtable) ){
    rc <- tabuleRaster(outFile, n256 = TRUE, del0 = TRUE)
    (yy <- paste0(anio_a, '-', anio_b))
    rc[, c(paste0('cnt', yy), 'yy')] <- cbind(rc$count, yy)
    write.csv(rc, file = csvtable)
    ansComp[[f]] <- ss
    
  }
  print(csvtable)
}
