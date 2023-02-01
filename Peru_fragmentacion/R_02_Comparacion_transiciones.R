
library(devtools)
#devtools::source_url("https://github.com/gonzalezivan90/test_inf599/blob/master/fun_raster_count.R")
### 0. Directorio de trabajo 
root <- 'C:/temp/Peru_fragmentacion/'
check <- source('I:/Mi unidad/Datos-NASA-ODS15/Talleres/Peru_fragmentacion/09_scripts/R_03_tabuleRaster.R')

# ## Para ejecutar en R dentro de Linux
# root = '/mnt/c/temp/Peru_fragmentacion/05_bosques-reclasificados'
# setwd(root)
# list.files()
# for (i in 2000:2022){
#   inname = paste0('recl12_bosqueFecha_', i, '.tif')
#   outname = paste0('fragmentacion_', i, '.tif')
#   if(!file.exists(outname) & file.exists(inname)){
#     #cmd = paste0(root, 'mspa_lin64 -i ', root, inname,' -o ', root, outname)
#     cmd = paste0('time ./mspa_lin64 -i ', inname,' -o ', outname, '\n')
#     cat(cmd)
#     #system(cmd)
#   }
# }


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )


## Renombrar --- correr con cuidado
(tifs <- list.files(path = '06_resultados-fragmentacion', #pueden estar en"05_bosques-reclasificados"
                    pattern = '_[0-9]_[0-9].+tif$', full.names = TRUE))

## Con cuidado. Esto cambiará nombre de archivos así: 
# "06_resultados-fragmentacion/frag_2000_8_1_1_1.tif"  a "06_resultados-fragmentacion/frag_2000.tif"
sapply(tifs, function(x){
  # x = tifs[1] # Prueba
  (new_name <- basename(tools::file_path_sans_ext(x)))
  (new_name <- gsub('_[0-9]_.+', '', new_name))
  file.rename(from = x, to = paste0(dirname(x), '/', new_name, '.tif'))
})


## Consolidar conteos de tipos de pixeles
## Renombrar --- correr con cuidado
(tifs <- list.files(path = '06_resultados-fragmentacion', #pueden estar en"05_bosques-reclasificados"
                    pattern = 'fragmentacion_[0-9].+.tif$', full.names = TRUE))

# Lista vacía
ans <- list()

## iterar y extraer conteo de pixeles por cada capa
for (f in 1:length(tifs)){ # f = 1
  rc <- tabuleRaster(tifs[f], n256 = TRUE, del0 = TRUE)
  ss <- subset(rc, count != 0)
  (yy <- gsub(x = basename(tifs[f]), '[a-z]|[[:punct:]]', ''))
  ss[, c(paste0('cnt'), 'yy')] <- cbind(ss$count, yy)
  ans[[f]] <- ss
}

ansdf <- do.call(rbind, ans)

## Total pixeles --- toca corregir porque recuerden que es un recorte del Peru
## Debemos contabilizar el total de la superficie
smx <- with(data = subset(ansdf, id != 99999 ), 
            tapply(count, yy, sum))

## Pixeles con bosques
sm <- with(data = subset(ansdf, id != 0 ), 
           tapply(count, yy, sum))

## Pixeles con bosque tipo nucleo (Core)
co <- with(data = subset(ansdf, id %in% c(1, 17)), 
           tapply(count, yy, sum))




## Crear tabla compilando
cb <- cbind.data.frame(smx, sm, co)
cb$propForest <- cb$co/cb$sm 
cb$proArea <- cb$co/cb$smx 
cb$yy <- as.numeric(rownames(cb))

## Ver tabla
cb

## Escribir tabla
write.csv(cb, 'stats_frag.csv')
write.csv2(cb, 'stats_frag2.csv')



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

           
setwd(root)

ans2 <- list()
for (i in 2000:2022){ # i = 2000
  
  (ra <- paste0('fragmentacion_', i, '.tif'))
  (rb <- paste0('fragmentacion_', i+1, '.tif'))
  (outFile <- paste0('comparacion_', i, '-', i+1, '.tif'))
  
  if( file.exists(ra) & file.exists(rb) & !file.exists(outFile) ){
    
    ##
    
    if (! GDAL){
      ## Hecho en R
      raster_j <- (1000 * (1 + raster::raster(ra))) + raster::raster(rb)
      writeRaster(x = raster_j, filename = outFile,
                  datatype = 'INT4U', options=c("COMPRESS=DEFLATE"))
      
    } else {
      ## Heho con gdal_calc.pys
      
      
      (cmd <- paste0(execGDAL,
                     '--calc="(1000*(1+A))+B', '" -A ', root2, ra, ' -B ', root2, rb,
                     ' --outfile=', root2, outFile,
                     ' --quiet --co="COMPRESS=DEFLATE" --type=UInt32'
                     #' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                     # INT2U	0			65,534					UInt16	
      ) 
      )
      print(system.time( system(intern = TRUE, cmd))) # 248
    }
    
    
    print ( paste('   -- ', outFile ) )
    
  }
  
  (csvtable <- paste0('comparacion_', i, '-', i+1, '.csv'))
  
  if( file.exists(outFile) & !file.exists(csvtable) ){
    rc <- raster_count(raster::raster(outFile), n256 = FALSE)
    ss <- subset(rc, count != 0)
    (yy <- gsub(x = basename(tifs[f]), '[a-z]|[[:punct:]]', ''))
    ss[, c(paste0('cnt', yy), 'yy')] <- cbind(ss$count, yy)
    write.csv(ss, file = csvtable)
  }
  print(i)
}
