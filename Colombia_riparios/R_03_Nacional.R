## Creado por Ivan Gonzalez [gonzalezgarzonivan@gmail.com]
## Estudiante doctorado en Ecoinformática @ NAU - ig299@nau.edu


## AVISOS: ----
## 1. Reabrir este archivo con codificación UTF8 (File/Archivo - Reopen with Encoding/Reabrir con codificación -- UTF8)
## 2. Todas las líneas de código deben ejecutarse.
## 3. Los análisis pesados no se repiten porque el código detecta que ya existen esos archivos
## 4. Comentarios del texto inicia con dos ##
## 5. Código inhabilitado inicia con un #. Puede removerse con Ctrl+C
## 6. Abrir en QGIS o similar los resultados después de cada paso y así verificar el avance del script
## 7. Los archivos se pueden descargar de esta carpeta: XXXXXXXXXXXXX
## 8. El algoritmo puede tomar hasta 6H en ejecutarse por el tamaño de los archivos
## 9. Se debe crear una carpeta de trabajo. Ojalá cerca de la raíz y sin espacios. Ej: C:/Colombia_riparios/
## 10. Las líneas de código que se deben cambiar tienen este texto al final "#CAMBIAR ---"
## 11. Los números al final de algunas funciones indican el tiempo aproximado en segundos que toma la operación

## Pasos de este script:
## 1. Liberías, definir ruta de trabajo + carpetas + archivos
## 2. Localizar gdal_calc.py que hace los análisis más rápidos, aunque no es obligatorio
## 3. Crear buffer de ríos en zonas riparias + eliminar superficies de agua
## 4. Rasterizar capas de zonas riparias
## 5. Extraer limite forestal - recortar el raster nacional - capa vectorial
## 6. Derivar capas anuales de bosque año a año
## 7. Cruzar bosques anuales con zonas riparias año a año
## 8. Extraer estadísticas y guardarlas en CSV año a año
## 9. Compilar resultados y generación de tabla final


## 1. Liberías, definir ruta de trabajo + carpetas + archivos ----------------
## Cargar librerías
library(raster) # Raster
library(foreign) # Cargar las tablas .dbf de shapefiles
library(rgdal)  # Funciones de GDAL / OGR para R
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(gdalUtils) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
## 'C:/temp/Colombia_riparios' Es la carpeta del proyecto. Esta puede SI PUEDE en cada computador
carpeta_trabajo <- 'C:/temp/Colombia_riparios/' ####### CAMBIAR ---
dir.create(paste0(carpeta_trabajo, '01_datos-originales'), recursive = TRUE) ## Crear la carpeta si no existe
file.exists(paste0(carpeta_trabajo, '01_datos-originales')) ## Debe ser TRUE 

## '04_calculoNacional' es la carpeta de calculo nacional. Esta ruta NO PUEDE cambiar y debe mantenerse igual
root <- paste0(carpeta_trabajo) # Usar / o \\. Cada uno cambia su carpeta_trabajo
dir.create(root) ## Puede generar un warning si ya exsiste. Es sólo un aviso
setwd( root ) # asignar ruta de trabajo
list.dirs(recursive = FALSE) # Revisar carpetas existentes


## Acá se deben ubicar los archivos originales descomprimidos:
paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales')

## Lo siguiente debe ser TRUE y no FALSE, Se revisa que existan las capas necesarias. NO puede ser FALSE.
(condicion <- all( c('Bosque_No_Bosque_1990.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2000.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2005.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2010.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2012.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2013.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2014.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2015.zip', # Archivos de bosque en tif
                     'Bosque_No_Bosque_2016.zip', # Archivos de bosque en tif
                     'CO_rivers.7z' # Archivos de capas vectorial de rios 
) %in% list.files(paste0(carpeta_trabajo, '/01_datos-originales'))))


## Evaluar si tenemos las condiciones para correr el script
if( !condicion){
  stop(paste0('ERROR: Copie los archivos mencionados a la carpeta: ---',
              paste0(carpeta_trabajo, '/01_datos-originales')
              , '---'))
}

## Descomprimimos los archivos anteriores con la opcion "descomprimir aca"
## Posteriormente traemos las capas TIF de bosque en esas subcarpetas a la carpeta de los 
## datos originales. Pasaremos de este archivo:
##  - 01_datos-originales//Bosque_No_Bosque_1990/Geotiff/SBQ_SMBYC_BQNBQ_V5_1990.tif a este:
##  - 01_datos-originales/BOSQUE_1990.tif, y asi con los demas archivos

(tifs_bosques_originales <- list.files(path = '01_datos-originales/', pattern = '.tif$|TIF$', 
                                       recursive = TRUE, full.names = TRUE))
sapply(tifs_bosques_originales, function(x) {
  newname <- gsub('^[a-zA-Z].+[0-9]_', 'BOSQUE_', basename(x))
  file.rename(x, paste0('01_datos-originales/', newname))
})


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
root2 <- paste0(paste0(root2, collapse = '/'), '/')


## Crear carpetas intermedias
outDirs <- c('02_rios-buffer', '03_rios-raster', 
             '04_bosques-filtrados', '05_bosques-raster', 
             '06_cruce-bosques-riparios')
sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave


## 2. Localizar gdal_calc.py que hace los análisis más rápidos, aunque no es obligatorio ----------------
## Evaluar acceso a gdal_calc ----

## Esto se hace para que el análisis a generar sea bastante más rápido, aunque no es necesario. Es opcional
## Necesitamos ubicar algunos archivos que generalmente trae QGIS. 
## En caso de no poder generar TRUE en el objeto "GDAL", no importa. No se usa gdal_calc de forma externa sino que se unsa la versión interna de R,
## que es mas lenta pero nos produce los mismo resultados

##Aca la configuración para QGIS3.14, pero abajo hay instrucciones para otras versiones. #CAMBIAR ---
execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat ', # Mantener espacio al final. Ubicación del archivo OSGeo4W.bat que es el orquestador de ejecutables GIS00
                   'py3_env.bat ', # Mantener espacio. Ubicación de el ejecutable que configura entorno de python. Puede llamarse tambien o4w_env.bat
                   '&& py3_env.bat ', # Mantener espacio al . Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
                   '&& gdal_calc ') # Llamado de gdal_calc, Dejar espacio al final

# # Instrucción en otras versiones QGIS. Se pueden habilitar las siguientes versiones. #CAMBIAR ---
# # QGIS version 3.16
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat py3_env.bat && py3_env.bat && gdal_calc '))
# # QGIS version 3.20
# (execGDAL <- paste0('C:\"Program Files"\"QGIS 3.20.1"\\OSGeo4W.bat C:\"Program Files"\\"QGIS 3.3.20.1"\\bin\\o4w_env.bat && C:\\"Program Files"\\"QGIS 3.3.20.1"\\bin\\o4w_env.bat && gdal_calc '))
# # QGIS version 3.22
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && gdal_calc '))
# # QGIS version 3.22.12
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.12"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.12"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.12"\\bin\\o4w_env.bat && gdal_calc '))
# # QGIS version 3.28
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.28.1"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.28.1"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.28.1"\\bin\\o4w_env.bat && gdal_calc '))
# # Para quienes hayan instalado QGIS directamente en la raiz de algun disco duro
# (execGDAL <- paste0('E:\\OSGeo4W.bat E:\\bin\\o4w_env.bat && E:\\bin\\o4w_env.bat && gdal_calc '))

## Mirar la respuesta del sistema a conexion a gdal_calc
(gdalcalc <- capture.output(system(paste0(execGDAL, ' --help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
(GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))






## 3. Crear buffer de ríos en zonas riparias + eliminar superficies de agua ----------------
## Usaremos la primera capa como referencia 

nombre_referencia <- '01_datos-originales/BOSQUE_1990.tif'
extent_nacional <- raster(nombre_referencia) # < Archivo externo >
extent_numerico <- extent_nacional@extent[c(1,3,2,4)] # xmin ymin xmax ymax
ancho_celda <- res(extent_nacional) # extraemos tamano de celda
info_raster <- gdalUtils::gdalinfo(nombre_referencia, noct = TRUE)
ancho_celda_texto <- grep('Pixel Size', value = TRUE, info_raster)
ancho_celda_texto2 <- gsub("([.])|[[:punct:]]|[a-zA-Z]| ", "\\1", strsplit(ancho_celda_texto, ',')[[1]])
options(digits=max(nchar(ancho_celda_texto2)))
ancho_celda


## Crear capa vectorial con el extent
gdalUtils::gdaltindex(index_file = '01_datos-originales\\extent_referencia.shp', 
                      gdal_file = '01_datos-originales\\BOSQUE_1990.tif')

## Rios ------
ruta_rios <- '01_datos-originales/CO_drenaje_sencillo.shp' # < Archivo externo >
file.exists( ruta_rios ) # Debe ser TRUE
rios_navegables <- '01_datos-originales/CO_drenaje_doble.shp' # < Archivo externo >
file.exists( ruta_rios_pol ) # Debe ser TRUE


## Crear buffer de 1m en capa de ríos tipo línea para unir a la capa creada anteriormente
# 1km =~ 0.008 # Conversion de grados a metros
#  1m =~ 0.000008

rios_buffer1m <-  paste0('02_rios-buffer/', 'rioslineasbuffer1m.shp')
if ( ! file.exists(rios_buffer1m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = '01_datos-originales/extent_referencia.prj',
                           src_datasource_name = ruta_rios, 
                           dst_datasource_name = rios_buffer1m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 0.000008) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(ruta_rios)))
    ) 
  ) # 60s
}


# Unir las capas de rios en poligonos y líneas (su buffer)
archivo_rios_poligonos <-  paste0('02_rios-buffer/', 'riospoligonos.shp')
if ( ! file.exists(archivo_rios_poligonos) ){
  
  ## Creamos una capa de rios poligonos igual a la de navegables
  print(system.time(
    gdalUtilities::ogr2ogr(t_srs = '01_datos-originales/extent_referencia.prj',
                           src_datasource_name = rios_navegables, 
                           dst_datasource_name = archivo_rios_poligonos,
                           f = "ESRI Shapefile")
  ) ) # 6s
  
  ## Anexamos la capa de buffer de 1m 
  print(system.time(
    gdalUtilities::ogr2ogr(t_srs = '01_datos-originales/extent_referencia.prj',
                           src_datasource_name = rios_buffer1m, 
                           dst_datasource_name = archivo_rios_poligonos,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(archivo_rios_poligonos)),
                           f = "ESRI Shapefile")
  )) # 25s
}



## Buffer rios ------
## Crear capa vector con buffers de 30 y 10m
# 1km =~ 0.008 # Conversion de grados a metros
#  1m =~ 0.000008
# 30m =~ 0.00024
#100m =~ 0.0008

archivo_buffer_30 <-  paste0('02_rios-buffer/', 'buffer30.shp')

if ( ! file.exists(archivo_buffer_30) ){
  print(system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = archivo_rios_poligonos, 
                           dst_datasource_name = archivo_buffer_30,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 0.00024) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_poligonos)) ))
  )) # 170
}

archivo_buffer_100 <-  paste0('02_rios-buffer/', 'buffer100.shp')
if (!file.exists(archivo_buffer_100)){
  print(system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = archivo_rios_poligonos, 
                           dst_datasource_name = archivo_buffer_100,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 0.0008) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_poligonos)) ))
  )) # 170
}

## 4. Rasterizar capas de zonas riparias----------------
## Crear capa raster de buffers de 30 y 10m

archivo_buffer_30_neg <- paste0('03_rios-raster/', 'buffer30neg.tif')
archivo_buffer_100_neg <- paste0('03_rios-raster/', 'buffer100neg.tif')

archivo_raster_buffer_30 <- paste0('03_rios-raster/', 'buffer30.tif')
archivo_raster_buffer_100 <- paste0('03_rios-raster/', 'buffer100.tif')


if (!file.exists(archivo_buffer_30_neg)){
  
  ## Rasterizar la capa de buffers
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_30, 
                                  dst_filename = archivo_buffer_30_neg,
                                  ot = 'Int8', # Para permitir valores negativos
                                  burn = 1, tr = ancho_celda, init = 0, 
                                  te =  extent_numerico, # xmin ymin xmax ymax
                                  co=c("COMPRESS=DEFLATE"))
  )) # 520seg
  
  ## Quitar los cuerpos permanentes de agua - rasterizar con valores negativos
  print(system.time(
    gdalUtils::gdal_rasterize(src_datasource = rios_navegables,
                              dst_filename = archivo_buffer_30_neg,  
                              add = TRUE, burn = -1)
  )) # 300seg
  
  ## Convertir en 1 y 0 comprimidos
  print(system.time(
    gdalUtilities::gdal_translate(src_dataset = archivo_buffer_30_neg, 
                                  dst_dataset = archivo_raster_buffer_30, 
                                  ot = 'Byte', # Forza a tener solo 0 y 1
                                  co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 66seg
}

if (!file.exists(archivo_buffer_100_neg)){
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_100, 
                                  dst_filename = archivo_buffer_100_neg, 
                                  ot = 'Int8', # Para permitir valores negativos
                                  burn = 1, tr = ancho_celda, init = 0, 
                                  te =  extent_numerico,
                                  co=c("COMPRESS=DEFLATE"))
  )) # 200seg
  
  ## Quitar los cuerpos permanentes de agua - rasterizar con valores negativos
  gdalUtils::gdal_rasterize(src_datasource = rios_navegables,
                            dst_filename = archivo_buffer_100_neg,  
                            add = TRUE, burn = -1)
  
  ## Convertir en 1 y 0 comprimidos
  gdalUtilities::gdal_translate(src_dataset = archivo_buffer_100_neg, 
                                dst_dataset = archivo_raster_buffer_100, 
                                ot = 'Byte', # Forza a tener solo 0 y 1
                                co=c("COMPRESS=DEFLATE", "NBITS=1"))
}



## Extraer estadísticas de zonas riparias nacionales ----
## Buffer de 30m
system.time( estadisticas_buffer_30m <- (gdalUtils::gdalinfo(archivo_raster_buffer_30, stats = TRUE, checksum = TRUE) ) )

dimensiones_buff_30 <- as.numeric(strsplit(gsub('[[:alpha:]]| ','', 
                                                grep('Size is', estadisticas_buffer_30m, value = TRUE) ),
                                           ",")[[1]])
celdas_buff_30 <- Reduce(f = '*', x = dimensiones_buff_30)
(promedio_buff_30 <- as.numeric(gsub(  " |STATISTICS_MEAN=", "",
                                       grep('STATISTICS_MEAN', 
                                            estadisticas_buffer_30m, value = TRUE)) ) )
(pixeles_zonas_riparias_30 <- celdas_buff_30 * promedio_buff_30)

## calcular esa estadística desde R interno. Se demora mucho mas
# raster_buffer_30 <- raster::raster(archivo_raster_buffer_30)
# system.time(stat30 <- raster::cellStats(raster_buffer_30, sum, na.rm = TRUE) ) # 200seg
# 0.01193012 * 53171 * 72218

## Buffer de 100m
system.time( estadisticas_buffer_100m <- gdalUtils::gdalinfo(archivo_raster_buffer_100, stats = TRUE, checksum = TRUE) )

dimensiones_buff_100 <- as.numeric(
  strsplit(gsub('[[:alpha:]]| ','', 
                grep('Size is', estadisticas_buffer_100m, value = TRUE) ),
           ",")[[1]])

celdas_buff_100 <- Reduce(f = '*', x = dimensiones_buff_100)
(promedio_buff_100 <- as.numeric(
  gsub(  " |STATISTICS_MEAN=", "",
         grep('STATISTICS_MEAN',
              estadisticas_buffer_100m, value = TRUE)) ) )

(pixeles_zonas_riparias_100 <- celdas_buff_100 * promedio_buff_100)



## 5. Derivar capas anuales de bosques riparios ----------------
(tifs_bosques <- list.files(path = '01_datos-originales/', pattern = 'BOSQUE.+.tif$|TIF$', 
                            full.names = TRUE))

## Verificamos la proyeccion y extent de los capas. Deberian ser iguales
unname(sapply(tifs_bosques, function(x){  raster(x)@nrows }))
unname(sapply(tifs_bosques, function(x){  raster(x)@ncols }))
unname(sapply(tifs_bosques, function(x){  raster(x)@crs@projargs }))

## Iterar sobre los bosques ----

for (i in 1:length(tifs_bosques)){ # i = 1 # }
  (nombre_bosque <- tifs_bosques[i])
  anio <- gsub('[[:punct:]]|[a-zA-Z]', '', basename(nombre_bosque))
  nombre_categorizado <- paste0('04_datos-filtrados/', basename(nombre_bosque))
  nombre_proyectado <- paste0('05_bosques-raster/', basename(nombre_bosque))
  
  
  
  ## 5. Proyectar y homogenizar capas ----------------
  print(paste(' ---- Capa ', i, ' de ', length(tifs_bosques), ' -- Reclasificar bosques entre 1 y 2 ',anio) )   
  
  ## Recategorizar raster si aun no existe. Dejar solo no bosque (0) y bosque (1)
  if( !file.exists(nombre_categorizado)){
      
      ## Opcion R
      print(system.time({ # 
        
        ## Opcion rapida. Escribe directamente el resultado como le pedimos # TRUE
        raster_bosque <- raster(nombre_bosque)
        calc(raster_bosque, fun = function(x) x == 1 , filename = nombre_categorizado, overwrite = TRUE,
             forcefun=FALSE, forceapply=FALSE, datatype = 'INT1U', NAflag = -5, options=c("COMPRESS=DEFLATE", "NBITS=1"))
      })) # 530
      
    # if (! GDAL){
    #   
    # } else {
    #   # Opcion GDAL. Mas lenta que R
    #   print(system.time(
    #     system(intern = TRUE,
    #            paste0(execGDAL, ' --calc="A==1" ',
    #                   '-A ', root2, '\\', nombre_bosque,
    #                   ' --outfile=', root2, '\\', nombre_categorizado,
    #                   ' --type=Byte --NoDataValue=-1 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
    #            )))) # 2400s
    # }
    
  }
  
  
  ## Reproyectar en caso que no coincidan los rasters
  print(paste(' ---- Capa ', i, ' de ', length(tifs_bosques), ' -- Reproyectando capas ',anio) )   
  capa_bosque <- raster(nombre_categorizado)
  
  if( !file.exists(nombre_proyectado)){
    ## Reproyectar raster si aun no coincide con la capa de referencia
    if (!all ( (capa_bosque@ncols == extent_nacional@ncols) &
               (capa_bosque@nrows == extent_nacional@nrows) &
               (capa_bosque@crs@projargs == extent_nacional@crs@projargs) &
               (capa_bosque@crs@projargs == extent_nacional@crs@projargs) &
               identical(extent(capa_bosque), extent(extent_nacional)))){
      
      print(system.time(
        gdalUtilities::gdalwarp(srcfile = nombre_categorizado, dstfile = nombre_proyectado, 
                                t_srs = '01_datos-originales/extent_referencia.prj',
                                tr = ancho_celda, te = extent_numerico,
                                ot = 'Byte', # Forza a tener solo 0 y 1
                                co=c("COMPRESS=DEFLATE", "NBITS=1"))
      )) # 102s
      ## En caso de problemas tambien se puede usar la funcion gdalUtils::gdalwarp dando el mismo resultado
  
      
    } else {
      ## Duplicar sin reproyectar
      file.copy(nombre_categorizado, nombre_proyectado)
    }
  }
  
  
  
  ## 7. Cruzar bosques anuales con zonas riparias año a año ----------------
  ## Intersectar con zona buffer --------
  ## Cruzar capas de bosques por anio con las areas de zonas buffer
  
  print(paste(' ---- Capa ', i, ' de ', length(tifs_bosques), ' -- Intersectar bosques y zonas riberenas ',anio) )   
  
  # Bosque en buffers 30m -----
  (bosque_buffer_30 <- paste0( '06_cruce-bosques-riparios/', 'bosque_ripario30m_', anio , '.tif'))
  
  
  if (!file.exists(bosque_buffer_30)){
    
    if (!GDAL){
      
      ## Opcion R
      stack_capas_30m <- raster::stack(nombre_raster_bosque, buffer30_recorte)
      
      print(system.time({ # 50 mins
        bosques_riparios_30 <- stack_capas_30m[[1]] *  stack_capas_30m[[2]] ;
        writeRaster(bosques_riparios_30, bosque_buffer_30, overwrite = TRUE, 
                    datatype = 'INT1U', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      }))
      
      
    } else {
      
      ## Opcion GDAL
      print(system.time(
        system(intern = TRUE,
               paste0(execGDAL,
                      ' --calc="0 + (1*(logical_and(A==1,B==1)))" ',
                      '-A ', root2, '\\', nombre_raster_bosque,' -B ', root2, '\\',buffer30_recorte,
                      ' --outfile=', root2, '\\', bosque_buffer_30,
                      ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
               )))) # 1713.92s - 2000s
    }
  }
  
  
  # Bosque en buffers 100m -----
  (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/', 'bosque_ripario100m_', anio , '.tif'))
  
  stack_capas_100m <- raster::stack(nombre_raster_bosque, buffer100_recorte)
  if (!file.exists(bosque_buffer_100)){
    
    if (!GDAL){
      ## Opcion R
      stack_capas_100m <- raster::stack(nombre_raster_bosque, buffer100_recorte)
      print(system.time({ # 50 mins
        bosques_riparios_100 <- stack_capas_100m[[1]] *  stack_capas_100m[[2]] ;
        writeRaster(bosques_riparios_100, bosque_buffer_30, overwrite = TRUE, 
                    datatype = 'INT1U', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      }))
      
    } else {
      # Opcion GDAL
      print(system.time(
        system(intern = TRUE,
               paste0(execGDAL, 
                      ' --calc="A*B" ',
                      '-A ', root2, '\\', nombre_raster_bosque,' -B ', root2, '\\',buffer100_recorte,
                      ' --outfile=', root2, '\\', bosque_buffer_100,
                      ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
               )))) # 1713.92s - 2000s
    }
  }

  
  
  
  ## 8. Extraer estadísticas y guardarlas en CSV año a año----------------
  ## Estadisticas ---
  ## Extraer estadisticas usando las funcionalidad de GDAL. Mucho mas rapido que R interno
  
  print(paste(' ---- Capa ', i, ' de ', length(tifs_bosques), ' -- Estadisticas ',anio) )   
  archivo_estadisticas <- paste0( '06_cruce-bosques-riparios/', 'estadisticas_bosque_riparios', anio , '.csv')
  
  ## Calcular estadisticas si el archivo CSV no existe
  if (!file.exists(archivo_estadisticas)){
    
    ## Estadisticas para 30m -----
    (estadisticas_ripario_30 <- gdalUtils::gdalinfo(bosque_buffer_30, stats = TRUE, checksum = TRUE))
    
    ## Extraer dimensiones (filas - columnas)
    (dimensiones_ripario_30 <- as.numeric(
      strsplit(gsub('[[:alpha:]]| ','', 
                    grep('Size is', estadisticas_buffer_30m,
                         value = TRUE) ), ",")[[1]]))
    
    (celdas_ripario_30 <- Reduce(f = '*', x = dimensiones_ripario_30))
    
    ## Extraer promedio
    (promedio_ripario_30 <- as.numeric(gsub(" |STATISTICS_MEAN=", "",
                                            grep('STATISTICS_MEAN', 
                                                 estadisticas_ripario_30, 
                                                 value = TRUE)) ) )
    (pixeles_bosques_riparios_30 <- celdas_ripario_30 * promedio_ripario_30)
    
    
    
    ## Estadisticas 100m ------
    (estadisticas_ripario_100 <- gdalUtils::gdalinfo(bosque_buffer_100, stats = TRUE, checksum = TRUE) )
    
    (dimensiones_ripario_100 <- as.numeric(
      strsplit(gsub('[[:alpha:]]| ','', 
                    grep('Size is', estadisticas_buffer_100m,
                         value = TRUE) ), ",")[[1]]))
    
    (celdas_ripario_100 <- Reduce(f = '*', x = dimensiones_ripario_100))
    
    (promedio_ripario_100 <- as.numeric(gsub(" |STATISTICS_MEAN=", "",
                                             grep('STATISTICS_MEAN', 
                                                  estadisticas_ripario_100, 
                                                  value = TRUE)) ) )
    (pixeles_bosques_riparios_100 <- celdas_ripario_100 * promedio_ripario_100)
    
    
    ## Crear tabla final y escribirla
    tabla_resumen <- data.frame(fecha = anio,
                                pix_rip30m = pixeles_zonas_riparias_30,
                                pix_bosrip30m = pixeles_bosques_riparios_30,
                                propbosrip30m = pixeles_bosques_riparios_30 / pixeles_zonas_riparias_30,
                                porcbosrip30m = pixeles_bosques_riparios_30 / pixeles_zonas_riparias_30 * 100,
                                pix_rip10m = pixeles_zonas_riparias_100,
                                pix_bosrip100m = pixeles_bosques_riparios_100,
                                propbosrip100m = pixeles_bosques_riparios_100 / pixeles_zonas_riparias_100,
                                porcbosrip100m = pixeles_bosques_riparios_100 / pixeles_zonas_riparias_100 * 100
    )
    
    #write.csv(tabla_resumen, file = archivo_estadisticas, row.names = FALSE) # Escribir separado por comas
    write.csv2(tabla_resumen, file = archivo_estadisticas, row.names = FALSE) # Escribir separado por punto y comas
  }
  
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- extraer estadisticas ',anio) )   
}



## 9. Compilar resultados y generación de tabla final ----------------
## Compilar datos de csv -----
(archivos_csv <- list.files(pattern = '^estadisticas_.+[0-9].csv', 
                           path = '05_bosques-riparios', 
                           full.names = TRUE))
tabla_compilada <- NULL
ans <- list()
for (i in 1:length(archivos_csv)){
  #print((archivos_csv[i]) )
  print(read.csv2(archivos_csv[i]) )
  #tabla_compilada <- rbind(tabla_compilada , read.csv2(archivos_csv[i]) )
  ans[[i]] <- read.csv2(archivos_csv[i])
}

df <- do.call(plyr::rbind.fill, ans)

df$porcbosrip30m <- df$pix_bosrip30m /df$pix_rip30m * 100
df$porcbosrip100m <- df$pix_bosrip100m /df$pix_rip10m * 100

write.csv(x = tabla_compilada, row.names = FALSE,
          file = paste0('05_bosques-riparios/',
                        'Consolidado_estadisticas_bosque_riparios_compiladas_',
                        Sys.Date(), '.csv'))

## Pasos para modificar el script con nuevos datos
# 1. Actualizar los archivos en la carpeta;
paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales')
# 2. Mantener los nombres de las capas de ser posible en los nuevos
# archivos. De lo contrario cambiar los nombres en las líneas con 
# el el comentario "# < Archivo externo >"
