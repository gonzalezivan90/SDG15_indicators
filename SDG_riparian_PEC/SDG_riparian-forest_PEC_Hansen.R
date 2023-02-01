## Creado por Ivan Gonzalez [gonzalezgarzonivan@gmail.com] y Patrick Jantz PhD (jantzenator@gmail.com)
## Estudiante doctorado en Ecoinformática [ig299@nau.edu] y profesor asistente [Patrick.Jantz@nau.edu] @ NAU

## El script es desarrollado en el contexto del proyecto NASA "SDG15: Life on Land".
## Los scripts e informaci[o]n se pueden descargar de este link: https://github.com/gonzalezivan90/SDG15_indicators


## AVISOS: ---- LEER !!!
## 1. Reabrir este archivo con codificación UTF8 (File/Archivo - Reopen with Encoding/Reabrir con codificación -- UTF8)
## 2. Todas las líneas de código deben ejecutarse.
## 3. Los análisis pesados no se repiten porque el código detecta que ya existen esos archivos
## 4. Comentarios del texto inicia con dos ##
## 5. Código inhabilitado inicia con un #. Puede removerse con Ctrl+C
## 6. Abrir en QGIS o similar los resultados después de cada paso y así verificar el avance del script
## 7. Los archivos se pueden descargar de esta carpeta: 
## 8. El algoritmo puede tomar hasta 6H en ejecutarse por el tamaño de los archivos
## 9. Se debe crear una carpeta de trabajo. Ojalá cerca de la raíz y sin espacios. Ej: C:/SDG_riparios/
## 10. Las líneas de código que se deben cambiar tienen este texto al final "#CAMBIAR ---"
## 11. Los números al final de algunas funciones indican el tiempo aproximado en segundos que toma la operación

## Pasos de este script:
## 1. Liberías, definir ruta de trabajo + carpetas + archivos
## 2. Localizar gdal_calc.py que hace los análisis más rápidos, aunque no es obligatorio
## 3. Combinar capas de bosques que vienen partidos desde Earth Engine
## 4. Crear buffer de ríos en zonas riparias + eliminar superficies de agua
## 5. Rasterizar capas de zonas riparias
## 6. Extraer limite forestal - recortar el raster nacional - capa vectorial
## 7. Derivar capas anuales de bosque año a año
## 8. Cruzar bosques anuales con zonas riparias año a año
## 9. Extraer estadísticas y guardarlas en CSV año a año
##10. Compilar resultados y generación de tabla final

## INPUTS:
## + 4 Capas de bosque (Hansen et al. 2013) extra[i]das de Google Earth Engine: https://glad.earthengine.app/view/global-forest-change
##   - 2 capas (norte y sur) de % de cobertura de dosel
##   - 2 capas (norte y sur) de a[n]o de p[e]rdida
##     Estas capas se pueden obtener con el siguiente script de Google Earth: https://code.earthengine.google.com/f102415e12f33395aa82354931738b59
## + 3 Capa de r[í]os sencillos (vector tipo l[í]nea) de Colombia, Ecuador, Per[u]
## + 3 Capa de r[í]os navegables o permanentes (vector tipo pol[í]gono) de Colombia, Ecuador, Per[u]
##     Ecuador: https://www.geoportaligm.gob.ec/portal/index.php/descargas/cartografia-de-libre-acceso/registro/
##     Peru: http://geo2.ana.gob.pe:8080/geonetwork/srv/spa/catalog.search;jsessionid=3F3668A151414B7481D1CB0B0757EAFE?node=srv#/search?facet.q=type%2Fdataset%26spatialRepresentationType%2Fvector&resultType=details&sortBy=relevance&fast=index&_content_type=json&from=1&to=20
##     Colombia: https://www.dane.gov.co/files/geoportal-provisional/index.html

## OUTPUTS:
## Capas raster TIFF de zonas riparias de 30 y 100m
## Capas raster TIFF anuales de bosque en zonas riparias
## Tablas CSV con las estad[i]sticas anuales




## 1. Liberías, definir ruta de trabajo + carpetas + archivos ----------------
## Cargar librerías
library(raster) # Raster
library(foreign) # Cargar las tablas .dbf de shapefiles
library(rgdal)  # Funciones de GDAL / OGR para R
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(gdalUtils) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
## 'C:/temp/SDG_riparian_PEC' Es la carpeta del proyecto. Esta puede SI PUEDE en cada computador
root <- 'C:/temp/SDG_riparian_PEC/' ####### CAMBIAR --- # Usar / o \\. Cada uno cambia su carpeta_trabajo
dir.create(paste0(root, '01_datos-originales'), recursive = TRUE) ## Crear la carpeta si no existe
file.exists(paste0(root, '01_datos-originales')) ## Debe ser TRUE 

## 
setwd( root ) # asignar ruta de trabajo
list.dirs(recursive = FALSE) # Revisar carpetas existentes


## Acá se deben ubicar los archivos originales descomprimidos:
paste0(root, '/01_datos-originales')



## Los siguientes archivos son necesarios para ejecutar la totalidad del script

inputs <-  c('hansen_pec_tree_-0000065536-0000000000.tif', # % de dosel en tif
             'hansen_pec_tree_-0000000000-0000000000.tif', # % de dosel en tif
             'hansen_pec_loss_-0000065536-0000000000.tif', # Deforestación en tif
             'hansen_pec_loss_-0000000000-0000000000.tif', # Deforestación en tif
             'PE_Rios_100000_350k.shp',                   # Rios de Peru en lineas
             'PE_Rios_navegables.shp',                    # Rios de Peru en poligonos (parte 1)
             'PE_Rios_area_idep_ign_100k_geogpsperu.shp', # Rios de Peru en poligonos (parte 2)
             'EC_Rio_l_250K_ECU_IGM.shp',                 # Rios de Ecuador en lineas
             'EC_Rio_a_250K_ECU_IGM.shp',                 # Rios de Ecuador en poligonos
             'CO_drenaje_sencillo.shp',                   # Rios de Colombia en lineas
             'CO_drenaje_doble.shp'                       # Rios de Colombia en poligonos
)

condicion <- inputs %in% list.files(paste0(root, '/01_original-data'))

## Lo siguiente debe ser TRUE y no FALSE, Se revisa que existan las capas necesarias. NO puede ser FALSE.
all(condicion)


## Evaluar si tenemos las condiciones para correr el script
if( !all(condicion)){
  stop(paste0('ERROR: Copie los archivos mencionados a la carpeta: ---',
              paste0(root, '/01_datos-originales')
              , '---'))
}


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
root2 <- paste0(paste0(root2, collapse = '/'), '/')


## Crear carpetas intermedias donde se guardan los archivos
outDirs <- c('02_rios-buffer', '03_rios-raster', 
             '04_bosques-filtrados', '05_bosques-raster', 
             '06_cruce-bosques-riparios')
sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings si ya existen las carpetas. No es grave


## 2. Localizar gdal_calc.py que hace los análisis más rápidos, aunque no es obligatorio ----------------
## Evaluar acceso a gdal_calc ----

## Esto se hace para que el análisis a generar sea bastante más rápido, aunque no es necesario. Es opcional
## Necesitamos ubicar algunos archivos que generalmente trae QGIS. 
## En caso de no poder generar TRUE en el objeto "GDAL", no importa. No se usa gdal_calc de forma externa sino
## que se unsa la versión interna de R, que es mas lenta pero nos produce los mismo resultados

##Aca la configuración para QGIS3.14, pero abajo hay instrucciones para otras versiones. #CAMBIAR ---
execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat ', # Mantener espacio al final. Ubicación del archivo OSGeo4W.bat que es el orquestador de ejecutables GIS00
                   'py3_env.bat ', # Mantener espacio. Ubicación de el ejecutable que configura entorno de python. Puede llamarse tambien o4w_env.bat
                   '&& py3_env.bat ', # Mantener espacio al . Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
                   '&& gdal_calc ') # Llamado de gdal_calc, Dejar espacio al final

# # Instrucción en otras versiones QGIS. Se pueden habilitar las siguientes versiones. #CAMBIAR ---
## QGIS version 3.16
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat py3_env.bat && py3_env.bat && gdal_calc '))
## QGIS version 3.20
# (execGDAL <- paste0('C:\"Program Files"\"QGIS 3.20.1"\\OSGeo4W.bat C:\"Program Files"\\"QGIS 3.3.20.1"\\bin\\o4w_env.bat && C:\\"Program Files"\\"QGIS 3.3.20.1"\\bin\\o4w_env.bat && gdal_calc '))
## QGIS version 3.22
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && gdal_calc '))
## QGIS version 3.22.12
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.12"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.12"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.12"\\bin\\o4w_env.bat && gdal_calc '))
## QGIS version 3.28
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.28.1"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.28.1"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.28.1"\\bin\\o4w_env.bat && gdal_calc '))

## Mirar la respuesta del sistema a conexion a gdal_calc
(gdalcalc <- capture.output(system(paste0(execGDAL, ' --help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
(GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))



## 3. Combinar capas de bosques que vienen partidos desde Earth Engine ----------------

## La combinacion de las dos capas raster provenientes desde Earth Engine que por la 
## extension del area de estudio, han sido partidas en dos zonas, norte y sur
## En R este procedimiento puede demorar bastante, por lo que se recomienda hacerlo
## en QGIS, ArcGIS o gdal. Se deben combinar en dos procedimientos independientes, las capas 
## de bosque (hansen_pec_tree_.tif) y de perdida (hansen_pec_loss_.tif)
## En QGIS se puede hacer del menu: Raster >> Miscellaneous >> Merge. En ArcGIS se puede hacer 
## desde el menu ArcToolBox >> Data Managment Tools >> Raster >> Raster dataset >> Mosaic
## Desde GDAL se pueden hacer con los siguientes comandos
# time gdal_merge.py -o hansen_tree.tif -ot Int16 -co COMPRESS=DEFLATE -co NBITS=5 hansen_pec_tree_-0000000000-0000000000.tif hansen_pec_tree_-0000065536-0000000000.tif
# time gdal_merge.py -o hansen_loss.tif -ot Int16 -co COMPRESS=DEFLATE -co NBITS=9 hansen_pec_loss_-0000000000-0000000000.tif hansen_pec_loss_-0000065536-0000000000.tif

## Límite nacional
archivo_bosques <- '01_datos-originales/hansen_tree.tif' # < Archivo externo >
file.exists(archivo_bosques) # Debe ser TRUE
archivo_perdida <- '01_datos-originales/hansen_loss.tif' # < Archivo externo >
file.exists(archivo_bosques) # Debe ser TRUE

raster_regional <- raster(archivo_bosques) # < Archivo externo >
extent_numerico <- raster_regional@extent[c(1,3,2,4)] # xmin ymin xmax ymax
# extent_numerico <- c(-81.34127, -18.39948, -66.78425,  12.49268)
ancho_celda <- res(raster_regional)
# ancho_celda <- c(0.0002694946, 0.0002694946)



## 4. Crear buffer de ríos en zonas riparias + eliminar superficies de agua ----------------
## Rios ------
ruta_rios_linea_Pe <- '01_datos-originales/PE_Rios_100000_350k.shp' # < Archivo externo >
file.exists( ruta_rios_linea_Pe ) # Debe ser TRUE
ruta_rios_area_Pe <- '01_datos-originales/PE_Rios_navegables.shp' # < Archivo externo >
file.exists( ruta_rios_area_Pe ) # Debe ser TRUE
ruta_rios_area_Pe2 <- '01_datos-originales/PE' # < Archivo externo >
file.exists( ruta_rios_area_Pe ) # Debe ser TRUE

ruta_rios_linea_Ec <- '01_datos-originales/EC_Rio_l_250K_ECU_IGM.shp' # < Archivo externo >
file.exists( ruta_rios_linea_Ec ) # Debe ser TRUE
ruta_rios_area_Ec <- '01_datos-originales/EC_Rio_a_250K_ECU_IGM.shp' # < Archivo externo >
file.exists( ruta_rios_area_Ec ) # Debe ser TRUE

ruta_rios_linea_Co <- '01_datos-originales/CO_drenaje_sencillo.shp' # < Archivo externo >
file.exists( ruta_rios_linea_Co ) # Debe ser TRUE
ruta_rios_area_Co <- '01_datos-originales/CO_drenaje_doble.shp' # < Archivo externo >
file.exists( ruta_rios_area_Co ) # Debe ser TRUE


## Unir rios que son navegables o areas
## Esta capa se debe debe eliminar de las zonas riparias porque son aguas permanentes
rios_navegables <-  paste0('02_rios-buffer/', 'rios_navegables.shp') 

if ( ! file.exists(rios_navegables) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = ruta_rios_area_Pe, overwrite = TRUE, 
                           dst_datasource_name = rios_navegables,
                           f = "ESRI Shapefile")
  ) # 1seg
  
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = ruta_rios_area_Pe2, 
                           dst_datasource_name = rios_navegables,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(rios_navegables)),
                           f = "ESRI Shapefile")
  ) # 1seg
  
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = ruta_rios_area_Ec, 
                           dst_datasource_name = rios_navegables,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(rios_navegables)),
                           f = "ESRI Shapefile")
  ) # 1seg
  
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = ruta_rios_area_Co, 
                           dst_datasource_name = rios_navegables,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(rios_navegables)),
                           f = "ESRI Shapefile")
  ) # 1seg
}


## Unir rios que son sencillos o lineas
rios_sencillos <-  paste0('02_rios-buffer/', 'rios_sencillos.shp') 

if ( ! file.exists(rios_sencillos) ){

  ## Crear la primer capa con rios de Peru
  ## Eliminar lineas que no son rios sino limite nacional (Valor LL)
  system.time(gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                         src_datasource_name = ruta_rios_linea_Pe, 
                         dst_datasource_name = rios_sencillos,
                         f = "ESRI Shapefile", 
                         where = "TIPO!='LL'") ## where = "TIPO!='LL'" # <>
  ) # 175s
  

  ## Anadir capa de Ecuador
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = ruta_rios_linea_Ec, 
                           dst_datasource_name = rios_sencillos,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(rios_sencillos)),
                           f = "ESRI Shapefile")
  ) # 17s
  
  ## Anadir capa de Colombia
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = ruta_rios_linea_Co, 
                           dst_datasource_name = rios_sencillos,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(rios_sencillos)),
                           f = "ESRI Shapefile")
  ) # 
}


## Crear buffers de 30 y 100m en capa de ríos tipo línea
## Se crearan para lineas y polígonos

## Rios sencillos en lineas de 30m
# 1km =~ 0.008 # Conversion de grados a metros
#  1m =~ 0.000008
# 30m =~ 0.00024
rios_buffer30m <-  paste0('02_rios-buffer/', 'rios_sencillos_buffer30m.shp')
if ( ! file.exists(rios_buffer30m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = rios_sencillos, 
                           dst_datasource_name = rios_buffer30m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 0.00024) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(rios_sencillos))) 
    ) 
  ) # 330s
}

## Rios sencillos en lineas de 100m
# 1km =~ 0.008 # Conversion de grados a metros
#  1m =~ 0.000008
#100m =~ 0.0008

rios_buffer100m <-  paste0('02_rios-buffer/', 'rios_sencillos_buffer100m.shp')
if ( ! file.exists(rios_buffer100m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = rios_sencillos, 
                           dst_datasource_name = rios_buffer100m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 0.0008) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(rios_sencillos))) 
    ) 
  ) # 330s
}


## Rios navegables en poligonos de 30m
# 1km =~ 0.008 # Conversion de grados a metros
# 1m =~ 0.000008
# 30m =~ 0.00024
rios_navegables_buffer30m <-  paste0('02_rios-buffer/', 'rios_navegables_buffer30m.shp')
if ( ! file.exists(rios_navegables_buffer30m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = rios_navegables, 
                           dst_datasource_name = rios_navegables_buffer30m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 0.00024) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(rios_navegables))) 
    ) 
  ) # 160s
}

## Rios navegables en poligonos de 100m
# 1km =~ 0.008 # Conversion de grados a metros
#  1m =~ 0.000008
#100m =~ 0.0008
rios_navegables_buffer100m <-  paste0('02_rios-buffer/', 'rios_navegables_buffer100m.shp')
if ( ! file.exists(rios_navegables_buffer100m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:4326',
                           src_datasource_name = rios_navegables, 
                           dst_datasource_name = rios_navegables_buffer100m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 0.0008) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(rios_navegables))) 
    ) 
  ) # 160s
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
    gdalUtilities::gdal_rasterize(src_datasource = rios_navegables_buffer30m,
                                  dst_filename = archivo_buffer_30_neg,
                                  ot = 'Int16', # Para permitir valores negativos
                                  burn = 1, tr = ancho_celda, init = 0,
                                  te =  extent_numerico, # xmin ymin xmax ymax
                                  co=c("COMPRESS=DEFLATE"))
  )) # 300seg

  ## Agregar rios navegables
  print(system.time(
    gdalUtils::gdal_rasterize(src_datasource = rios_buffer30m,
                              dst_filename = archivo_buffer_30_neg,
                              add = TRUE, burn = 1)
  )) # 300seg


  ## Quitar los cuerpos permanentes de agua - rasterizar con valores negativos
  print(system.time(
    gdalUtils::gdal_rasterize(src_datasource = rios_navegables,
                              dst_filename = archivo_buffer_30_neg,
                              add = TRUE, burn = -10)
    )) # 300seg


  ## Convertir en 1 y 0 comprimidos
  print(system.time(
    gdalUtilities::gdal_translate(src_dataset = archivo_buffer_30_neg,
                                  dst_dataset = archivo_raster_buffer_30,
                                  ot = 'Byte', # Forza a tener solo 0 y 1
                                  co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 80seg
  
}

if (!file.exists(archivo_buffer_100_neg)){
  
  ## Rasterizar la capa de buffers
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = rios_navegables_buffer100m,
                                  dst_filename = archivo_buffer_100_neg,
                                  ot = 'Int16', # Para permitir valores negativos
                                  burn = 1, tr = ancho_celda, init = 0,
                                  te =  extent_numerico, # xmin ymin xmax ymax
                                  co=c("COMPRESS=DEFLATE"))
  )) # 300seg
  
  ## Agregar rios navegables
  print(system.time(
    gdalUtils::gdal_rasterize(src_datasource = rios_buffer100m,
                              dst_filename = archivo_buffer_100_neg,
                              add = TRUE, burn = 1)
  )) # 300seg
  
  
  ## Quitar los cuerpos permanentes de agua - rasterizar con valores negativos
  print(system.time(
    gdalUtils::gdal_rasterize(src_datasource = rios_navegables,
                              dst_filename = archivo_buffer_100_neg,
                              add = TRUE, burn = -10)
  )) # 300seg
  
  
  ## Convertir en 1 y 0 comprimidos
  print(system.time(
    gdalUtilities::gdal_translate(src_dataset = archivo_buffer_100_neg,
                                  dst_dataset = archivo_raster_buffer_100,
                                  ot = 'Byte', # Forza a tener solo 0 y 1
                                  co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 80seg
}



## Extraer estadísticas de zonas riparias nacionales ----
## Buffer de 30m
system.time( estadisticas_buffer_30m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_30, stats = TRUE, checksum = TRUE) ) ) # 40s

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
# system.time(stat30 <- raster::cellStats(raster_buffer_30, sum, na.rm = TRUE) ) # 311.75
# stat30 == (6191854080 * 0.01216495609018)

## Buffer de 100m
system.time( estadisticas_buffer_100m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_100, stats = TRUE, checksum = TRUE) )) # 40s

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





## 5. Extraer limite forestal - recortar el raster nacional - capa vectorial ----------------

## Convertir capa de bosques entre 0 y 1 con bosques a 2000
archivo_bosques <- '01_datos-originales/hansen_tree.tif' # < Archivo externo >
archivo_perdida <- '01_datos-originales/hansen_loss.tif' # < Archivo externo >
file.exists(archivo_bosques) # Debe ser TRUE
file.exists(archivo_perdida) # Debe ser TRUE


## Generar capa con pixeles con bosque a 2000
archivo_inicial_0 <- '04_bosques-filtrados/Bosque_inicial2000.tif'
if (! file.exists(archivo_inicial_0) ){
  if (!GDAL){
    ## Opcion R
    print(system.time(
      if (! file.exists(archivo_inicial_0) ){
        capa_bosque <- raster(archivo_bosques)
        bosque_inicial2000 <- Which(capa_bosque >= 25)
        # plot(bosque_inicial2000)
        writeRaster(x = bosque_inicial2000, filename = archivo_inicial_0,
                    datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      }
    )) # 4356.74seg # 70mins
    
  } else {
    ## Opcion GDAL
    print(system.time(
      system(
        paste0(execGDAL, 
               ' --calc="A>= 25" ',
               '-A ', root2, '\\', archivo_bosques,
               ' --outfile=', root2, '\\', archivo_inicial_0,
               ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
        )))) # # 221.73 s
  }
}


## Cambiar datos nullos a 0 en capa raster de perdida de bosque. Este paso no es necesario si el merge de capas de 
## perdida asignó como 0 los pixeles que no han presentado deforestacion. De todas formas queda esta opcion para ejecutar 
archivo_perdida_0 <- '04_bosques-filtrados/Perdida_2001_2020_llenado0.tif'
if (! file.exists(archivo_perdida_0) ){
  print(system.time(
    capa_perdida_0 <- gdalUtilities::gdal_translate(a_nodata = 999, # dato que se usa como nodata original 
                                                    src_dataset = archivo_perdida, 
                                                    dst_dataset = archivo_perdida_0,
                                                    ot = 'Int16',
                                                    co=c("COMPRESS=DEFLATE"))) # Esta opción permite reducir de 2GB a 15MB
  ) # 221.73 s
}


## Cargar capas raster en R en caso que debamos operar en R los cálculos
raster_inicial_0 <- raster(archivo_inicial_0)
raster_perdida_0 <- raster(archivo_perdida_0)


## 6. Derivar capas anuales de bosque año a año ----------------

## Extraer fechas de perdida de bosque sobre la cual iterar. Dejaremos desde 2000 a 2020
fechas_estadisticas <- gdalUtils::gdalinfo(archivo_perdida_0, approx_stats = TRUE)
fechas_unicas0 <- range(as.numeric(gsub('[[:alpha:]]| |=|_', '', 
                                        grep('_MIN|_MAX', fechas_estadisticas, value = TRUE))))
fechas_unicas <- fechas_unicas0[1]:fechas_unicas0[2]



## Definir si se deben guardar los pasos intermedios. La variable "pasos_intermedios" debe ser TRUE o FALSE.
## Dejar en FALSE si se debe saltar el paso de bosques nacionales de 
## la carpeta "04_calculoNacional/05_bosques-raster/" y solo escribir los
## datos finales en la carpeta "04_calculoNacional/06_cruce-bosques-riparios/". Ahorra 15mins approx por cada año trabajado
pasos_intermedios <- TRUE

## Iterar sobre los bosques ----
## Las siguientes intrucciones se repetiran para cada año entre 2000 y 2020
for( f in 1:length(fechas_unicas)){ # f = 5 # }
  
  ## Definir valores en cada paso, como anio y nombres de capas
  (anio <- 2000+fechas_unicas[f]) ## entre 2000 y 2020
  anio2 <- fechas_unicas[f] ## entre 0 y 20
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas),
              ' -- anio ', anio ) )
  (nombre_raster_bosque <- paste0('05_bosques-raster', '/bosqueFecha_', anio, '.tif' ))
  
  ## Tomar dos posibles rutas: Escribiendo datos de bosques anuales en "04_calculoNacional/05_bosques-raster/" o no
  
  if (pasos_intermedios){
    ## 6. Cruces completos ----------------
    ## Calcular pasos intermedios. Escribir datos en las carpetas 
    ## "04_calculoNacional/05_bosques-raster/" y "04_calculoNacional/06_cruce-bosques-riparios/"
    
    ## 6. Derivar capas anuales de bosque año a año ----------------
    ## Crear capa de bosque por anio reclasificando valores de capa de bosque de perdida
    if (!file.exists(nombre_raster_bosque)){
      if (! GDAL){
        ## Hecho en R
        
        raster_j <- raster_inicial_0 - raster::Which(raster_perdida_0 <= anio2 & raster_perdida_0 !=0);
        writeRaster(x = raster_j, filename = nombre_raster_bosque, 
                    datatype = 'INT1U', options=c("COMPRESS=DEFLATE", "NBITS=1"));
        
        
      } else {
        ## Hecho con gdal_calc.py
        print(system.time( 
          system(intern = TRUE,
                 paste0(execGDAL,
                        ' --calc="A-(1*(logical_and(B!=0,B<=', anio2, ')))" ',
                        '-A ', root2, '\\', archivo_inicial_0,' -B ', root2, '\\',archivo_perdida_0, 
                        ' --outfile=', root2, '\\', nombre_raster_bosque,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 ) )
        )) # 4800s
      }
      
      print ( paste('   -- Guardado bosque fecha ', anio ) )
    }
    
    ## 7. Cruzar bosques anuales con zonas riparias año a año ----------------
    ## Intersectar con zona buffer --------
    ## Cruzar capas de bosques por anio con las areas de zonas buffer
    
    print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Intersectar bosques y zonas riberenas ',anio) )   
    
    # Bosque en buffers 30m -----
    (bosque_buffer_30 <- paste0( '06_cruce-bosques-riparios/', 
                                 'bosque_ripario30m_', anio , '.tif'))
    
    
    
    if (!file.exists(bosque_buffer_30)){
      
      if (!GDAL){
        
        ## Opcion R
        stack_capas_30m <- raster::stack(nombre_raster_bosque, archivo_raster_buffer_30)
        
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
                        ' --calc="A*B" ',
                        '-A ', root2, '\\', nombre_raster_bosque,' -B ', root2, '\\',archivo_raster_buffer_30,
                        ' --outfile=', root2, '\\', bosque_buffer_30,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 )))) # 1713.92s - 2000s
      }
    }
    
    
    # Bosque en buffers 100m -----
    (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/',
                                  'bosque_ripario100m_', anio , '.tif'))
    
    if (!file.exists(bosque_buffer_100)){
      
      if (!GDAL){
        ## Opcion R
        stack_capas_100m <- raster::stack(nombre_raster_bosque, archivo_raster_buffer_100)
        stack_capas_100m <- raster::stack(nombre_raster_bosque, archivo_raster_buffer_100)
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
                        '-A ', root2, '\\', nombre_raster_bosque,' -B ', root2, '\\',archivo_raster_buffer_100,
                        ' --outfile=', root2, '\\', bosque_buffer_100,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 )))) # 1713.92s - 2000s
      }
    }
    
  } else {
    
    ## 6 + 7. SIMPLIFICADO: Cruzar capa de bosques y pérdida con zonas riparias año a año ----------------
    ## Omitir pasos intermedios. Escribir resultados directamente en la carpeta "06_cruce-bosques-riparios/"
    ## y no escribir nada en "05_bosques-raster/"
    
    # Bosque en buffers 30m -----
    (bosque_buffer_30 <- paste0( '06_cruce-bosques-riparios/', 'bosque_ripario30m_', anio , '.tif'))
    print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Intersectar bosques y zonas riberenas ',anio, ' 30m -') )   
    
    if (!file.exists(bosque_buffer_30)){
      if (! GDAL){
        ## Hecho en R
        stack_capas_30m <- raster::stack(archivo_inicial_0, archivo_perdida_0, archivo_raster_buffer_30)
        print(system.time(
          {resultado_raster_riparios_30 <- stack_capas_30m[[1]] * ( stack_capas_30m[[2]] > anio2 | stack_capas_30m[[2]] == 0  )* stack_capas_30m[[3]] ;
          writeRaster(x = resultado_raster_riparios_30, filename = bosque_buffer_30, 
                      overwrite = TRUE, datatype = 'LOG1S', options=c("COMPRESS=DEFLATE", "NBITS=1"))}
        )) # 
        
      } else {
        ## Hecho con gdal_calc.py
        print(system.time( 
          system(intern = TRUE,
                 paste0(execGDAL,
                        ' --calc=" A * logical_or(B>', anio2, ',B==0) * C" ', 
                        ' -A ', root2, '\\', archivo_inicial_0, ' -B ', root2, '\\',archivo_perdida_0, ' -C ', root2, '\\',archivo_raster_buffer_30, 
                        ' --outfile=', root2, '\\', bosque_buffer_30,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 ) )
        )) # 2722.67
      }
    }
    
    # Bosque en buffers 100m -----
    (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/', 'bosque_ripario100m_', anio , '.tif'))
    print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Intersectar bosques y zonas riberenas ',anio, ' 100m -') )   
    
    if ( !file.exists(bosque_buffer_100) ){
      
      if (!GDAL){
        ## Opcion R
        stack_capas_100m <- raster::stack(archivo_inicial_0, archivo_perdida_0, archivo_raster_buffer_100)
        print(system.time(
          {resultado_raster_riparios_100 <- stack_capas_100m[[1]] * ( stack_capas_100m[[2]] > anio2 | stack_capas_100m[[2]] == 0  )* stack_capas_100m[[3]] ;
          writeRaster(x = resultado_raster_riparios_100, filename = bosque_buffer_100, 
                      overwrite = TRUE, datatype = 'LOG1S', options=c("COMPRESS=DEFLATE", "NBITS=1"))}
        )) # 
        
      } else {
        ## Hecho con gdal_calc.py
        print(system.time( 
          system(intern = TRUE,
                 paste0(execGDAL,
                        ' --calc=" A * ( (B>', anio2, ')+ (B==0)) * C" ',
                        ' -A ', root2, '\\', archivo_inicial_0, ' -B ', root2, '\\',archivo_perdida_0, ' -C ', root2, '\\',archivo_raster_buffer_100, 
                        ' --outfile=', root2, '\\', bosque_buffer_100,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 )))) # 2663.83
      }
    }
  }
  
  
  
  ## 8. Extraer estadísticas y guardarlas en CSV año a año----------------
  ## Estadisticas ---
  ## Extraer estadisticas usando las funcionalidad de GDAL. Mucho mas rapido que R interno
  
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Estadisticas ',anio) )   
  archivo_estadisticas <- paste0( '06_cruce-bosques-riparios/', 'estadisticas_bosque_riparios', anio , '.csv')
  
  ## Calcular estadisticas si el archivo CSV no existe
  if (!file.exists(archivo_estadisticas)){
    
    ## Estadisticas para 30m -----
    (estadisticas_ripario_30 <- capture.output(
      gdalUtilities::gdalinfo(bosque_buffer_30, stats = TRUE, checksum = TRUE)))
    
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
    (estadisticas_ripario_100 <- capture.output(
      gdalUtilities::gdalinfo(bosque_buffer_100, stats = TRUE, checksum = TRUE) ))
    
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
archivos_csv <- list.files(pattern = '^estadisticas_.+[0-9].csv', 
                           path = '06_cruce-bosques-riparios', 
                           full.names = TRUE)
tabla_compilada <- NULL
for (i in 1:length(archivos_csv)){
  #print((archivos_csv[i]) )
  #print(read.csv2(archivos_csv[i]) )
  tabla_compilada <- rbind(tabla_compilada , read.csv2(archivos_csv[i]) )
}

tabla_compilada

## Si la tabla tiene valores extraños, se recomienda hacer estos pasos en orden:
## - Cargar las capas TIF en QGIS para ver que no tengan errores. 
##   Si se ven defectos en la capa:
##      - eliminarla junto a los archivos CSV y XML, y correr nuevamente el ciclo
##   Si no se ven errores:
##      - Eliminar los archivos CSV y .aux.xml de las capas, y volver
##        a correr el ciclo "for" donde calcule nuevamente los metadatos
## 

write.csv(x = tabla_compilada, row.names = FALSE,
          file = paste0('06_cruce-bosques-riparios/',
                        'Consolidado_estadisticas_bosque_riparios_compiladas_',
                        Sys.Date(), '.csv'))


write.csv2(x = tabla_compilada, row.names = FALSE,
          file = paste0('06_cruce-bosques-riparios/',
                        'Consolidado_estadisticas_bosque_riparios_compiladas2_',
                        Sys.Date(), '.csv'))

## Pasos para modificar el script con nuevos datos
# 1. Actualizar los archivos en la carpeta;
paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales')
# 2. Mantener los nombres de las capas de ser posible en los nuevos
# archivos. De lo contrario cambiar los nombres en las líneas con 
# el el comentario "# < Archivo externo >"
