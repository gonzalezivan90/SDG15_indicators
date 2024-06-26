## Creado por Ivan Gonzalez [gonzalezgarzonivan@gmail.com]
## Estudiante doctorado en Ecoinformática @ NAU - ig299@nau.edu
## Buscar la version mas reciente en la página: https://github.com/gonzalezivan90/SDG15_indicators
## Script para calcular bosques riparios en Ecuador usando datos oficiales, y en forma de indicador para el
## reporte de indicadores complementarios ODS/SDG15.
##    Contacto: gonzalezgarzonivan@gmail.com / ig299@nau.edu
## Obtener datos aca: http://ide.ambiente.gob.ec:8080/mapainteractivo/



## AVISOS: ----
## 1. Reabrir este archivo con codificación UTF8 (File/Archivo - Reopen with Encoding/Reabrir con codificación -- UTF8)
## 2. Todas las líneas de código deben ejecutarse.
## 3. Los análisis pesados no se repiten porque el código detecta que ya existen esos archivos
## 4. Comentarios del texto inicia con dos ##
## 5. Código inhabilitado inicia con un #. Puede removerse con Ctrl+C
## 6. Abrir en QGIS o similar los resultados después de cada paso y así verificar el avance del script
## 7. Los archivos se pueden descargar de esta carpeta: https://drive.google.com/open?id=16YRC4BjqgC77Ffb2jUP1yV4CfcgI7yDk&authuser=ig299%40nau.edu&usp=drive_fs
## 8. El algoritmo puede tomar hasta 6H en ejecutarse por el tamaño de los archivos
## 9. Se debe crear una carpeta de trabajo. Ojalá cerca de la raíz y sin espacios. Ej: C:/Ecuador_riparios/
## 10. Las líneas de código que se deben cambiar tienen este texto al final "#CAMBIAR ---"
## 11. Los números al final de algunas funciones indican el tiempo aproximado en segundos que toma la operación

## Pasos de este script:
## 1. Liberías, definir ruta de trabajo + carpetas + archivos
## 2. Crear buffer de ríos en zonas riparias + eliminar superficies de agua
## 3. Rasterizar capas de zonas riparias
## 4. Extraer limite forestal - recortar el raster nacional - capa vectorial
## 5. Cruzar bosques anuales con zonas riparias año a año
## 6. Extraer estadísticas y guardarlas en CSV año a año
## 7. Compilar resultados y generación de tabla final



## Cargar librería ---

## Cargar librería --- Todo debería ser TRUE. Usar archivo de instalación si no.
c('raster', 'gdalUtilities', 'gdalUtils', 'devtools') %in% rownames(installed.packages())

# library(raster) # Raster
# library(rgdal)  # Funciones de GDAL / OGR para R
library(terra) # Funciones de capas raster
library(sf)  # Funciones de capas vectoriales
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(gdalUtils) # Conexion a suit GDAL/OGR para ejecutar externo 

## Leer funciones
## Cargar funcion de conteo de pixeles
source("https://raw.githubusercontent.com/gonzalezivan90/SDG15_indicators/main/scripts/tabuleRaster.R")
## Cargar funcion para encontrar los ejecutables de GDAL en el computador
source("https://raw.githubusercontent.com/gonzalezivan90/SDG15_indicators/main/scripts/find_gdal.R")

# Si el anterior genera error por conexión a Github, entonces llamar localmente al achivo:
# El archivo se puede descargar con la opción "Guardar como" desde el navegador, al copiar el link anterior. Es posible que el archivo descarge como R_03_tabuleRaster.R.txt
## source("C:/temp/tabuleRaster.R") ## Cambiar por ruta equivalente, o agregar ".txt" al nombre ## <Dato externo original>
# source("C:/temp/tabuleRaster.R.txt") ## Cambiar por ruta equivalente ## <Dato externo original>
# source("C:/temp/find_gdal.R.txt") ## Cambiar por ruta equivalente ## <Dato externo original>



# Buscar GDAL en nuestro computador
gdalPaths(help = TRUE)
gdal <- gdalPaths(depth = 3, drives = c('C'), latestQ = FALSE, help = FALSE)

(execGDAL <- gdal$execGDALcalc)

## Definir Ruta de trabajo ----
## 'C:/temp/Ecuador_riparios' Es la carpeta del proyecto. Esta puede SI PUEDE en cada computador
carpeta_trabajo <- 'H:/tempH/Ecuador_riparios/' ####### CAMBIAR ---
dir.create(paste0(carpeta_trabajo, '04_calculoNacional/01_insumos'), recursive = TRUE) ## Crear la carpeta si no existe
file.exists(paste0(carpeta_trabajo, '04_calculoNacional/01_insumos')) ## Debe ser TRUE 


## '04_calculoNacional' es la carpeta de calculo nacional. Esta ruta NO PUEDE cambiar y debe mantenerse igual
root <- paste0(carpeta_trabajo, '/04_calculoNacional/') # Usar / o \\. Cada uno cambia su carpeta_trabajo
dir.create(root) ## Puede generar un warning si ya exsiste. Es sólo un aviso
setwd( root ) # asignar ruta de trabajo
list.dirs(recursive = FALSE) # Revisar carpetas existentes


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', 
               x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )



## Acá se deben ubicar los archivos originales descomprimidos:
paste0(root, '01_insumos/')
list.files(paste0(root, '01_insumos/')) # Archivos actuales

## Lo siguiente debe ser TRUE y no FALSE, Se revisa que existan las capas necesarias. NO puede ser FALSE.
(condicion <- all( 
  c('v_ff010_cobertura_vegetal_1990_aPolygon.shp', # Bosque en SHP
                     'v_ff010_cobertura_vegetal_2000_aPolygon.shp', # Bosque en SHP
                     'v_ff010_cobertura_vegetal_2008_aPolygon.shp', # Bosque en SHP
                     'v_ff010_cobertura_vegetal_2014_aPolygon.shp', # Bosque en SHP
                     'v_ff010_cobertura_vegetal_2016_aPolygon.shp', # Bosque en SHP
                     'v_ff010_cobertura_vegetal_2018_aPolygon.shp', # Bosque en SHP
                     'v_ff010_cobertura_vegetal_2020_aPolygon.shp', # Bosque en SHP
                     'ORGANIZACION TERRITORIAL PROVINCIAL.shp', # Provincias
                     'Rio_l_250K_ECU_IGM.shp', # Rios en lineas
                     'Rio_a_250K_ECU_IGM.shp' # Rios en poligonos
) %in% list.files(paste0(root, '/01_insumos'))
))

## Revisar directorios
list.dirs(path = paste0(carpeta_trabajo, '04_calculoNacional/'), 
          full.names = TRUE, recursive = FALSE)


## Crear directorios
outDirs <- c('01_insumos', '02_rios-buffer', '03_rios-raster',
             '04_bosques-filtrados', '05_bosques-raster', '06_cruce-bosques-riparios')
sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave


## Bosques -----
folder_bosques <- '01_insumos'
(archivos_bosques <- list.files(path = folder_bosques, 
                                pattern = '^v_ff.+shp$', full.names = TRUE))

# Extraemos extent nacional
(info_bosques <- tryCatch(gdalUtils::ogrinfo(archivos_bosques[1], so = TRUE, al = TRUE), error = function(e) NULL))
if(!is.null(info_bosques)){
  (extent_bosques <- grep('Extent', info_bosques, value = TRUE))
  (extent_sin_texto <- gsub('[a-zA-Z]|\\)|\\(|-|:|: |,|^ ', '', extent_bosques) )
  (extent_numerico <- as.numeric(strsplit( gsub('  ', ' ', 
                                                extent_sin_texto), ' ')[[1]] ))
  names(extent_numerico) <- c("xmin", "ymin", "xmax", "ymax")
} else {
  ## En caso de error con la linea 25 gdalUtils::ogrinfo
  (polygon_bosques <- tryCatch( sf::read_sf(archivos_bosques[1]) , error = function(e) NULL))
  (extent_numerico <-  sf::st_bbox(polygon_bosques))
}

## Extent de trabajo
extent_numerico


## Rios ------
ruta_rios <- '01_insumos/Rio_l_250K_ECU_IGM.shp' # < Archivo externo >
file.exists( ruta_rios ) # Debe ser TRUE
ruta_rios_pol <- '01_insumos/Rio_a_250K_ECU_IGM.shp' # < Archivo externo >
file.exists( ruta_rios_pol ) # Debe ser TRUE

# Buffer de 1m a rios en lineas
rios_buffer1m <-  paste0('02_rios-buffer/', 'rioslineasbuffer1m.shp')
if ( ! file.exists(rios_buffer1m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32717',
                           src_datasource_name = ruta_rios, 
                           dst_datasource_name = rios_buffer1m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 1) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(ruta_rios)))
    ) 
  ) # 50
}


# Unir las capas de rios en poligonos y líneas (su buffer)
archivo_rios_poligonos <-  paste0('02_rios-buffer/', 'riospoligonos.shp')
if ( ! file.exists(archivo_rios_poligonos) ){
  
  ## Creamos una capa de rios poligonos igual a la de navegables
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32717',
                           src_datasource_name = ruta_rios_pol, 
                           dst_datasource_name = archivo_rios_poligonos,
                           f = "ESRI Shapefile")
  )
  
  ## Anexamos la capa de buffer de 1m 
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32717',
                           src_datasource_name = rios_buffer1m, 
                           dst_datasource_name = archivo_rios_poligonos,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(archivo_rios_poligonos)),
                           f = "ESRI Shapefile")
  ) # 13
}


## Buffer rios ------
archivo_buffer_30 <-  paste0('02_rios-buffer/', 'buffer30.shp')

if ( ! file.exists(archivo_buffer_30) ){
  print(system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = archivo_rios_poligonos, 
                           dst_datasource_name = archivo_buffer_30,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 30) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_poligonos)) ))
  )) # 30
}


archivo_buffer_100 <-  paste0('02_rios-buffer/', 'buffer100.shp')

if (!file.exists(archivo_buffer_100)){
  print(system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = archivo_rios_poligonos, 
                           dst_datasource_name = archivo_buffer_100,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 100) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_poligonos)) ))
  )) # 30
}



## Crear capa raster de buffers de 30 y 100m

archivo_buffer_30_neg <- paste0('03_rios-raster/', 'buffer30neg.tif')
archivo_buffer_100_neg <- paste0('03_rios-raster/', 'buffer100neg.tif')

archivo_raster_buffer_30 <- paste0('03_rios-raster/', 'buffer30.tif')
archivo_raster_buffer_100 <- paste0('03_rios-raster/', 'buffer100.tif')


if (!file.exists(archivo_buffer_30_neg)){
  
  ## Rasterizar la capa de buffers
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_30, 
                                  dst_filename = archivo_buffer_30_neg,
                                  ot = 'Int16', # Para permitir valores negativos
                                  burn = 1, tr = c(30, 30), init = 0, 
                                  te =  extent_numerico, # xmin ymin xmax ymax
                                  co=c("COMPRESS=DEFLATE"))
  )) # 100seg
  
  ## Quitar los cuerpos permanentes de agua - rasterizar con valores negativos
  ## Si hay error, reemplazar gdalUtils:: con gdalUtilities::
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = ruta_rios_pol,
                              dst_filename = archivo_buffer_30_neg,  
                              add = TRUE, 
                              burn = -1)
  )) # 60seg
  
  ## Convertir en 1 y 0 comprimidos
  print(system.time(
    gdalUtilities::gdal_translate(src_dataset = archivo_buffer_30_neg, 
                                  dst_dataset = archivo_raster_buffer_30, 
                                  ot = 'Byte', # Forza a tener solo 0 y 1
                                  co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 80seg
}

if (!file.exists(archivo_buffer_100_neg)){
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_100, 
                                  dst_filename = archivo_buffer_100_neg, 
                                  ot = 'Int16', # Para permitir valores negativos
                                  burn = 1, tr = c(30, 30), init = 0, 
                                  te =  extent_numerico,
                                  co=c("COMPRESS=DEFLATE"))
  )) # 14seg
  
  ## Quitar los cuerpos permanentes de agua - rasterizar con valores negativos
  gdalUtils::gdal_rasterize(src_datasource = ruta_rios_pol,
                            dst_filename = archivo_buffer_100_neg,  
                            add = TRUE, burn = -1)
  
  ## Convertir en 1 y 0 comprimidos
  gdalUtilities::gdal_translate(src_dataset = archivo_buffer_100_neg, 
                                dst_dataset = archivo_raster_buffer_100, 
                                ot = 'Byte', # Forza a tener solo 0 y 1
                                co=c("COMPRESS=DEFLATE", "NBITS=1"))
}



## Extraer estadísticas ----
(estadisticas_buffer_30m <- tabuleRaster(archivo_raster_buffer_30, n256 = TRUE, del0 = TRUE)) 
(pixeles_zonas_riparias_30 <- subset(estadisticas_buffer_30m, id == 1)$count)

(estadisticas_buffer_100m <- tabuleRaster(archivo_raster_buffer_100, n256 = TRUE, del0 = TRUE)) 
(pixeles_zonas_riparias_100 <- subset(estadisticas_buffer_100m, id == 1)$count)

# raster_buffer_30 <- raster(archivo_raster_buffer_30)
# system.time( starR <- raster::cellStats(raster_buffer_30, sum, na.rm = TRUE) ) 
# 8799976



## Extraer nombres de columnas

todos_nombres_columnas <- lapply(archivos_bosques, function(x){ # x <- archivos_bosques[1]
  dbf.x <- read.dbf( gsub('.shp', '.dbf', x), as.is = TRUE)
  bosque_pos <- unique(unlist(sapply(dbf.x, function(x) {grep('bosque', tolower(x))} )))
  (anio <- gsub('_$|.+__', "", gsub("([^_{0-9, 4}_])", '', basename(x))))
  dbf.y <- cbind.data.frame(yyyy = anio, colnam = colnames(dbf.x), 
                            example = as.vector(t(dbf.x[bosque_pos[1], ])))
})

do.call(rbind, todos_nombres_columnas)


# Consignar los nombres que contienen las columnas con bosque en las diferentes capas
nombres_columnas <- c('cobertura_', 'ctn2')


## Iterar sobre los bosques -----

for( f in 1:length(archivos_bosques)){ # f = 1 # }
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- ', basename(archivos_bosques[f]) ) )
  
  ## Bosques filtrar polígono -----
  (bosque_archivo <- gsub('v_ff010_cobertura_vegetal_', 'BOSQUE_',
                          gsub('_aPolygon', '', basename(archivos_bosques[f]) ) ))
  (bosque_archivo_ruta <- paste0('04_bosques-filtrados/', bosque_archivo ) )
  (anio <- gsub('\\.|[[:alpha:]]|[[:punct:]]', '', bosque_archivo))
  
  #(cmd <- paste0('ogr2ogr -where "\"cobertura_\" = \"BOSQUE\" AND \"cobertura_\" = \"BOSQUE NATIVO\"" -f "ESRI Shapefile" ',outshp, ' ', forests[f]))
  
  print(bosque_archivo_ruta)
  if ( !file.exists(bosque_archivo_ruta) ) {
    
    tabla_dbf_boque <- read.dbf(gsub('.shp', '.dbf', archivos_bosques[f]), as.is = TRUE)
    column_name <- nombres_columnas[nombres_columnas %in% colnames(tabla_dbf_boque)][1]
    # head(tabla_dbf_boque)
    valores_unicos <- unique(tabla_dbf_boque[, column_name])
    (valores_bosque <- grep('BOS', valores_unicos, value = TRUE))
    
    system.time(
      gdalUtilities::ogr2ogr(src_datasource_name = archivos_bosques[f], 
                           dst_datasource_name = bosque_archivo_ruta, f = "ESRI Shapefile",
                           #where = "cobertura_ = 'BOSQUE' AND cobertura_ = 'BOSQUE NATIVO'"
                           #where = "cobertura_ LIKE 'BOSQUE' AND cobertura_ LIKE 'BOSQUE NATIVO'"
                           where = paste0(column_name, " LIKE '", valores_bosque,"'")
      )
    )
  }
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- filtro de bosques ',anio) ) 
  
  
  ## Bosques rasterizar -----
  (bosque_rasterizado <- paste0( '05_bosques-raster/', #  
                                 basename(tools::file_path_sans_ext(bosque_archivo_ruta)), '.tif'))
  if ( !file.exists(bosque_rasterizado) ) {
    
    system.time(
      gdalUtilities::gdal_rasterize(src_datasource = bosque_archivo_ruta, 
                                    dst_filename = bosque_rasterizado, 
                                    ot = 'Byte',
                                    burn = 1, tr = c(30, 30), init = 0,
                                    te =  extent_numerico,
                                    co=c("COMPRESS=DEFLATE", "NBITS=1"))
    ) # 235s
  }
  
  # bos_rast_r <- raster(bosque_rasterizado)
  # plot(bos_rast_r)
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- rasterizar bosques ',anio) )   
  
  
  # Bosque en buffers 30m -----
  (bosque_buffer_30 <- paste0( '06_cruce-bosques-riparios/', 
                               'bosque_ripario30m_', anio , '.tif'))
  
  (bosque_buffer_30_01 <- paste0( '06_cruce-bosques-riparios/',
                                  'bosque_ripario30m_', anio , '_01.tif'))
  
  stack_capas_30m <- c(list(terra::rast(bosque_rasterizado), 
                            terra::rast(archivo_raster_buffer_30)))

  if (!file.exists(bosque_buffer_30)){
    print(system.time({ # 50 mins
      bosques_riparios_30 <-  stack_capas_30m[[1]] & stack_capas_30m[[2]]
      terra::writeRaster(x = bosques_riparios_30, filename = bosque_buffer_30, 
                         overwrite = TRUE, datatype = 'INT1S', 
                         gdal=c("COMPRESS=DEFLATE", "NBITS=1"))
    })) #203.89s
  }
  
  
  ## Crear un archivo con 0 y 1 --------
  if (!file.exists(bosque_buffer_30_01)){
    system.time(
      bosques_riparios_30_01 <- gdalUtilities::gdal_translate(
        stats = TRUE, a_nodata = 999, #para que los 0 no sean guardados como nodata
        src_dataset = bosque_buffer_30, dst_dataset = bosque_buffer_30_01,
        ot = 'Byte', co=c("COMPRESS=DEFLATE", "NBITS=1")))
    # 4S
  }
  
  #raster_bosque30m01 <- raster(bosque_buffer_30_01); plot(raster_bosque30m01)
  
  # Bosque en buffers 100m -----
  (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/',
                                'bosque_ripario100m_', anio , '.tif'))
  
  (bosque_buffer_100_01 <- paste0( '06_cruce-bosques-riparios/', 
                                   'bosque_ripario100m_', anio , '_01.tif'))
  
  stack_capas_100m <- c(list(terra::rast(bosque_rasterizado), 
                            terra::rast(archivo_raster_buffer_100)))
  
  
  if (!file.exists(bosque_buffer_100)){
    print(system.time({ # 
      bosques_riparios_100 <-  stack_capas_100m[[1]] & stack_capas_100m[[2]]
      terra::writeRaster(x = bosques_riparios_100, filename = bosque_buffer_100, 
                         overwrite = TRUE, datatype = 'INT1S', 
                         gdal=c("COMPRESS=DEFLATE", "NBITS=1"))
    })) # 208
    
  }
  
  ## Crear un archivo con 0 y 1 --------
  if (! file.exists(bosque_buffer_100_01) ){
    system.time(
      bosques_riparios_100_01 <- gdalUtilities::gdal_translate(
        stats = TRUE, a_nodata = 999, #para que los 0 no sean guardados como nodata
        src_dataset = bosque_buffer_100, dst_dataset = bosque_buffer_100_01,
        ot = 'Byte', co=c("COMPRESS=DEFLATE", "NBITS=1"))) # 17s
  }
  
  
  #raster_bosque01 <- raster(bosque_rasterizado)
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- Intersectar bosques y zonas riberenas ',anio) )   
  
  archivo_tabla_resumen <- paste0( '06_cruce-bosques-riparios/', 
                                   'estadisticas_bosque_riparios', anio , '.csv') 
  
  
  if(!file.exists(archivo_tabla_resumen)){
    
    ## Estadisticas 30m---
    (estadisticas_ripario_30 <- tabuleRaster(bosque_buffer_30_01, n256 = TRUE, del0 = TRUE)) 
    (pixeles_bosques_riparios_30 <- subset(estadisticas_ripario_30, id == 1)$count)
    
    ## Estadisticas 100m ------
    (estadisticas_ripario_100 <- tabuleRaster(bosque_buffer_100_01, n256 = TRUE, del0 = TRUE)) 
    (pixeles_bosques_riparios_100 <- subset(estadisticas_ripario_100, id == 1)$count)
    
    
    (tabla_resumen <- data.frame(fecha = anio,
                                pix_rip30m = pixeles_zonas_riparias_30,
                                pix_bosrip30m = pixeles_bosques_riparios_30,
                                propbosrip30m = pixeles_bosques_riparios_30 / pixeles_zonas_riparias_30,
                                porcbosrip30m = pixeles_bosques_riparios_30 / pixeles_zonas_riparias_30 * 100,
                                pix_rip10m = pixeles_zonas_riparias_100,
                                pix_bosrip100m = pixeles_bosques_riparios_100,
                                propbosrip100m = pixeles_bosques_riparios_100 / pixeles_zonas_riparias_100,
                                porcbosrip100m = pixeles_bosques_riparios_100 / pixeles_zonas_riparias_100 * 100
    ))
    write.csv(tabla_resumen, file = archivo_tabla_resumen, row.names = FALSE)
  }
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- extraer estadisticas ',anio) )   
}

## Compilar datos de csv -----
(archivos_csv <- list.files(pattern = 'estadisti.+[0-9].csv', 
                            path = '06_cruce-bosques-riparios', 
                            full.names = TRUE))
tabla_compilada <- NULL
for (i in 1:length(archivos_csv)){
  tabla_compilada <- rbind(tabla_compilada , read.csv(archivos_csv[i]))
}

write.csv(tabla_compilada, 
          file = '06_cruce-bosques-riparios/estadisticas_bosque_riparios_compiladas.csv')
write.csv2(tabla_compilada, 
          file = '06_cruce-bosques-riparios/estadisticas_bosque_riparios_compiladas_puntoycoma.csv')


plot(tabla_compilada$fecha, tabla_compilada$propbosrip30m, type = 'b', 
     ylim = c(0, 1), main = 'Proporción de bosques en zonas riparias')
lines(tabla_compilada$fecha, tabla_compilada$propbosrip100m, type = 'b', 
      col = 'green')
legend('topright', legend = c('30m', '100m'), col = c('darkblue', 'darkgreen'), lty = 1)
