## Creado por Ivan Gonzalez [gonzalezgarzonivan@gmail.com]
## Estudiante doctorado en Ecoinformática @ NAU - ig299@nau.edu


## AVISOS: ----
## 1. Reabrir este archivo con codificación UTF8 (File/Archivo - Reopen with Encoding/Reabrir con codificación -- UTF8)
## 2. Todas las líneas de código deben ejecutarse.
## 3. Los análisis pesados no se repiten porque el código detecta que ya existen esos archivos
## 4. Comentarios del texto inicia con dos ##
## 5. Código inhabilitado inicia con un #. Puede removerse con Ctrl+C
## 6. Abrir en QGIS o similar los resultados después de cada paso y así verificar el avance del script
## 7. Los archivos se pueden descargar de esta carpeta: https://drive.google.com/open?id=1KME0tvdK8HxQx6G3A7z7vtvqC88fqyAs&authuser=ig299%40nau.edu&usp=drive_fs
## 8. El algoritmo puede tomar hasta 6H en ejecutarse por el tamaño de los archivos
## 9. Se debe crear una carpeta de trabajo. Ojalá cerca de la raíz y sin espacios. Ej: C:/Peru_riparios/
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
## 'C:/temp/Peru_riparios' Es la carpeta del proyecto. Esta puede SI PUEDE en cada computador
carpeta_trabajo <- 'C:/temp/Peru_riparios/' ####### CAMBIAR ---
dir.create(paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales'), recursive = TRUE) ## Crear la carpeta si no existe
file.exists(paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales')) ## Debe ser TRUE 

## '04_calculoNacional' es la carpeta de calculo nacional. Esta ruta NO PUEDE cambiar y debe mantenerse igual
root <- paste0(carpeta_trabajo, '/04_calculoNacional/') # Usar / o \\. Cada uno cambia su carpeta_trabajo
dir.create(root) ## Puede generar un warning si ya exsiste. Es sólo un aviso
setwd( root ) # asignar ruta de trabajo
list.dirs(recursive = FALSE) # Revisar carpetas existentes


## Acá se deben ubicar los archivos originales descomprimidos:
paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales')

## Bosque_No_Bosque_2020_Raster.zip
## Bosque_No_Bosque_2020_Vector.zip
## Perdida_2001_2020_Raster.zip
## Cuencas.zip
## Cuerpos_agua_10k.zip
## rionavegables.zip
## rios_area_idep_ign_100k_geogpsperu.zip


## Lo siguiente debe ser TRUE y no FALSE, Se revisa que existan las capas necesarias. NO puede ser FALSE.
(condicion <- all( c('Bosque_No_Bosque_2020.tif', # Bosque en tif
                     'Perdida_2001_2020.tif',   # Deforestación en tif
                     'Bosque_No_Bosque_2020_Vector.shp', # Bosque en vector
                     'Bas_PE_1268.shp', # Cuencas
                     'Riv_100000_350k.shp', # Rios en lineas
                     'Rio_navegables.shp', # Rios en poligonos
                     'rios_area_idep_ign_100k_geogpsperu.shp' # Rios en polígonos
) %in% list.files(paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales'))))

## Evaluar si tenemos las condiciones para correr el script
if( !condicion){
  stop(paste0('ERROR: Copie los archivos mencionados a la carpeta: ---',
              paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales')
              , '---'))
}


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





## Límite nacional -----

extent_nacional <- raster('01_datos-originales/Bosque_No_Bosque_2020.tif') # < Archivo externo >
extent_numerico <- extent_nacional@extent[c(1,3,2,4)] # xmin ymin xmax ymax


# ## Rasterizaremos la capa de cuencas del país para saber el 100% de la zonas riparias
# raster_limite_nacional <- paste0('01_datos-originales/', 'mascara_30m.tif')
# if (!file.exists(raster_limite_nacional)){
#   print(system.time(
#     gdalUtilities::gdal_rasterize(src_datasource = '01_datos-originales/Bas_PE_1268.shp',
#                                   dst_filename = raster_limite_nacional,
#                                   ot = 'Byte',
#                                   burn = 1, tr = c(30, 30), init = 0,
#                                   te =  extent_numerico,
#                                   co=c("COMPRESS=DEFLATE", "NBITS=1"))
#   )) # 400seg
# }


## 3. Crear buffer de ríos en zonas riparias + eliminar superficies de agua ----------------
## Rios ------
ruta_rios <- '01_datos-originales/Riv_100000_350k.shp' # < Archivo externo >
file.exists( ruta_rios ) # Debe ser TRUE
ruta_rios_pol_1 <- '01_datos-originales/Rio_navegables.shp' # < Archivo externo >
file.exists( ruta_rios_pol_1 ) # Debe ser TRUE
ruta_rios_pol_2 <- '01_datos-originales/rios_area_idep_ign_100k_geogpsperu.shp' # < Archivo externo >
file.exists( ruta_rios_pol_2 ) # Debe ser TRUE


## Unir rios que son navegables o areas
rios_navegables <-  paste0('02_rios-buffer/', 'riosnavegables.shp') 

if ( ! file.exists(rios_navegables) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32718',
                           src_datasource_name = ruta_rios_pol_1, overwrite = TRUE, 
                           dst_datasource_name = rios_navegables,
                           f = "ESRI Shapefile")
  ) # 1seg
  
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32718',
                           src_datasource_name = ruta_rios_pol_2, 
                           dst_datasource_name = rios_navegables,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(rios_navegables)),
                           f = "ESRI Shapefile")
  ) # 1seg
}


## Crear buffer de 1m en capa de ríos tipo línea para unir a la capa creada anteriormente
## Revisar tipo de líneas en la capa de rios
# rios_lineas_tabla <- foreign::read.dbf('01_datos-originales/Riv_100000_350k.dbf') # < Archivo externo >
# table(rios_lineas_tabla$TIPO)

rios_buffer1m <-  paste0('02_rios-buffer/', 'rioslineasbuffer1m.shp')
if ( ! file.exists(rios_buffer1m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32718',
                           src_datasource_name = ruta_rios, 
                           dst_datasource_name = rios_buffer1m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 1) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(ruta_rios)),
                                        " WHERE TIPO!='LL'") ## where = "TIPO!='LL'" # <>
    ) 
  ) # 244.75 
}


# Unir las capas de rios en poligonos y líneas (su buffer)
archivo_rios_poligonos <-  paste0('02_rios-buffer/', 'riospoligonos.shp')
if ( ! file.exists(archivo_rios_poligonos) ){
  
  ## Creamos una capa de rios poligonos igual a la de navegables
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32718',
                           src_datasource_name = rios_navegables, 
                           dst_datasource_name = archivo_rios_poligonos,
                           f = "ESRI Shapefile")
  )
  
  ## Anexamos la capa de buffer de 1m 
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32718',
                           src_datasource_name = rios_buffer1m, 
                           dst_datasource_name = archivo_rios_poligonos,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(archivo_rios_poligonos)),
                           f = "ESRI Shapefile")
  ) # 158.85
}




## Buffer rios ------
## Crear capa vector con buffers de 30 y 10m
archivo_buffer_30 <-  paste0('02_rios-buffer/', 'buffer30.shp')

if ( ! file.exists(archivo_buffer_30) ){
  system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = archivo_rios_poligonos, 
                           dst_datasource_name = archivo_buffer_30,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 30) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_poligonos)) ))
  ) # 261.81
}

archivo_buffer_100 <-  paste0('02_rios-buffer/', 'buffer100.shp')
if (!file.exists(archivo_buffer_100)){
  system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = archivo_rios_poligonos, 
                           dst_datasource_name = archivo_buffer_100,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 100) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_poligonos)) ))
  ) # 270
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
                                  burn = 1, tr = c(30, 30), init = 0, 
                                  te =  extent_numerico, # xmin ymin xmax ymax
                                  co=c("COMPRESS=DEFLATE"))
  )) # 715seg
  
  ## Quitar los cuerpos permanentes de agua - rasterizar con valores negativos
  print(system.time(
    gdalUtils::gdal_rasterize(src_datasource = rios_navegables,
                              dst_filename = archivo_buffer_30_neg,  
                              add = TRUE, burn = -1)
  )) # 420seg
  
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
                                  ot = 'Int8', # Para permitir valores negativos
                                  burn = 1, tr = c(30, 30), init = 0, 
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
system.time( estadisticas_buffer_30m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_30, stats = TRUE, checksum = TRUE) ) )

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
system.time( estadisticas_buffer_100m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_100, stats = TRUE, checksum = TRUE) ))

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

## Extraer máscara vectorial de zonas con bosque para filtrar
## polígonos y extraer ventana donde recortaremos los tifs nacionales y así
## reducir el tamaño de las capas a analizar en fragmentación, para así ahorrar tiempo 
## de procesamiento y almacenamiento 

## El siguiente procedmiento filtra la capa vectorial de polígonos de bosque
## pero se demora bastante (1h) -- no usaremos esta opción

archivo_vect_bosque <- '01_datos-originales/Bosque_No_Bosque_2020_Vector.shp' # < Archivo externo >
file.exists(archivo_vect_bosque)

archivo_vect_bosque_filtrado <-  paste0('04_bosques-filtrados/', 'vect_bosques_filtrados.shp')

if (!file.exists(archivo_vect_bosque_filtrado)){
  print(system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = archivo_vect_bosque,
                           dst_datasource_name = archivo_vect_bosque_filtrado,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           where = paste0("Cobertura!='No Monitoreado'") )
    
  )) #  3593.69s = 1h
}



## Extraer la extensión de las zonas donde hay bosque en el pais
(info_bosques <- gdalUtils::ogrinfo(archivo_vect_bosque_filtrado, so = TRUE, al = TRUE)) # 
(extent_bosques <- grep('Extent', info_bosques, value = TRUE))
(extent_sin_texto <- gsub('[a-zA-Z]|\\)|\\(|:|: |,|^ ', '', gsub('\\) - \\(', ' ', extent_bosques) ))
(extent_numerico <- as.numeric(strsplit(
  gsub('  ', ' ', extent_sin_texto), ' ')[[1]] )) # orden del resultado: xmn ymn xmx ymx
names(extent_numerico) <- c('xmn', 'ymn', 'xmx', 'ymx')
extent_numerico


## En caso de error con la funcion "gdalUtils::ogrinfo". No correr si extent_numerico funciona
# (info_bosques <- rgdal::ogrInfo(archivo_vect_bosque_filtrado)) # Demora 5mins
# (extent_numerico <- info_bosques$extent)


## Habilitar si tuvimos error en el paso anterior
# extent_numerico <- c( -2792, 8387100, 1190998, 9995730)  ## (xmin, xmax, ymin, ymax)


## Convertir capa de bosques entre 0 y 1 con bosques a 2000
archivo_bosques <- '01_datos-originales/Bosque_No_Bosque_2020.tif' # < Archivo externo >
archivo_perdida <- '01_datos-originales/Perdida_2001_2020.tif' # < Archivo externo >
file.exists(archivo_bosques)
file.exists(archivo_perdida)

archivo_bosques_corte <- '04_bosques-filtrados/Bosque_No_Bosque_2020_corte.tif'
archivo_perdida_corte <- '04_bosques-filtrados/Perdida_2001_2020_corte.tif'

if(!file.exists(archivo_bosques_corte)){
  print(system.time(
    gdalUtilities::gdalwarp(srcfile = archivo_bosques, 
                            dstfile = archivo_bosques_corte, 
                            te = extent_numerico + c(-60, -60, 60, 60), # xmin ymin xmax ymax
                            #cutline = archivo_vect_bosque_filtrado_bbox, # Usar poligono de la capa 
                            #crop_to_cutline = TRUE, # Cortar por el polígono
                            ot = 'Byte', co = c("NBITS=8", "COMPRESS=DEFLATE")
    )
  )) # 434s
}

if(!file.exists(archivo_perdida_corte)){
  print(system.time(
    gdalUtilities::gdalwarp(srcfile = archivo_perdida, 
                            dstfile = archivo_perdida_corte, 
                            te = extent_numerico + c(-60, -60, 60, 60), # xmin ymin xmax ymax
                            # cutline = archivo_vect_bosque_filtrado_bbox, # Usar poligono de la capa 
                            # crop_to_cutline = TRUE, # Cortar por el polígono
                            ot = 'Byte', co = c("NBITS=8", "COMPRESS=DEFLATE"),
                            dstnodata = 0
    )
  )) # 
}



## Convertir capa de bosques entre 0 y 1 con bosques a 2000
archivo_bosques <- archivo_bosques_corte 
archivo_perdida <- archivo_perdida_corte 
file.exists(archivo_bosques)
file.exists(archivo_perdida)



archivo_inicial_0 <- '04_bosques-filtrados/Bosque_inicial2000.tif'
if (! file.exists(archivo_inicial_0) ){
  if (!GDAL){
    ## Opcion R
    print(system.time(
      if (! file.exists(archivo_inicial_0) ){
        capa_bosque <- raster(archivo_bosques)
        bosque_inicial2000 <- Which(capa_bosque == 3 | capa_bosque == 5)
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
               ' --calc="0 + (1*(logical_or(A==3, A==5)))" ',
               '-A ', root2, '\\', archivo_bosques_corte,
               ' --outfile=', root2, '\\', archivo_inicial_0,
               ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
        )))) # 
  }
}


## Cambiar datos nullos a 0 en capa raster de perdida de bosque
archivo_perdida_0 <- '04_bosques-filtrados/Perdida_2001_2020_llenado0.tif'
if (! file.exists(archivo_perdida_0) ){
  print(system.time(
    capa_perdida_0 <- gdalUtilities::gdal_translate(a_nodata = 999, # dato que se usa como nodata original 
                                                    src_dataset = archivo_perdida_corte, 
                                                    dst_dataset = archivo_perdida_0,
                                                    ot = 'Byte',
                                                    co=c("COMPRESS=DEFLATE", "NBITS=1"))) # Esta opción permite reducir de 2GB a 15MB
  ) # 21
}

## Cortar capa de zonas riparias a zonas forestales ----
## Capa raster buffer de 30m
buffer30_recorte <- '03_rios-raster/buffer30_recortado.tif'
if (! file.exists(buffer30_recorte) ){
  print(system.time(
    gdalUtilities::gdalwarp(srcfile = archivo_raster_buffer_30, 
                            dstfile = buffer30_recorte, 
                            te = extent_numerico + c(-60, -60, 60, 60), # xmin ymin xmax ymax
                            ot = 'Byte', co = c("NBITS=8", "COMPRESS=DEFLATE"),
                            dstnodata = 999 ## Para que los 0 sean datos y no "NoData"
    )
  )) # 80s
}

## Capa raster buffer de 100m
buffer100_recorte <- '03_rios-raster/buffer100_recortado.tif'
if (! file.exists(buffer100_recorte) ){
  print(system.time(
    gdalUtilities::gdalwarp(srcfile = archivo_raster_buffer_100, 
                            dstfile = buffer100_recorte, 
                            te = extent_numerico + c(-60, -60, 60, 60), # xmin ymin xmax ymax
                            ot = 'Byte', co = c("NBITS=8", "COMPRESS=DEFLATE"),
                            dstnodata = 999 ## Para que los 0 sean datos y no "NoData"
    )
  )) # 80s
}



## Cargar capas raster en R en caso que debamos operar en R los cálculos
raster_inicial_0 <- raster(archivo_inicial_0)
raster_perdida_0 <- raster(archivo_perdida_0)



## 6. Derivar capas anuales de bosque año a año ----------------

## Extraer fechas de perdida de bosque sobre la cual iterar. Dejaremos desde 2000 a 2020
raster_fechas <- raster(archivo_perdida)
# fechas_unicas <- unique(raster_fechas) # 400 seg
# fechas_perdida <- unique(raster_fechas[])
# fechas_perdida <- sort(na.omit(fechas_perdida))
fechas_unicas <- (0:21)


## Definir si se deben guardar los pasos intermedios. La variable "pasos_intermedios" debe ser TRUE o FALSE.
## Dejar en FALSE si se debe saltar el paso de bosques nacionales de 
## la carpeta "04_calculoNacional/05_bosques-raster/" y solo escribir los
## datos finales en la carpeta "04_calculoNacional/06_cruce-bosques-riparios/". Ahorra 15mins approx por cada año trabajado
pasos_intermedios <- FALSE

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
        ## Heho con gdal_calc.py
        print(system.time( 
          system(intern = TRUE,
                 paste0(execGDAL,
                        ' --calc="A-(1*(logical_and(B!=0,B<=', anio2, ')))" ',
                        '-A ', root2, '\\', archivo_inicial_0,' -B ', root2, '\\',archivo_perdida_0, 
                        ' --outfile=', root2, '\\', nombre_raster_bosque,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 ) )
        )) # 1713.92s - 2000s
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
    (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/',
                                  'bosque_ripario100m_', anio , '.tif'))
    
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
    
  } else {
    
    ## 6 + 7. SIMPLIFICADO: Cruzar capa de bosques y pérdida con zonas riparias año a año ----------------
    ## Omitr pasos intermedios. Escribir resultados directamente en la carpeta "04_calculoNacional/06_cruce-bosques-riparios/"
    ## y no escribir nada en "04_calculoNacional/05_bosques-raster/"
    
    # Bosque en buffers 30m -----
    (bosque_buffer_30 <- paste0( '06_cruce-bosques-riparios/', 'bosque_ripario30m_', anio , '.tif'))
    print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Intersectar bosques y zonas riberenas ',anio, ' 30m -') )   
    
    if (!file.exists(bosque_buffer_30)){
      if (! GDAL){
        ## Hecho en R
        stack_capas_30m <- raster::stack(archivo_inicial_0, archivo_perdida_0, buffer30_recorte)
        print(system.time(
          {resultado_raster_riparios_30 <- stack_capas_30m[[1]] * ( stack_capas_30m[[2]] > anio2 | stack_capas_30m[[2]] == 0  )* stack_capas_30m[[3]] ;
          writeRaster(x = resultado_raster_riparios_30, filename = bosque_buffer_30, 
                      overwrite = TRUE, datatype = 'LOG1S', options=c("COMPRESS=DEFLATE", "NBITS=1"))}
        )) # 
        
      } else {
        ## Heho con gdal_calc.py
        print(system.time( 
          system(intern = TRUE,
                 paste0(execGDAL,
                        ' --calc=" A * logical_or(B>', anio2, ',B==0) * C" ', 
                        ' -A ', root2, '\\', archivo_inicial_0, ' -B ', root2, '\\',archivo_perdida_0, ' -C ', root2, '\\',buffer30_recorte, 
                        ' --outfile=', root2, '\\', bosque_buffer_30,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 ) )
        )) # 2000s - 2722.67
      }
    }
    
    # Bosque en buffers 100m -----
    (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/', 'bosque_ripario100m_', anio , '.tif'))
    print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Intersectar bosques y zonas riberenas ',anio, ' 100m -') )   
    
    if ( !file.exists(bosque_buffer_100) ){
      
      if (!GDAL){
        ## Opcion R
        stack_capas_100m <- raster::stack(archivo_inicial_0, archivo_perdida_0, buffer100_recorte)
        print(system.time(
          {resultado_raster_riparios_100 <- stack_capas_100m[[1]] * ( stack_capas_100m[[2]] > anio2 | stack_capas_100m[[2]] == 0  )* stack_capas_100m[[3]] ;
          writeRaster(x = resultado_raster_riparios_100, filename = bosque_buffer_100, 
                      overwrite = TRUE, datatype = 'LOG1S', options=c("COMPRESS=DEFLATE", "NBITS=1"))}
        )) # 
        
      } else {
        ## Heho con gdal_calc.py
        print(system.time( 
          system(intern = TRUE,
                 paste0(execGDAL,
                        ' --calc=" A * ( (B>', anio2, ')+ (B==0)) * C" ',
                        ' -A ', root2, '\\', archivo_inicial_0, ' -B ', root2, '\\',archivo_perdida_0, ' -C ', root2, '\\',buffer100_recorte, 
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

write.csv(x = tabla_compilada, row.names = FALSE,
          file = paste0('06_cruce-bosques-riparios/',
                        'Consolidado_estadisticas_bosque_riparios_compiladas_',
                        Sys.Date(), '.csv'))

## Pasos para modificar el script con nuevos datos
# 1. Actualizar los archivos en la carpeta;
paste0(carpeta_trabajo, '04_calculoNacional/01_datos-originales')
# 2. Mantener los nombres de las capas de ser posible en los nuevos
# archivos. De lo contrario cambiar los nombres en las líneas con 
# el el comentario "# < Archivo externo >"
