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
## + 3 Capa de limites nacionales (vector tipo pol[í]gono) de Colombia, Ecuador, Per[u]
##    - Peru: http://geo2.ana.gob.pe:8080/geonetwork/srv/spa/catalog.search#/metadata/c7ff4df5-98f2-4607-9284-2c4b67bffa63
##    - Ecuador: https://www.geoportaligm.gob.ec/portal/index.php/descargas/cartografia-de-libre-acceso/
##    - Colombia: https://www.dane.gov.co/files/geoportal-provisional/index.html

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
root <- 'C:/temp/Colombia_fragmentacion/' ####### CAMBIAR --- # Usar / o \\. Cada uno cambia su carpeta_trabajo
dir.create(paste0(root, '01_datos-originales'), recursive = TRUE) ## Crear la carpeta si no existe
file.exists(paste0(root, '01_datos-originales')) ## Debe ser TRUE 

## 
setwd( root ) # asignar ruta de trabajo
list.dirs(recursive = FALSE) # Revisar carpetas existentes

## Cargar funcion para calcular histogramas de rasters
check <- source('I:/Mi unidad/Datos-NASA-ODS15/Talleres/SDG_fragmentation_PEC/R_99_tabuleRaster.R') # # < Archivo externo >

## O podemos traer la funcion desde GitHub directamente
devtools::source_url("https://github.com/gonzalezivan90/SDG15_indicators/blob/main/scripts/R_99_tabuleRaster.R")



## Acá se deben ubicar los archivos originales descomprimidos:
paste0(root, '/01_datos-originales')



## Los siguientes archivos son necesarios para ejecutar la totalidad del script

inputs <-  c('hansen_pec_tree_-0000065536-0000000000.tif', # % de dosel en tif
             'hansen_pec_tree_-0000000000-0000000000.tif', # % de dosel en tif
             'hansen_pec_loss_-0000065536-0000000000.tif', # Deforestación en tif
             'hansen_pec_loss_-0000000000-0000000000.tif', # Deforestación en tif
             'mspa_lin64'                                  # Ejecutable de linux
)

condicion <- inputs %in% list.files(paste0(root, '/01_datos-originales'))

## Lo siguiente debe ser TRUE y no FALSE, Se revisa que existan las capas necesarias. NO puede ser FALSE.
all(condicion)


## Evaluar si tenemos las condiciones para correr el script
if( !all(condicion)){
  stop(paste0('ERROR: Copie los archivos mencionados a la carpeta: --- ',
              paste0(root, '/01_datos-originales')
              , ' ---'))
}


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
root2 <- paste0(paste0(root2, collapse = '\\'), '\\')


## Crear carpetas intermedias donde se guardan los archivos
outDirs <- c('02_bosques-filtrados', '03_bosques-anuales', 
             '04_bosques-reclasificados', '05_resultados-fragmentacion')
sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings si ya existen las carpetas. No es grave

## Copiar el ejecutable desde la carpeta de datos orignales a los folders donde 
## se puede ejecutar GUIDOS desde linux. Este paso no se necesita si se ejecutaran
## en el sistema operativo Windows

file.copy('01_datos-originales/mspa_lin64', './mspa_lin64')
file.copy('01_datos-originales/mspa_lin64', '04_bosques-reclasificados/mspa_lin64')
file.copy('01_datos-originales/mspa_lin64', '05_resultados-fragmentacion/mspa_lin64')


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

## No usaremos GDAL porque R es mas rapido
(GDAL <- FALSE)



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

## Límite regional
archivo_bosques <- '01_datos-originales/BOSQUE_1990.tif' # < Archivo externo >
file.exists(archivo_bosques) # Debe ser TRUE

raster_regional <- raster(archivo_bosques) # < Archivo externo >
extent_numerico <- raster_regional@extent[c(1,3,2,4)] # xmin ymin xmax ymax
# extent_numerico <- c(-81.34127, -18.39948, -66.78425,  12.49268)
ancho_celda <- res(raster_regional)
# ancho_celda <- c(0.0002694946, 0.0002694946)
(proyecc <- gsub(' |ID|[[:punct:]]', '', strsplit(grep('EPSG', gdalUtils::gdalinfo(archivo_bosques), value = TRUE),',')[[1]]))
EPSG <- paste0(proyecc, collapse = ':')

## Combinar las capas de limites terrestres (excluyendo islas) de la region de estudio
ruta_limites_Co <- '01_datos-originales/CO_MGN_ANM_DPTOS.shp' # < Archivo externo >
file.exists( ruta_limites_Co ) # Debe ser TRUE

capa_limites <-  paste0('02_bosques-filtrados/', 'limites_terrestres_region.shp') 

if ( ! file.exists(capa_limites) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = '01_datos-originales/extent_referencia.prj',
                           src_datasource_name = ruta_limites_Co, 
                           where = "DPTO_CCDGO!='88'", # Eliminar Galapagos
                           dst_datasource_name = capa_limites,
                           f = "ESRI Shapefile")
  ) # 1seg
}


## Rasterizar la capa de limites para conocer el numero total de pixeles 
raster_limites <-  paste0('02_bosques-filtrados/', 'limites_terrestres_region.tif') 


if ( ! file.exists(raster_limites) ){
  print(system.time(gdalUtilities::gdal_rasterize(src_datasource = capa_limites, 
                                dst_filename = raster_limites,  
                                ot = 'Byte',
                                burn = 1, tr = ancho_celda, init = 0,
                                # where = # Todala region
                                te =  extent_numerico,
                                co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 120  seg
}


system.time(pixeles_totales <- tabuleRaster(raster_limites, n256 = TRUE, del0 = TRUE)) # Calcular histogramas
pixeles_terrestres <- pixeles_totales$count[pixeles_totales$id == 1] 

## 5. Extraer limite forestal - recortar el raster nacional - capa vectorial ----------------

## Convertir capa de bosques entre 0 y 1 con bosques a 2000
archivo_bosques <- '01_datos-originales/hansen_tree.tif' # < Archivo externo >
archivo_perdida <- '01_datos-originales/hansen_loss.tif' # < Archivo externo >
file.exists(archivo_bosques) # Debe ser TRUE
file.exists(archivo_perdida) # Debe ser TRUE


## Generar capa con pixeles con bosque a 2000
archivo_inicial_0 <- '02_bosques-filtrados/Bosque_inicial2000.tif'
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
archivo_perdida_0 <- '02_bosques-filtrados/Perdida_2001_2020_llenado0.tif'
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


## Iterar sobre los bosques ----
## Las siguientes intrucciones se repetiran para cada año entre 2000 y 2020
for( f in (1:length(fechas_unicas))){ # f = 2 # }
  
  ## Definir valores en cada paso, como anio y nombres de capas
  (anio <- 2000+fechas_unicas[f]) ## entre 2000 y 2020
  anio2 <- fechas_unicas[f] ## entre 0 y 20
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas),
              ' -- anio ', anio ) )
  (nombre_raster_bosque <- paste0('03_bosques-anuales', '/bosqueFecha_', anio, '.tif' ))
  
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
  
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Reclasificar bosques entre 1 y 2 ',anio) )   
  
  (nombre_raster_bosque_12 <- paste0('04_bosques-reclasificados', '/bosqueFecha_', anio, '.tif' ))
  
  if (!file.exists(nombre_raster_bosque_12) & 
      !file.exists(gsub('.tif$', '.txt', nombre_raster_bosque_12))){
    
    # Los mapas deben tener: 1 para no bosque, 2 para bosque, TIF tipo Byte [0, 255]
    # https://ies-ows.jrc.ec.europa.eu/gtb/GTB/MSPA_Guide.pdf
    
    write(file = gsub('.tif$', '.txt', nombre_raster_bosque_12), x = 1)

    if (! GDAL){
    
    ## Opcion R
      print(system.time({ # 
        
        ## Opcion lenta. Guarda un archivo temporal y luego lo transcribe.
        # raster_bosque <- raster(nombre_raster_bosque)
        # raster_clasificado <- raster_bosque + 1
        # # Este archivo raster ya esta en el equipo en la ruta tempdir(), con el nombre raster_clasificado@file@name
        # # Escribir el archivo en formato TIF
        # writeRaster(x = raster_bosque, filename = nombre_raster_bosque_12, overwrite = TRUE, 
        #             datatype = 'INT1U', options=c("COMPRESS=DEFLATE", "NBITS=2"))
        
        ## Opcion rapida. Escribe directamente el resultado como le pedimos
        raster_bosque <- raster(nombre_raster_bosque)
        calc(raster_bosque, fun = function(x) x + 1 , filename = nombre_raster_bosque_12, overwrite = TRUE,
             forcefun=FALSE, forceapply=FALSE, datatype = 'INT1U', options=c("COMPRESS=DEFLATE", "NBITS=8"))
      })) # 1203.78 
        

      } else {
        # Opcion GDAL. Mas lenta que R
        print(system.time(
          system(intern = TRUE,
                 paste0(execGDAL,
                        ' --calc="A+1" ',
                        '-A ', root2, '\\', nombre_raster_bosque,
                        ' --outfile=', root2, '\\', nombre_raster_bosque_12,
                        ' --type=Byte --NoDataValue=999 --co="NBITS=8" --co="COMPRESS=DEFLATE" --quiet'
                 )))) # 2400s
      }
    
  }
}


## 6. Ejecutar el algoritmo de fragmentacion GUIDOS MSPA ----------------

## WINDOWS
## Si los usuarios van a ejecutar el algoritmo en Windows, este se requiere hacer de manera manual
## como se pueden ver en los videos de capacitacion. El resultado en Windows no es optimo, sin embargo 
## es la plataforma mas comun.
## Los resultados de cada capa reclasificada en la carpeta '04_bosques-reclasificados' deben 
## llamarse fragmentacion_2000.tif, fragmentacion_2001.tif, etc. y almacenarse en la carpeta 
## '05_resultados-fragmentacion'


## LINUX
## Si se desean hacer los analisis en Linux, se pueden realizar de dos formas:
## A: Generando los comandos en R (este script) y pegarlos en la consola de Linux (1 comando por capa), o
## B: Correr un script de R en Linux para que se ejecuten los analisis de manera automatica.

## Opcion A:
## Copiar los siguientes comandos resultantes en la consola de R y pegarlos en la consola de lunix 
## asegurandose que el navegador se encuentra en la carpeta donde se encuentran los 
## bosques reclasificados: el equivalente a C:/temp/SDG_fragmentacion_PEC/04_bosques-reclasificados

  
(bosques_reclasificados <- list.files(path = '04_bosques-reclasificados', pattern = '[0-9].tif'))
for (b in 1:length(bosques_reclasificados)){ # b = 1
  archivo_bosque <- paste0('04_bosques-reclasificados/', bosques_reclasificados[b])
  resultado_fragmentacion <- paste0('05_resultados-fragmentacion/', 
                                    gsub('bosqueFecha', 'fragmentacion', bosques_reclasificados[b])) #Crear nombre de capa final
  # Validar que exista el input y no exista el resultado, y sea un archivo valido
  if(!file.exists(resultado_fragmentacion) & file.exists(archivo_bosque) 
     & (file.size(archivo_bosque)/1000/1000 > 50)){ 
    cmd <- paste0('time ./mspa_lin64 -i ', archivo_bosque,
                  ' -o ', resultado_fragmentacion, ' -eew 34\n') # Crear comando de ejecución de GUIDOS
    # Ver comando. Copiar resultado y pegarlo en la carpeta de linux '04_bosques-reclasificados' y
    # que debe tener el ejecutable mspa_lin64
    cat(cmd) 

    ## Ejecutar el comando si se esta en un ambiente Linux
    if (any(grep('Linux', Sys.getenv('OS')))){
      system(cmd)
    }
  }
}

## Opcion B:

## Se puede habilitar el siguiente comando dentro de R para que se creen las instrucciones para 
## ejecutar en linux

# cd /mnt/c/temp/SDG_fragmentation_PEC/ # En linux, ir a la carpeta de trabajo
# R # En Linux, al interior de la carpeta del proyecto, ejecutar R
# getwd() # En R, validar la carpeta de trabajo
# root = '/mnt/c/temp/SDG_fragmentation_PEC/' # En R, crear variable con ruta del proyecto
# setwd(root) # En R, asignar ruta de trabajo
# getwd() # En R, validar la carpeta de trabajo
# list.files(path = '04_bosques-reclasificados/') # Listar archivos
# tifs <- list.files(pattern = '.tif$', path = '04_bosques-reclasificados/') # Crear lista de tifs
# 
# 
# for (i in 1:length(tifs)){ # Iterar sobre cada archivo
#   (inname = paste0('04_bosques-reclasificados/', tifs[i])) # Crear nombre de la capa original
#   (anio <- (gsub('[a-zA-Z]|[[:punct:]]', '', tifs[i]))) # Extraer el ano
#   (outname = paste0('05_resultados-fragmentacion/fragmentacion_', anio, '.tif')) #Crear nombre de capa final
#   #Validar que exista el input y no exita el resultado
#   if(!file.exists(outname) & file.exists(inname) & (file.size(inname)/1000/1000 > 50)){
#     #cmd = paste0(root, 'mspa_lin64 -i ', root, inname,' -o ', root, outname)
#     cmd = paste0('time ./mspa_lin64 -i ', inname,' -o ', outname, ' -eew 34 \n') # Crear comando de ejecución de GUIDOS
#     cat(cmd) # Ver comando
#     #system(cmd) # Pedir al sistema que ejecute el comando
#   }
# }



## 7. Extraer bosque nucleo ----------------
## Se extraeran solamente las categorías de bosque nucleo de las capas generadas por el
## algoritmo MSPA de GUIDOS.





## 6+7. Alternativa: Identificar los bosques nucleo usando distancia al borde ----------------
## Para esto se identificaran los bosques en una distancia menor a 1km, o 34 pixeles de 30m
## de lado. Se extraeran solamente aquellos pixeles del nucelo

## Ubicar el ejecutable de gdal_proximity
execGDALproxy <- gsub(x = execGDAL, 'gdal_calc', 'gdal_proximity')
(gdalproxy <- capture.output(system(paste0(execGDALproxy, ' --help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
(GDAL2 <- ifelse(any(grep('gdal_proximity.py', gdalproxy)), TRUE, FALSE))

## El resultado de GDAL2 debe ser TRUE, de lo contrario se usara la version
## interna de R que es mucho mas lenta

(bosques_reclasificados <- list.files(path = '04_bosques-reclasificados', pattern = '[0-9].tif$'))

for( b in 1:length(bosques_reclasificados)){ # b = 1}
  archivo_bosque <- paste0('04_bosques-reclasificados/', bosques_reclasificados[b])
  archivo_nucelos <- paste0('05_resultados-fragmentacion/', 
                            gsub('bosqueFecha', 'fragmentacion', bosques_reclasificados[b])) #Crear nombre de capa final
  
  # Validar que no exista el resultado
  if(!file.exists(archivo_nucelos) ){
    if ( GDAL2){
      # Opcion GDAL. 
      print(system.time(
        system(intern = TRUE,
               paste0(execGDALproxy,
                      root2, '\\', gsub( '/', '\\', archivo_bosque, fixed = TRUE), " ",
                      root2, '\\', gsub('/', '\\', archivo_nucelos, fixed = TRUE),
                      ' -distunits PIXEL -values 0 -nodata 36 -ot Byte -fixed-buf-val 0 -maxdist 35 -co NBITS=1 -co COMPRESS=DEFLATE'
               )
               ))) # 2400s
    }
  }
}


## 8. Calcular estadisticas ----------------
## Se hace el conteo de pixeles en nucleos de las categorias 1 y 17 (o solo 1 se hace con GDAL)

(capas_fragmentacion <- list.files(path = '05_resultados-fragmentacion/', pattern = '[0-9].tif$'))

for( b in 1:length(capas_fragmentacion)){ # b = 5}
  
  (anio <- gsub('[a-z]|[[:punct:]]', '', capas_fragmentacion[b]))
  archivo_salida <- paste0('05_resultados-fragmentacion/estadisticas_', anio,'.csv')
  
  if(!file.exists(archivo_salida)){
    
    archivo_bosque <- paste0('03_bosques-anuales/BOSQUE_', anio,'.tif')
    archivo_fragmentacion <- paste0('05_resultados-fragmentacion/fragmentacion_', anio,'.tif')
    
    ## Iterar y extraer conteo de pixeles en bosque
    conteo_pixeles_bosque <- tabuleRaster(archivo_bosque, n256 = TRUE, del0 = TRUE) # Calcular histogramas
    pixeles_bosque <- subset(conteo_pixeles_bosque, id == 1 & count != 0)$count # change 2
    
    ## Iterar y extraer conteo de pixeles en bosque
    conteo_pixeles_nucleo <- tabuleRaster(archivo_fragmentacion, n256 = TRUE, del0 = TRUE) # Calcular histogramas
    pixeles_nucleo <- subset(conteo_pixeles_nucleo, id %in% c(1, 17) & count != 0)$count
    
    ## Crear tabla final y escribirla
    (tabla_resumen <- data.frame(fecha = anio,
                                 pix_totales = pixeles_terrestres,
                                 pix_bosque = pixeles_bosque,
                                 pix_nucleo = pixeles_nucleo,
                                 prop_bosque = pixeles_bosque / pixeles_terrestres,
                                 prop_nucleo = pixeles_nucleo / pixeles_terrestres,
                                 prop_bosque_nucleo = pixeles_nucleo / pixeles_bosque,
                                 porc_bosque = pixeles_bosque / pixeles_terrestres * 100,
                                 porc_nucleo = pixeles_nucleo / pixeles_terrestres * 100,
                                 porc_bosque_nucleo = pixeles_nucleo / pixeles_bosque * 100
    ))
    
    #write.csv(tabla_resumen, file = archivo_estadisticas, row.names = FALSE) # Escribir separado por comas
    write.csv2(tabla_resumen, file = archivo_salida, row.names = FALSE) # Escribir separado por punto y comas
  }
    print(paste(' ---- Capa ', b, ' de ', length(capas_fragmentacion), ' -- extraer estadisticas ',anio) )   
}



## 9. Compilar resultados y generación de tabla final ----------------
## Compilar datos de csv -----
archivos_csv <- list.files(pattern = '^estadisticas_.+[0-9].csv', 
                           path = '05_resultados-fragmentacion/', 
                           full.names = TRUE)
tabla_compilada <- NULL
for (i in 1:length(archivos_csv)){
  #print((archivos_csv[i]) )
  #print(read.csv2(archivos_csv[i]) )
  tabla_compilada <- rbind(tabla_compilada , read.csv2(archivos_csv[i]) )
}

write.csv(x = tabla_compilada, row.names = FALSE,
          file = paste0('05_resultados-fragmentacion/',
                        'Consolidado_estadisticas_fragmentacion_compiladas_',
                        Sys.Date(), '.csv'))



## Pasos para modificar el script con nuevos datos
# 1. Actualizar los archivos en la carpeta;
paste0(root, '/01_datos-originales')
# 2. Mantener los nombres de las capas de ser posible en los nuevos
# archivos. De lo contrario cambiar los nombres en las líneas con 
# el el comentario "# < Archivo externo >"
