## Cargar librería --- Todo debería ser TRUE. Usar archivo de instalación si no.
c('raster', 'gdalUtilities', 'gdalUtils', 'devtools') %in% rownames(installed.packages())

library(raster) # Raster
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(gdalUtils) # Conexion a suit GDAL/OGR para ejecutar externo 

## Cargar funcion de conteo de pixeles
source("https://raw.githubusercontent.com/gonzalezivan90/SDG15_indicators/main/Ecuador_fragmentacion/R_03_tabuleRaster.R")
source("https://raw.githubusercontent.com/gonzalezivan90/SDG15_indicators/main/find_gdal.R")
# Si el anterior genera error por conexión a Github, entonces llamar localmente al achivo:
# El archivo se puede descargar con la opción "Guardar como" desde el navegador, al copiar el link anterior. Es posible que el archivo descarge como R_03_tabuleRaster.R.txt
## source("C:/temp/Peru_fragmentacion/09_scripts/R_03_tabuleRaster.R") ## Cambiar por ruta equivalente, o agregar ".txt" al nombre ## <Dato externo original>
# source("C:/temp/Peru_fragmentacion/09_scripts/R_03_tabuleRaster.R.txt") ## Cambiar por ruta equivalente ## <Dato externo original>
# source("C:/temp/Peru_fragmentacion/09_scripts/find_gdal.R.txt") ## Cambiar por ruta equivalente ## <Dato externo original>


### 1 Definir Ruta de trabajo ----
root <- 'C:/temp/Peru_fragmentacion/' # Usar / o \\. Cada uno cambia su ruta ## <Dato externo original>
setwd( root ) # asignar ruta de trabajo

# Descargar y descomprimir los archivos en esta cartpeta
(orig <- paste0(root, '/01_datos-originales')); dir.create(orig, recursive = TRUE)

## Asegure tener estas capas -- cambie nombres más adelante si usa capas diferentes
## donde dice  ## <Dato externo original>
## Las capas deben tener la misma proyeccion
## Descargar datos desde: https://geobosques.minam.gob.pe/geobosque/view/descargas.php#download
# Bosque_No_Bosque_2020.tif
# Perdida_2001_2020.tif
# Bosque_No_Bosque_2020_Vector.shp
# rios_poligonos.shp
# Polígonos_limite_nacional.shp --- Esta capa requiere ser del extent completo del pais


## Identificar archivos originales
archivo_bosques <- '01_datos-originales/Bosque_No_Bosque_2022.tif' ## <Dato externo original>
file.exists(archivo_bosques)
archivo_perdida <- '01_datos-originales/Perdida_2001_2022.tif' ## <Dato externo original>
file.exists(archivo_perdida)
poligonos_rios <- '01_datos-originales\\rios_poligonos.shp' ## <Dato externo original>
file.exists(poligonos_rios)
poligonos_limites <- '01_datos-originales\\Polígonos_limite_nacional.shp' ## <Dato externo original>
file.exists(poligonos_limites)

## Crear subcarpetas

outDirs <- c("02_bosques-filtrados", "03_bosques-anuales", "04_bosques-reclasificados",  
             "05_resultados-fragmentacion")

sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave
list.dirs(path = '.', full.names = TRUE, recursive = FALSE)


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', 
               x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )


## vERSION ANTIGUA -- LA FUNCION gdalPaths los reemplaza
# Evaluar si tenemos acceso a gdal_calc --
# Validar si tenemos la libreria gdal_calc
#  -- Dejar un espacio despues de gdal_calc --
# execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat
#                    'py3_env.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
#                    '&& py3_env.bat ', # Mantener espacio. Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
#                    '&& gdal_calc ') # Llamado de gdal_calc

## Instrucción en QGIS 3.22.6
## (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && gdal_calc'))
## (execGDAL <- paste0('C:\"Program Files"\"QGIS 3.20.1"\OSGeo4W.bat C:\"Program Files"\"QGIS 3.3.20.1"\bin\o4w_env.bat && C:\"Program Files"\"QGIS 3.3.20.1"\bin\o4w_env.bat && gdal_calc'))
## (execGDAL <- paste0('C:/OSGeo4W64/OSGeo4W.bat py3_env.bat && py3_env.bat && gdal_calc'))

gdalPaths(help = TRUE)
gdal <- gdalPaths(depth = 3, drives = c('C'), latestQ = FALSE, help = FALSE)

(execGDAL <- gdal$execGDALcalc)

## Mirar la respuesta del sistema a conexion a gdal_calc
(gdalcalc <- (system(paste0(execGDAL, ' --help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
# (GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))
(GDAL <- gdal$gdalcalcworks)

# (execGDALsieve <- gsub('gdal_calc', 'gdal_sieve', execGDAL))
# (gdalsieve <- (system(paste0(execGDALsieve, ''), intern = TRUE))) ## Esta opción no necesita "--help"
(execGDALsieve <- gdal$execGDALsieve)
(gdalsieve <- gdal$gdalsieveworks)

# (execGDALproxy <- gsub('gdal_calc', 'gdal_proximity', execGDAL))
# (gdalproxy <- (system(paste0(execGDALproxy, ''), intern = TRUE))) ## Esta opción no necesita "--help"
(execGDALproxy <- gdal$execGDALproxy)
(gdalproxy <- gdal$execGDALproxy)



### 2. Cortar a zonas de bosques -----
## Asignar TRUE si se quiere hacer un recorte de las capas y sólo usar la parte
## del país que tiene bosque. Esto conviene para maquinas pequeñas. Se recomienda
## dejar en FALSE, especialmente si se cuenta con un buen computador

recortar_analisis <- FALSE # dejar como TRUE o FALSE. Mejor como FALSE

## Recortar el extent del pais. 
if (!recortar_analisis) {
  
  ## Extrar máscara vectorial de zonas con bosque para filtrar
  ## polígonos y extraer ventana donde recortaremos los tifs nacionales y así
  ## reducir el tamaño de las capas a analizar en fragmentación, para así ahorrar tiempo 
  ## de procesamiento y almacenamiento 
  
  ## El siguiente procedmiento filtra la capa vectorial de polígonos de bosque
  ## pero se demora bastante (1h) -- no usaremos esta opción
  
  archivo_vect_bosque <- '01_datos-originales/Bosque_No_Bosque_2020_Vector.shp'
  file.exists(archivo_vect_bosque)
  
  archivo_vect_bosque_filtrado <-  paste0('08_recorte_MSPA/', 'vect_bosques_filtrados.shp')
  
  if (!file.exists(archivo_vect_bosque_filtrado)){
    print(system.time(
      gdalUtilities::ogr2ogr(src_datasource_name = archivo_vect_bosque,
                             dst_datasource_name = archivo_vect_bosque_filtrado,
                             f = "ESRI Shapefile", dialect = 'sqlite',
                             where = paste0("Cobertura!='No Monitoreado'") )
    )) #  3593.69s = 1h
  }
  
  
  ## El siguiente procedimiento crea una capa de cuadrado o envolvente (bounding box - bbox)
  ## de la capa vectorial de polígonos de bosque solamente
  ## -- Omitiremos ese paso por ahora, así dejamos el analisis en todo el extent nacional
  
  archivo_vect_bosque <- '01_datos-originales/Bosque_No_Bosque_2020_Vector.shp'
  file.exists(archivo_vect_bosque)
  
  archivo_vect_bosque_filtrado_bbox <-  paste0('01_datos-originales/', 'vect_bosques_bbox.shp')
  
  if (!file.exists(archivo_vect_bosque_filtrado_bbox)){
    print(system.time(
      gdalUtilities::ogr2ogr(src_datasource_name = archivo_vect_bosque,
                             dst_datasource_name = archivo_vect_bosque_filtrado_bbox,
                             f = "ESRI Shapefile", dialect = 'sqlite',
                             sql = paste0("SELECT extent(geometry) FROM Bosque_No_Bosque_2020_Vector WHERE ",
                                          'Cobertura IN ["No Monitoreado"]') )
    )) #  355s
  }
}


## Obtener extents de capas 
(info_perdida <- raster::raster(archivo_perdida)) # 
(extent_completo <- c(xmn = info_perdida@extent@xmin, 
                     ymn = info_perdida@extent@ymin, 
                     xmx = info_perdida@extent@xmax, 
                     ymx = info_perdida@extent@ymax))

(info_bosques <- raster::raster(archivo_bosques)) # 
(extent_bosques <- c(xmn = info_bosques@extent@xmin, 
                     ymn = info_bosques@extent@ymin, 
                     xmx = info_bosques@extent@xmax, 
                     ymx = info_bosques@extent@ymax))

(info_nacional <- raster::shapefile(poligonos_limites)) # 
(extent_nacional <- c(xmn = info_nacional@bbox['x', 'min'], 
                     ymn = info_nacional@bbox['y', 'min'], 
                     xmx = info_nacional@bbox['x', 'max'], 
                     ymx = info_nacional@bbox['y', 'max']))




if (!recortar_analisis) {
  ## Extent completo
  (extent_numerico <- extent_completo)
  
  (info_perdida <- raster::raster(archivo_perdida)) # 
  extent_numerico_perd <- c(info_perdida@extent@xmin, 
                            info_perdida@extent@ymin, 
                            info_perdida@extent@xmax, 
                            info_perdida@extent@ymax)
  names(extent_numerico_perd) <- c('xmn', 'ymn', 'xmx', 'ymx')
  extent_numerico <- extent_numerico_perd 
  
} else {
  
  ## Extraer la extensión de las zonas donde hay bosque en el pais
  extent_recortado <- raster::shapefile(archivo_vect_bosque_filtrado_bbox)
  e_r <- extent_recortado@bbox
  extent_numerico <- extent_numerico_perd <- 
    c(xmn = e_r['x', 'min'], 
      ymn = e_r['y', 'min'], 
      xmx = e_r['x', 'max'], 
      ymx = e_r['y', 'max'])
}




### 3. Filtrar capas y recategorizar  -----
## Convertir capa de bosques entre 0 y 1 con bosques a 2000
# Identificar bosques de linea base al año 2000
archivo_bosques_inicial <- '02_bosques-filtrados\\Bosque_inicial_2000.tif' ## Usar doble \ en este caso
raster_inicial <- raster(archivo_bosques)

if(!file.exists(archivo_bosques_inicial) ){
  if (!GDAL){
    ## Opcion R
    print(system.time(
      if (! file.exists(archivo_inicial_0) ){
        bosque_inicial2000 <- 1 + Which(raster_inicial == 3 | raster_inicial == 1) # Escoger valores de bosque(3) y deforestacion (1)
        # plot(bosque_inicial2000)
        writeRaster(x = bosque_inicial2000, filename = archivo_bosques_inicial,
                    datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      }
    )) # 4356.74seg # 70mins
  } else {
    ## Opcion GDAL
    print(system.time(
      system(
        paste0(execGDAL, 
               ' --calc=" (1*(logical_or(A==3, A==1)))" ', # Escoger valores de bosque(3) y deforestacion (1) 
               '-A ', root2, '\\', archivo_bosques,
               ' --outfile=', root2, '\\', archivo_bosques_inicial,
               ' --type=Byte --NoDataValue=255 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
        )))) # 7500.20s   
  }
}

## Crear archivos de linea base
archivo_perdida_inicial <- '02_bosques-filtrados\\Perdida_2001_2021_llenado0.tif'

## Cambiar datos nulos a 0 en capa raster de perdida de bosque
if (! file.exists(archivo_perdida_inicial) ){
  # print(system.time(
  #   gdalUtilities::gdal_translate(a_nodata = 255, # dato que se usa como nodata original, es decir que el 0 sea escrito
  #                                                   dst_dataset = archivo_perdida_inicial,
  #                                                   src_dataset = archivo_perdida, 
  #                                                   ot = 'Byte')
  # ))
  
  print(system.time(
    gdalUtilities::gdalwarp(srcnodata = 255, dstnodata = 255, # dato que se usa como nodata original, es decir que el 0 sea escrito
                            dstfile = archivo_perdida_inicial,
                            srcfile = archivo_perdida, 
                            te = extent_bosques, 
                            tr = res(info_bosques),
                            ot = 'Byte')
  )) #70
}



## Crear capas de máscara con pixeles terrrestres: limites nacionales - ríos

mascara_terrestre <- '02_bosques-filtrados\\mascara_terrestre.tif'
mascara_terrestre_01 <- '02_bosques-filtrados\\mascara_terrestre01.tif' ## Corregido sin valores negativos
mascara_terrestre0 <- '02_bosques-filtrados\\mascara_terrestre0.tif' # Final con extensión del analisis 


## Crear máscara de pixeles terrestres
if (! file.exists(mascara_terrestre_01) ){
  if (! file.exists(mascara_terrestre) ){
    print(system.time(
      gdalUtilities::gdal_rasterize(src_datasource = poligonos_limites, 
                                    dst_filename = mascara_terrestre,
                                    te = extent_nacional, ## Extent nacional
                                    tr = res(info_perdida),
                                    burn = 1, init = 0, 
                                    ot = 'Int16',
                                    co = c("COMPRESS=DEFLATE")
      ))) # 145s
    
    ## Eliminamos los rios. gdalUtils::gdal_rasterize sí tiene el parametro add
    print(system.time(
      gdalUtils::gdal_rasterize(src_datasource = poligonos_rios, 
                                    dst_filename = mascara_terrestre, 
                                    at = TRUE, 
                                    add = TRUE,  burn = -1)
    )) ## 201
    
    ## Corregimso valores negativos
    if (! file.exists(mascara_terrestre_01) ){
      print(system.time(
        gdalUtilities::gdalwarp(srcnodata = 255, dstnodata = 255, # dato que se usa como nodata original, es decir que el 0 sea escrito
                                srcfile = mascara_terrestre, 
                                dstfile = mascara_terrestre_01,
                                ot = 'Byte', co = c("COMPRESS=DEFLATE", "NBITS=1")) # Asegurarnos de solo 1 y 0
      )) # 149s
    }
  }
}
  
## Obtener pixeles terrestres
(pixeles_mascara <- tabuleRaster(mascara_terrestre_01, del0 = TRUE, n256 = TRUE))
## Debe ser 1398064402

mascara_terrestre0 <- '02_bosques-filtrados\\mascara_terrestre0.tif' ## Corregido
## Extraer la extensión de las zonas donde hay bosque en el pais
if(!file.exists(mascara_terrestre0)){
  print(system.time(
    gdalUtilities::gdalwarp(srcnodata = 255, dstnodata = 255, # dato que se usa como nodata original, es decir que el 0 sea escrito
                            srcfile = mascara_terrestre_01, 
                            dstfile = mascara_terrestre0,
                            te = extent_bosques, 
                            tr = res(info_bosques),
                            ot = 'Byte', co = c("COMPRESS=DEFLATE", "NBITS=1"))
  ))
}

  

### 3 Generar capas anuales ----

raster_inicial <- raster(archivo_bosques_inicial)
raster_perdida <- raster(archivo_perdida_inicial)
fechas_unicas_pixeles <- tabuleRaster(archivo_perdida_inicial, del0 = TRUE, n256 = TRUE)
(fechas_unicas <- fechas_unicas_pixeles$id)

## Iterar sobre los bosques 
## Las siguientes intrucciones se repetiran para cada año entre 2000 y 2020

f = 10 ## Inicializamos para hacer la prueba con una sola 

for( f in (1:length(fechas_unicas))){ # f = 10 # } -------- DESCOMENTAR Y HABILITAR PARA CORRER PARA TODOS LOS AÑOS
  
  ## Definir valores en cada paso, como anio y nombres de capas
  (anio <- 2000+fechas_unicas[f]) ## entre 2000 y 2020
  anio2 <- fechas_unicas[f] ## entre 0 y 20
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- anio ', anio ) )
  (nombre_raster_bosque <- paste0('03_bosques-anuales', '/bosqueFecha_', anio, '.tif' ))
  
  
  ## Crear capa de bosque por anio reclasificando valores de capa de bosque de perdida
  if (!file.exists(nombre_raster_bosque)){
    if (! GDAL){
      ## Hecho en R
      raster_j <- 1 + raster_inicial - raster::Which(raster_perdida < anio2)
      writeRaster(x = raster_j, filename = nombre_raster_bosque,
                  datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      
    } else {
      ## Hecho con gdal_calc.py
      (cmd <- gsub(fixed = TRUE, '/', '\\', 
         paste0(execGDAL,
                ' --calc=" A-(1*(logical_and(B!=0,B<=', anio2, ')))" ',
                '-A ', root2, '\\', archivo_bosques_inicial,
                ' -B ', root2, '\\', archivo_perdida_inicial , 
                ' --outfile=', root2, '\\', nombre_raster_bosque,
                ' --type=Byte --NoDataValue=255 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                # 
         ) 
      ))
      
      print(system.time( 
        system(intern = TRUE, cmd) #  2802.00  ~ 40m
      ))
    }
  }
  
  
  ## Eliminar areas pequeñas -- microperforaciones
  (bosques_rellenados_pesado <- paste0('04_bosques-reclasificados/', '/bosqueLlenadoPesado_', anio, '.tif' ))
  (bosques_rellenados <- paste0('04_bosques-reclasificados/', '/bosqueLlenado_', anio, '.tif' ))
  
  
  if (!file.exists(bosques_rellenados)){
    if (!file.exists(bosques_rellenados_pesado) ){
      ## cmd <- paste0('time gdal_sieve.py ', intif,' -st 11 -4 -nomask ', sieve3, '')
      (cmd <- gsub(fixed = TRUE, '/', '\\', 
                   paste0(execGDALsieve,
                          ' -st 11 -4 -nomask ',
                          ' ', root2, '\\', nombre_raster_bosque, # Entrada
                          ' ', root2, '\\', bosques_rellenados_pesado # Salida
                          # 
                   ) 
      ))
      print(system.time(system(cmd)) )  #  170.97s 
      
    }
    
    
    ## Crear archivo mas liviano
    if (!file.exists(bosques_rellenados) ){
      rx <- tryCatch(raster::raster(bosques_rellenados_pesado), error = function(e) NULL)
      ## Crear archivo mas liviano
      if ( class(rx) == 'RasterLayer' ){
        # cmd <- paste0('time gdal_translate ', sieve3,' ', sieve2, ' -ot Byte -co "COMPRESS=DEFLATE"')
        print(system.time(gdalUtilities::gdal_translate(a_nodata = 255, # dato que se usa como nodata original, es decir que el 0 sea escrito
                                                        src_dataset = bosques_rellenados_pesado, 
                                                        dst_dataset = bosques_rellenados,
                                                        ot = 'Byte', co = "COMPRESS=DEFLATE")
        ))
        
      }
      rx <- tryCatch(raster::raster(bosques_rellenados), error = function(e) NULL)
      if ( class(rx) == 'RasterLayer' & file.size(bosques_rellenados) > 100000000 ){
        # file.remove(bosques_rellenados_pesado)  ## Borrar archivo si pesa mucho
      }
    }
  }
  


  ## Recuperar fragmentos pequeños y no cosniderar rios o limites como fragmentacion  
  (bosques_refinados <- paste0('04_bosques-reclasificados/', '/bosqueRefinado_', anio, '.tif' ))
  if (!file.exists(bosques_refinados) & file.exists(nombre_raster_bosque) & file.exists(bosques_rellenados)) {
    (cmd <- gsub(fixed = TRUE, '/', '\\', 
                 paste0(execGDAL,
                        ' -A ', root2, '\\', nombre_raster_bosque, # 57549, 41470, 2386557030  (nrow, ncol, ncell)
                        ' -B ', root2, '\\', bosques_rellenados, # 57549, 41470, 2386557030  (nrow, ncol, ncell)
                        ' -C ', root2, '\\', mascara_terrestre0, # mascara_terrestre0
                        ' --outfile=', root2, '\\', bosques_refinados,
                        ' --calc="(C * (A + (logical_and(A==0, B==1 )))) + (2 * (C == 0))" ',
                        ' --type=Byte  --co="NBITS=8" --co="COMPRESS=DEFLATE"')
    ))
    print(system.time(system(cmd)) ) # 611 || 2870.31 ~ 50min
  }
  
  ## 0 pixeles que fragmentan
  ## 1 para bosque
  ## 2 para agua y limites nacionales que no fragmentan
  
  
  ## Crear el buffer de distancia
  (distancia_bosques <- paste0('04_bosques-reclasificados/', '/distanciaABosques_', anio, '.tif' ))
  if ( !file.exists(distancia_bosques)) {
    # cmd <- paste0('time gdal_proximity.py ', sieve, " ", distt, ' -distunits PIXEL -values 1 -nodata 36 -ot Byte -fixed-buf-val 0 -maxdist 35 -co NBITS=1 -co COMPRESS=DEFLATE')
    (cmd <- gsub(fixed = TRUE, '/', '\\', 
                 paste0(execGDALproxy,
                        ' ', root2, '\\', bosques_refinados, # Entrada
                        ' ', root2, '\\', distancia_bosques, # Salida
                        ' -distunits PIXEL', # Unidades a medir en pixel
                        ' -values 0 ', # 0 es el valor de pixeles que deforestan y generan efecto borde, al que se medira la distancia
                        ' -maxdist 35 ', # Distancia maxima. Despues de 35 pixeles se asigna una constante 
                        ' -nodata 36 ', # # Valores a dejar fuera del buffer despues de 35 pixeles // el 36 se convierte en 1 por NBITS=1
                        ' -fixed-buf-val 0 ', # Valores a dejar dentro del buffer
                        ' -ot Byte -co NBITS=1 -co COMPRESS=DEFLATE')
    ))
    
    print(system.time(system(cmd)) ) # 
    
  }
  
  
  ## Crear bosque nucleo
  (bosque_nucleo <- paste0('05_resultados-fragmentacion/', 'bosqueNucleo_', anio, '.tif' ))
  if (!file.exists(bosque_nucleo) ) {
    (cmd <- gsub(fixed = TRUE, '/', '\\', 
                 paste0(execGDAL,
                        ' -A ', root2, '\\', nombre_raster_bosque, 
                        ' -B ', root2, '\\', distancia_bosques, 
                        ' --outfile=', root2, '\\', bosque_nucleo,
                        ' --calc="(logical_and(A==1 , B ==1 ))" ',
                        ' --type=Byte  --co="NBITS=1" --co="COMPRESS=DEFLATE"')
    ))
    print(system.time(system(cmd)) ) # 611 / 270 co
  }
  
  print ( paste('   -- Guardado fecha ', anio ) )

} # Aparecera en error si no habilitamos el "for" 


(area_terrestre <- tabuleRaster(mascara_terrestre_01, del0 = TRUE, n256 = TRUE))
# 1 == 1398064402

(archivos_bosques <- list.files(path = '03_bosques-anuales/', pattern = 'bosqueFecha.+.tif$', full.names = TRUE))
areas_bosques <- lapply(archivos_bosques, function(x){
  print(x)
  cbind.data.frame(tipo = 'bosque', anio = gsub('bosqueFecha|_|.tif', '', basename(x)), 
                   tabuleRaster(x, del0 = TRUE, n256 = TRUE))
})


(archivos_nucleos <- list.files(path = '05_resultados-fragmentacion/', pattern = 'bosqueNucleo.+.tif$', full.names = TRUE))
areas_nucleos <- lapply(archivos_nucleos, function(x){
  print(x)
  cbind.data.frame(tipo = 'nucleo', anio = gsub('bosqueNucleo|_|.tif', '', basename(x)), 
                   tabuleRaster(x, del0 = TRUE, n256 = TRUE))
})


tabla_cifras <- rbind(do.call(rbind, areas_nucleos), do.call(rbind, areas_bosques))
tabla_parcial <- xtabs(data = tabla_cifras, count ~ anio + id + tipo)

tabla_final <- cbind.data.frame(anio = rownames(tabla_parcial), 
                                pix_total_terr = area_terrestre$count[area_terrestre$id == 1],
                                pix_bosq = tabla_parcial[, '1', 'bosque'],
                                pix_nucl = tabla_parcial[, '1', 'nucleo'])
tabla_final$porc_area_nucl <- tabla_final$pix_nucl / tabla_final$pix_total_terr * 100
tabla_final$porc_bosq_nucl <- tabla_final$pix_nucl / tabla_final$pix_bosq * 100

tabla_final # ver resultado


## Guardar archivo en 2 formatos, con mismos valores, pr cuestion de formatos internos de cada computador
print(getwd()) ## aca se guardan los resultados
write.csv(tabla_final, 'tabla_final_fragmentacion__sep-coma.csv')
write.csv2(tabla_final, 'tabla_final_fragmentacion__sep-puntoycoma.csv')
