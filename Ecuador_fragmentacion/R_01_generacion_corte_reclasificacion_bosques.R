
## Cargar librería --- Todo debería ser TRUE. Usar archivo de instalación si no.
c('terra', 'gdalUtilities', 'gdalUtils', 'devtools') %in% rownames(installed.packages())

library(terra) # Raster
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(gdalUtils) # Conexion a suit GDAL/OGR para ejecutar externo 

## Cargar funcion de conteo de pixeles
source("https://raw.githubusercontent.com/gonzalezivan90/SDG15_indicators/main/Ecuador_fragmentacion/R_03_tabuleRaster.R")
## Cargar funcion para encontrar los ejecutables de GDAL en el computador
source("https://raw.githubusercontent.com/gonzalezivan90/SDG15_indicators/main/find_gdal.R")

# Si el anterior genera error por conexión a Github, entonces llamar localmente al achivo:
# El archivo se puede descargar con la opción "Guardar como" desde el navegador, al copiar el link anterior. Es posible que el archivo descarge como R_03_tabuleRaster.R.txt
## source("C:/temp/Peru_fragmentacion/09_scripts/R_03_tabuleRaster.R") ## Cambiar por ruta equivalente, o agregar ".txt" al nombre ## <Dato externo original>
# source("C:/temp/Peru_fragmentacion/09_scripts/R_03_tabuleRaster.R.txt") ## Cambiar por ruta equivalente ## <Dato externo original>
# source("C:/temp/Peru_fragmentacion/09_scripts/find_gdal.R.txt") ## Cambiar por ruta equivalente ## <Dato externo original>



### 1 Definir Ruta de trabajo ----
root <- 'C:/temp/Ecuador_fragmentacion/' # Usar / o \\. Cada uno cambia su ruta
dir.create(root); setwd( root ) # asignar ruta de trabajo

outDirs <- c('01_datos-originales', "02_mascara-terrestre", 
             "03_bosques-raster", "04_bosques-reclasificados",  
             "05_bosques-nucleo" )

sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave
list.dirs(path = '.', full.names = TRUE, recursive = FALSE)


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )

## Asegure tener estas capas -- cambie nombres más adelante si usa capas diferentes
## donde dice  ## <Dato externo original>
## Las capas deben tener la misma proyeccion
## Descargar datos desde: http://ide.ambiente.gob.ec:8080/mapainteractivo/
# Capas de bosque: v_ff010_cobertura_vegetal ... 
# Rio_l_250K_ECU_IGM.shp
# Rio_a_250K_ECU_IGM.shp


## Identificar archivos originales -- TODOS deben se TRUE
poligonos_rios_a <- '01_datos-originales\\Rio_a_250K_ECU_IGM.shp' ## <Dato externo original>
file.exists(poligonos_rios_a) # - debe se TRUE
lineas_rios_l <- '01_datos-originales\\Rio_l_250K_ECU_IGM.shp' ## <Dato externo original>
file.exists(lineas_rios_l) # - debe se TRUE



# Evaluar si tenemos acceso a gdal_calc --
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


### 2. Extraer polígonos de bosques -----


## Bosques -----
folder_bosques <- '01_datos-originales/'
## Revisar bosques disponibles
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
## Por si hay error en el paso anterior
#extent_numerico <- c(490613.4,  9445246.3,  1147853.4, 10163536.3)


## Extraer nombres de columnas
todos_nombres_columnas <- lapply(archivos_bosques, function(x){ # x <- archivos_bosques[1]
  dbf.x <- foreign::read.dbf( gsub('.shp', '.dbf', x), as.is = TRUE)
  bosque_pos <- unique(unlist(sapply(dbf.x, function(x) {grep('bosque', tolower(x))} )))
  (anio <- gsub('_$|.+__', "", gsub("([^_{0-9, 4}_])", '', basename(x))))
  dbf.y <- cbind.data.frame(yyyy = anio, colnam = colnames(dbf.x), 
                            example = as.vector(t(dbf.x[bosque_pos[1], ])))
})

do.call(rbind, todos_nombres_columnas)


# Consignar los nombres que contienen las columnas con bosque en las diferentes capas
nombres_columnas <- c('cobertura_', 'ctn2')



## Rios ------
# Cuerpos de agua de mapa de ecosistemas
coberturas <-  archivos_bosques[1]
agua_ecosistemas <-  paste0('02_mascara-terrestre/', 'agua_ecosistemas.shp')
dbf_inicial = foreign::read.dbf(gsub('.shp', '.dbf', coberturas))
table(dbf_inicial$cobertura_)
if ( ! file.exists(agua_ecosistemas) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32717',
                           src_datasource_name = coberturas, 
                           dst_datasource_name = agua_ecosistemas,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           where = "cobertura_ LIKE 'ESPEJOS DE AGUA NATURAL'")
  ) # 50
}

# Buffer de 1m a rios en lineas
rios_buffer1m <-  paste0('02_mascara-terrestre/', 'rioslineasbuffer1m.shp')
if ( ! file.exists(rios_buffer1m) ){
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32717',
                           src_datasource_name = lineas_rios_l, 
                           dst_datasource_name = rios_buffer1m,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('SELECT ST_buffer(geometry, 1) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(lineas_rios_l)))
    ) 
  ) # 50
}


# Unir las capas de rios en poligonos y líneas (su buffer)
archivo_rios_poligonos <-  paste0('02_mascara-terrestre/', 'rios_poligonos.shp')
if ( ! file.exists(archivo_rios_poligonos) ){
  
  ## Creamos una capa de rios poligonos igual a la de navegables
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32717',
                           src_datasource_name = poligonos_rios_a, 
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
  
  ## Anexamos la capa de buffer de 1m 
  system.time(
    gdalUtilities::ogr2ogr(t_srs = 'EPSG:32717',
                           src_datasource_name = agua_ecosistemas, 
                           dst_datasource_name = archivo_rios_poligonos,
                           update = TRUE, append = TRUE, 
                           nln = tools::file_path_sans_ext(basename(archivo_rios_poligonos)),
                           f = "ESRI Shapefile")
  ) # 13
  
  
}

mascara_terrestre <- paste0('02_mascara-terrestre/', 'mascara_terrestre.tif')
if (!file.exists(archivo_rios_buffer)){
  
  ## Rasterizar la capa de buffers
  print(system.time(
    gdalUtilities::gdal_rasterize(at = TRUE, 
                                  src_datasource = archivo_rios_poligonos, 
                                  dst_filename = archivo_rios_buffer,
                                  ot = 'Byte', # Para permitir valores negativos
                                  burn = 1, tr = c(30, 30), init = 0, 
                                  te =  extent_numerico, # xmin ymin xmax ymax
                                  co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 30seg
}

## Borrar archivos intermedios? 
borrar_archivos_intermedios <- TRUE

## Iterar sobre los bosques -----
for( f in 1:length(archivos_bosques)){ # f = 1 # }
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- ', basename(archivos_bosques[f]) ) )
  
  
  ## Bosques filtrar polígono y rasterizar -----
  (bosque_archivo <- gsub('v_ff010_cobertura_vegetal_', 'BOSQUE_',
                          gsub('_aPolygon', '', basename(archivos_bosques[f]) ) ))
  (anio <- gsub('\\.|[[:alpha:]]|[[:punct:]]', '', bosque_archivo))
  
  ## Bosques rasterizar -----
  (bosque_rasterizado <- paste0( '03_bosques-raster/BOSQUE_', anio, '.tif'))
  print(bosque_rasterizado)
  
  if ( !file.exists(bosque_rasterizado) ) {
    
    tabla_dbf_boque <- foreign::read.dbf(gsub('.shp', '.dbf', archivos_bosques[f]), as.is = TRUE)
    # head(tabla_dbf_boque)
    (field.f <- colnames(tabla_dbf_boque)[which(colnames(tabla_dbf_boque) %in% nombres_columnas)[1]])
    valores_unicos <- unique(tabla_dbf_boque[, field.f])
    (valores_bosque <- valores_unicos[grep('bos', tolower(valores_unicos))])
    
    cat(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
                ' -- filtro de bosques ',anio, '\n    Campo: ',
                field.f, '\n    Valores: ',
                paste0(valores_bosque, collapse = ', '), '\n') ) 
    
    print(system.time(
      gdalUtilities::gdal_rasterize(src_datasource = archivos_bosques[f], 
                                    dst_filename = bosque_rasterizado, 
                                    ot = 'Byte',
                                    burn = 1, tr = c(30, 30), init = 0,
                                    where = paste0(field.f, " LIKE '", valores_bosque[1],"'"), 
                                    te =  extent_numerico,
                                    co=c("COMPRESS=DEFLATE", "NBITS=1"))
    )) # 260 - 5mins
  }
  
  ## 
  
  
  
  ## Eliminar areas pequeñas -- microperforaciones
  (bosques_rellenados_pesado <- paste0('04_bosques-reclasificados/', '/bosqueLlenadoPesado_', anio, '.tif' ))
  (bosques_rellenados <- paste0('04_bosques-reclasificados/', '/bosqueLlenado_', anio, '.tif' ))
  
  if (!file.exists(bosques_rellenados)){
    if (!file.exists(bosques_rellenados_pesado) ){
      (cmd <- gsub(fixed = TRUE, '/', '\\', 
                   paste0(execGDALsieve,
                          ' -st 11 -4 -nomask ',
                          ' ', root2, '\\', bosque_rasterizado, # Entrada
                          ' ', root2, '\\', bosques_rellenados_pesado # Salida
                   ) 
      ))
      print(system.time(system(cmd)) )  #  30
    }
    
    ## Crear archivo mas liviano
    if (!file.exists(bosques_rellenados) ){
      rx <- tryCatch(terra::rast(bosques_rellenados_pesado), error = function(e) NULL)
      ## Crear archivo mas liviano
      if ( class(rx) %in% c('RasterLayer', 'SpatRaster') ){
        # cmd <- paste0('time gdal_translate ', sieve3,' ', sieve2, ' -ot Byte -co "COMPRESS=DEFLATE"')
        print(
          system.time(
            gdalUtilities::gdal_translate(
              a_nodata = 255, # dato que se usa como nodata original, es decir que el 0 sea escrito
              src_dataset = bosques_rellenados_pesado, 
              dst_dataset = bosques_rellenados,
              ot = 'Byte', co = "COMPRESS=DEFLATE")
          ))
      }
      rx <- tryCatch(terra::rast(bosques_rellenados), error = function(e) NULL)
      if ( class(rx) %in% c('RasterLayer', 'SpatRaster') 
           & file.size(bosques_rellenados) > 100000000 ){
        if( borrar_archivos_intermedios)
          file.remove(bosques_rellenados_pesado)  ## Borrar archivo si pesa mucho
      }
    }
  }
  ## En QGIS: Revisar capa creada
  
  
  ## Recuperar fragmentos pequeños y no considerar rios o limites como fragmentacion.   
  (bosques_refinados <- paste0('04_bosques-reclasificados/', '/bosqueRefinado_', anio, '.tif' ))
  if (!file.exists(bosques_refinados) & file.exists(bosque_rasterizado) & file.exists(bosques_rellenados)) {
    (cmd <- gsub(fixed = TRUE, '/', '\\', 
                 paste0(execGDAL,
                        ' -A ', root2, '\\', bosque_rasterizado, # 0 no bosque, 1 bosque
                        ' -B ', root2, '\\', bosques_rellenados, # 0 no bosque, 1 bosque
                        ' -C ', root2, '\\', mascara_terrestre , # 0 no bosque, 1 bosque
                        ' --outfile=', root2, '\\', bosques_refinados,
                        ## Dejar bosques (1) en mascara terrestre (C), que hayan sido bosques iniciales (B)
                        ## y rellenandos en la capa refinada (B). Los rios quedan como 2, y no bosque como 0
                        ' --calc="(C * (A + (logical_and(A==0, B==1 )))) + (2 * (C == 0))" ',
                        ' --type=Byte  --co="COMPRESS=DEFLATE"') # --co="NBITS=8"
    ))
    print(system.time(system(cmd)) ) # 611 || 2870.31 ~ 50min
  }
  ## En QGIS: Revisar capa creada
  
  ## Categorias a usar en las distancias
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
  ## En QGIS: Revisar capa creada
  
  
  ## Crear bosque nucleo
  (bosque_nucleo <- paste0('05_bosques-nucleo/', 'bosqueNucleo_', anio, '.tif' ))
  if (!file.exists(bosque_nucleo) ) {
    (cmd <- gsub(fixed = TRUE, '/', '\\', 
                 paste0(execGDAL,
                        ' -A ', root2, '\\', bosque_rasterizado, 
                        ' -B ', root2, '\\', distancia_bosques, 
                        ' --outfile=', root2, '\\', bosque_nucleo,
                        ' --calc="(logical_and(A==1 , B==1 ))" ',
                        ' --type=Byte  --co="NBITS=1" --co="COMPRESS=DEFLATE"')
    ))
    print(system.time(system(cmd)) ) # 611 / 270 co
  }
  ## En QGIS: Revisar capa creada
  
  print ( paste('   -- Guardado fecha ', anio ) )
  
}

## Calcula pixeles terrestres totales del pais
(area_terrestre <- tabuleRaster(mascara_terrestre_01, del0 = TRUE, n256 = TRUE))
# 1 == 1398064402

## Extraer estadisticas de capa de bosques anuales
(archivos_bosques <- list.files(path = '03_bosques-anuales/', pattern = 'bosqueFecha.+.tif$', full.names = TRUE))
areas_bosques <- lapply(archivos_bosques, function(x){
  print(x)
  cbind.data.frame(tipo = 'bosque', anio = gsub('bosqueFecha|_|.tif', '', basename(x)), 
                   tabuleRaster(x, del0 = TRUE, n256 = TRUE))
})

## Extraer estadisticas de capa de bosques nucelos anuales
(archivos_nucleos <- list.files(path = '05_resultados-fragmentacion/', pattern = 'bosqueNucleo.+.tif$', full.names = TRUE))
areas_nucleos <- lapply(archivos_nucleos, function(x){
  print(x)
  cbind.data.frame(tipo = 'nucleo', anio = gsub('bosqueNucleo|_|.tif', '', basename(x)), 
                   tabuleRaster(x, del0 = TRUE, n256 = TRUE))
})


## Compilar tablas
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
write.csv(x = tabla_final, file = paste0('tabla_final_fragmentacion__sep-coma_', Sys.Date(), '.csv'))
write.csv2(x = tabla_final, file = paste0('tabla_final_fragmentacion__sep-puntoycoma_', Sys.Date(), '.csv'))

