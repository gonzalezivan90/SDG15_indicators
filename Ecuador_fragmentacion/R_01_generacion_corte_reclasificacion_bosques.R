## Cargar librería ---
c('raster', 'gdalUtilities', 'gdalUtils', 'devtools') %in% rownames(installed.packages())

library(foreign) # Leer tablas de shapefiles y otros formatos
library(raster) # Raster
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(gdalUtils) # Conexion a suit GDAL/OGR para ejecutar externo 


### 1 Definir Ruta de trabajo ----
root <- 'C:/temp/Ecuador_fragmentacion/' # Usar / o \\. Cada uno cambia su ruta
setwd( root ) # asignar ruta de trabajo


outDirs <- c("02_bosques-raster", "03_bosques-reclasificados",  
             "04_resultados-fragmentacion", "05_transiciones" )

sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave
list.dirs(path = '.', full.names = TRUE, recursive = FALSE)


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )



# Evaluar si tenemos acceso a gdal_calc --
# Validar si tenemos la libreria gdal_calc
execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat
                   'py3_env.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
                   '&& py3_env.bat ', # Mantener espacio. Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
                   '&& gdal_calc ') # Llamado de gdal_calc

## Instrucción en QGIS 3.22.6
## (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && gdal_calc'))
## (execGDAL <- paste0('C:\"Program Files"\"QGIS 3.20.1"\OSGeo4W.bat C:\"Program Files"\"QGIS 3.3.20.1"\bin\o4w_env.bat && C:\"Program Files"\"QGIS 3.3.20.1"\bin\o4w_env.bat && gdal_calc'))
## (execGDAL <- paste0('E:\OSGeo4W.bat E:\bin\o4w_env.bat && E:bin\o4w_env.bat && gdal_calc '))



## Mirar la respuesta del sistema a conexion a gdal_calc
(gdalcalc <- capture.output(system(paste0(execGDAL, '--help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
(GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))


### 2. Extraer polígonos de bosques -----



## Bosques -----
folder_bosques <- '01_datos-originales/'
(archivos_bosques <- list.files(path = folder_bosques, 
                                pattern = '^v_ff.+shp$', full.names = TRUE))


## Extraer extent de la capa y rasterizar posteriormente
(info_bosques <- gdalUtils::ogrinfo(archivos_bosques[1], so = TRUE, al = TRUE))
(extent_bosques <- grep('Extent', info_bosques, value = TRUE))
(extent_sin_texto <- gsub('[a-zA-Z]|\\)|\\(|-|:|: |,|^ ', '', extent_bosques) )
(extent_numerico <- as.numeric(strsplit( gsub('  ', ' ', 
                                              extent_sin_texto), ' ')[[1]] ))

## Por si hay error en el paso anterior
# info_bosques <- rgdal::ogrInfo(archivos_bosques[1])
# extent_numerico <- info_bosques$extent


## Por si hay error en el paso anterior
#extent_numerico <- c(490613.4,  9445246.3,  1147853.4, 10163536.3)



## Iterar sobre los bosques -----
for( f in 1:length(archivos_bosques)){ # f = 1 # }
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- ', basename(archivos_bosques[f]) ) )
  
  
  ## Bosques filtrar polígono y rasterizar -----
  (bosque_archivo <- gsub('v_ff010_cobertura_vegetal_', 'BOSQUE_',
                          gsub('_aPolygon', '', basename(archivos_bosques[f]) ) ))
  (anio <- gsub('\\.|[[:alpha:]]|[[:punct:]]', '', bosque_archivo))
  
  ## Bosques rasterizar -----
  (bosque_rasterizado <- paste0( '02_bosques-raster/BOSQUE_', anio, '.tif'))
  print(bosque_rasterizado)
  
  if ( !file.exists(bosque_rasterizado) ) {
    
    tabla_dbf_boque <- foreign::read.dbf(gsub('.shp', '.dbf', archivos_bosques[f]), as.is = TRUE)
    # head(tabla_dbf_boque)
    valores_unicos <- unique(tabla_dbf_boque$cobertura_)
    
    (valores_bosque <- grep('BOS', valores_unicos, value = TRUE))
    
    print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
                ' -- filtro de bosques ',anio) ) 
    print(system.time(
      gdalUtilities::gdal_rasterize(src_datasource = archivos_bosques[f], 
                                    dst_filename = bosque_rasterizado, 
                                    ot = 'Byte',
                                    burn = 1, tr = c(30, 30), init = 0,
                                    where = paste0("cobertura_ LIKE '", valores_bosque,"'"), 
                                    te =  extent_numerico,
                                    co=c("COMPRESS=DEFLATE", "NBITS=1"))
    ))
  }
  
  
  (bosque_recategorizado <- paste0( '03_bosques-reclasificados/BOSQUE_', anio, '.tif'))
  

  if(!file.exists(bosque_recategorizado) & file.exists(bosque_rasterizado)){
    print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
                ' -- reclasificacion de bosques ',anio) )
    if (!GDAL){
      ## Opcion R
      print(system.time({
        bosque_01 <- raster(bosque_rasterizado)
        bosque_12 <- bosque_01 + 1
        writeRaster(x = bosque_12, filename = bosque_recategorizado,
                    datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=2"))
      })) # 179.02 seg 
      
    } else {
      ## Opcion GDAL
      ## Hecho con gdal_calc.py
      (cmd <- gsub(fixed = TRUE, '/', '\\', 
          paste0(execGDAL, 
                 '--calc="1+A"', 
                 ' -A ', root, bosque_rasterizado, 
                 ' --outfile=', root, bosque_recategorizado,
                 ' --type=Byte --co="NBITS=8" --co="COMPRESS=DEFLATE" --quiet'
          ))) #
      print(system.time( 
        system(intern = TRUE, cmd) # 35.21 seg 
      ))
    }
  }
}
