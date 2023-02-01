## Cargar librería ---
c('raster', 'gdalUtilities', 'gdalUtils', 'devtools') %in% rownames(installed.packages())

library(raster) # Raster
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 
library(gdalUtils) # Conexion a suit GDAL/OGR para ejecutar externo 


### 1 Definir Ruta de trabajo ----
root <- 'C:/temp/Peru_fragmentacion/' # Usar / o \\. Cada uno cambia su ruta
setwd( root ) # asignar ruta de trabajo


outDirs <- c("02_bosques-cortados", "03_bosques-filtrados", "04_bosques-anuales", "05_bosques-reclasificados",  
             "06_resultados-fragmentacion", "07_transiciones" )

sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave
list.dirs(path = '.', full.names = TRUE, recursive = FALSE)


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
(root2 <- gsub(pattern = '/', replacement = '\\', x = paste0(paste0(root2, collapse = '/'), '/'), fixed = TRUE))
(root2 <- gsub(pattern = '/', replacement = '\\', x = root2) )



# Evaluar si tenemos acceso a gdal_calc --
# Validar si tenemos la libreria gdal_calc
#  -- Dejar un espacio despues de gdal_calc --
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


### 2. Cortar a zonas de bosques -----

## Extrar máscara vectorial de zonas xcon bosque para filtrar
## polígonos y extraer ventana donde recortaremos los tifs nacionales y así
## reducir el tamaño de las capas a analizar en fragmentación, para así ahorrar tiempo 
## de procesamiento y almacenamiento 

## El siguiente procedmiento filtra la capa vectorial de polígonos de bosque
## pero se demora bastante (1h) -- no usaremos esta opción

# archivo_vect_bosque <- '../01_DatosOriginales/Nacional/Bosque_No_Bosque_2020_Vector.shp'
# file.exists(archivo_vect_bosque)
# 
# archivo_vect_bosque_filtrado <-  paste0('08_recorte_MSPA/', 'vect_bosques_filtrados.shp')
# 
# if (!file.exists(archivo_vect_bosque_filtrado)){
#   print(system.time(
#     gdalUtilities::ogr2ogr(src_datasource_name = archivo_vect_bosque, 
#                            dst_datasource_name = archivo_vect_bosque_filtrado,
#                            f = "ESRI Shapefile", dialect = 'sqlite',
#                            where = paste0("Cobertura!='No Monitoreado'") )
#     
#   )) #  3593.69s = 1h
# }


## El siguiente procedimiento crea una capa de cuadrado o envolvente (bounding box - bbox)
## de la capa vectorial de polígonos de bosque solamente

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

## Alternativa si el procedimiento anterior arrojó error:
system.time(valores_unicos <- ogrinfo(archivo_vect_bosque, dialect = "sqlite", 
         sql = paste0("select distinct ", 
                      "Cobertura", # Column name -- no spaces here
                      " from ", 
                      'Bosque_No_Bosque_2020_Vector', #"Bosque_No_Bosque_2020_Vector.shp",
                      " order by ", 
                      "Cobertura" # Column name -- no spaces here
                      )))# 3 mins

(categorias <- gsub('^.+= ', '', 
                    grep('\\(String\\)', valores_unicos, value = TRUE)))

sql
3:12 
Sys.time()

#https://gis.stackexchange.com/questions/77476/selecting-features-by-attributes-using-ogr2ogr
ogr2ogr -sql "SELECT * FROM infile WHERE ID IN [1,5,29]" outfile.shp infile.shp


## Extraer la extensión de las zonas donde hay bosque en el pais
(info_bosques <- gdalUtils::ogrinfo(archivo_vect_bosque_filtrado_bbox, so = TRUE, al = TRUE)) # 
(extent_bosques <- grep('Extent', info_bosques, value = TRUE))
(extent_sin_texto <- gsub('[a-zA-Z]|\\)|\\(|:|: |,|^ ', '', gsub('\\) - \\(', ' ', extent_bosques) ))
(extent_numerico <- as.numeric(strsplit(
  gsub('  ', ' ', extent_sin_texto), ' ')[[1]] )) # orden del resultado: xmn ymn xmx ymx
names(extent_numerico) <- c('xmn', 'ymn', 'xmx', 'ymx')
extent_numerico



## Convertir capa de bosques entre 0 y 1 con bosques a 2000
archivo_bosques <- '01_datos-originales/Bosque_No_Bosque_2020.tif'
archivo_perdida <- '01_datos-originales/Perdida_2001_2020.tif'
file.exists(archivo_bosques)
file.exists(archivo_perdida)

archivo_bosques_corte <- '02_bosques-cortados/Bosque_No_Bosque_2020_corte.tif'
archivo_perdida_corte <- '02_bosques-cortados/Perdida_2001_2020_corte.tif'

if(!file.exists(archivo_bosques_corte)){
  print(system.time(
    gdalUtilities::gdalwarp(srcfile = archivo_bosques, 
                            dstfile = archivo_bosques_corte, 
                            te = extent_numerico + c(-60, -60, 60, 60), # xmin ymin xmax ymax
                            #cutline = archivo_vect_bosque_filtrado_bbox, # Usar poligono de la capa 
                            #crop_to_cutline = TRUE, # Cortar por el polígono
                            ot = 'Byte', co = c("NBITS=8", "COMPRESS=DEFLATE"),
                            setci = TRUE
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


### 3. Filtrar capas y recategorizar  -----
# Identificar bosques de linea base al año 2000
archivo_bosques_inicial <- '03_bosques-filtrados\\Bosque_inicial_2000_corte.tif' ## Usar doble \ en este caso

raster_inicial <- raster(archivo_bosques_corte)

if(!file.exists(archivo_bosques_inicial)){
  
  if (!GDAL){
    ## Opcion R
    print(system.time(
      if (! file.exists(archivo_inicial_0) ){
        bosque_inicial2000 <- 1 + Which(raster_inicial == 3 | raster_inicial == 5)
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
               '--calc=" (1*(logical_or(A==3, A==5)))" ',
               '-A ', root2, '\\', archivo_bosques_corte,
               ' --outfile=', root2, '\\', archivo_bosques_inicial,
               ' --type=Byte --NoDataValue=255 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
        )))) # 1393.75s  
  }
}


archivo_bosques_inicial <- '03_bosques-filtrados\\Bosque_inicial_2000_corte.tif' ## Usar doble \ en este caso
archivo_perdida_inicial <- '03_bosques-filtrados\\Perdida_2001_2020_llenado0.tif'

## Cambiar datos nulos a 0 en capa raster de perdida de bosque
archivo_perdida_inicial <- '03_bosques-filtrados\\Perdida_2001_2020_llenado0.tif'
if (! file.exists(archivo_perdida_inicial) ){
  print(system.time(
    capa_perdida_0 <- gdalUtilities::gdal_translate(a_nodata = 255, # dato que se usa como nodata original, es decir que el 0 sea escrito
                                                    dst_dataset = archivo_perdida_inicial,
                                                    src_dataset = archivo_perdida_corte, 
                                                    ot = 'Byte')
  ))
}



### 3 Generar capas anuales ----

raster_inicial <- raster(archivo_bosques_inicial)
raster_perdida <- raster(archivo_perdida_inicial)
fechas_unicas <- (0:20)

## Iterar sobre los bosques 
## Las siguientes intrucciones se repetiran para cada año entre 2000 y 2020
for( f in 1:length(fechas_unicas)){ # f = 10 # }
  
  ## Definir valores en cada paso, como anio y nombres de capas
  (anio <- 2000+fechas_unicas[f]) ## entre 2000 y 2020
  anio2 <- fechas_unicas[f] ## entre 0 y 20
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- anio ', anio ) )
  (nombre_raster_bosque <- paste0('04_bosques-anuales', '/bosqueFecha_', anio, '.tif' ))
  
  
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
                '--calc=" A-(1*(logical_and(B!=0,B<=', anio2, ')))" ',
                '-A ', root2, '\\', archivo_bosques_inicial,
                ' -B ', root2, '\\', archivo_perdida_inicial , 
                ' --outfile=', root2, '\\', nombre_raster_bosque,
                ' --type=Byte --NoDataValue=255 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                # 
         ) 
      ))
      
      print(system.time( 
        system(intern = TRUE, cmd) # 2000s ~ 30m
      ))
      
    }
    print ( paste('   -- Guardado fecha ', anio ) )
  }
}




## Listar bosques tifs -----
folder_bosques <- '04_bosques-anuales/'
(archivos_bosques <- list.files(path = folder_bosques, 
                                pattern = '^bosqueFecha.+.tif$', full.names = TRUE))



## Iterar para cada capa. Recortaremos las capas de bosque a un extent mas pequeno
for (i in 1:length(archivos_bosques)){ # i = 2; j = 1
  (bosque.i <- archivos_bosques[i])
  (bosque.i2 <- basename(bosque.i))
  (bosque.resultado <- paste0('recl12_', basename(bosque.i)))
  (bosque.recorte <- paste0('05_bosques-reclasificados\\recl12_', basename(bosque.i)))
  
  (anio <- tools::file_path_sans_ext(gsub('.+_', '', bosque.i)))
  
  
  ## Iterar para cada capa. Recortaremos las capas de bosque a un extent mas pequeno
  
  
  
  ## Crear capa de bosque por anio reclasificando valores para GUIDOS (1; no bosque, 2:bosque) 
  if ( !file.exists(bosque.recorte) ){
    if (! GDAL){
      ## Hecho en R
      raster_bosque <- raster(bosque.i)
      raster_j <- raster_bosque + 1
      writeRaster(x = raster_j, filename = bosque.recorte)
      
      # plot(raster_j)
      # plot(plantilla_bosque)
      # plot(poligonos_seleccionados)
      
    } else {
      ## Hecho con gdal_calc.py
      # --projwin <ulx> <uly> <lrx> <lry> || xmn ymx xmx ymn 
      
      (comando <- gsub(fixed = TRUE, '/', '\\', 
                       paste0(execGDAL,
                              '--calc="1+A', '" -A ', root2, archivos_bosques[i], 
                              ' --outfile=', root2, bosque.recorte, 
                              ' --quiet', ' --type=Byte --NoDataValue=255 --co="COMPRESS=DEFLATE"' # UInt16
                              # '--projwin ', extent_corte['x', 'min'], " ", extent_corte['y', 'max'], " ", extent_corte['x', 'max'], " ", extent_corte['y', 'min']
                       ))
      ) # 447.31
      
      print(system.time( result <- system( comando , intern = TRUE) )) # 2157.89s 36m vs 1155.17 
      print(bosque.resultado)
     
      ## Ejecutar
      
    } #  447.31s 
  }
}







# cd C:\GuidosToolbox\MSPAstandalone
# mspa_win64.exe
# mspa_win64.exe -i C:\temp\Peru_riparios\04_calculoNacional\08_recorte_MSPA\recl12_bosqueFecha_2000.tif -o C:\temp\guidos_pe_2000.tif


####

# 
# ## Partir las capa raster en escenas mas pequeñas, menores a 10.000 × 10.000 pixels
# ## Crear poligonos con buffer sobrelapado para correr el analisis MSPA en tiles
# ## pequenos y evitar que los bordes sean identificados como fragmentados
# 
# (archivo_grid_poligonos <- paste0(outDirs, '/grid_finales.shp'))
# 
# capa_rectangular_bosque <- readOGR(archivo_vect_bosque_filtrado_bbox)
# 
# if(!file.exists(archivo_grid_poligonos) ){
#   
#   ## Crear planilla rectangular
#   plantilla_bosque <- raster(paste0(outDirs, '/', bosques_reclasificados[1]))
#   (n_columnas <- ceiling(plantilla_bosque@ncols / 10000)) # Parametro de guidos
#   (n_filas <- ceiling(plantilla_bosque@nrows / 10000)) #  Parametro de guidos
#   extent_bosque <- raster(nrow = n_columnas, ncol = n_filas, 
#                           ext = extent(plantilla_bosque)) # resolution = c(4500000,4500000)
#   extent_bosque[] <- 1:ncell(extent_bosque)
#   extent_bosque@crs <- plantilla_bosque@crs
#   grid_poligono <- rasterToPolygons(extent_bosque)
#   
#   plot(plantilla_bosque)
#   plot(grid_poligono, add = TRUE, border = 2)
#   centroides <- rgeos::gCentroid(grid_poligono, byid = TRUE)
#   text(x = centroides@coords[, c('x')], 
#        y = centroides@coords[, c('y')], 
#        labels = grid_poligono$layer, col = 2, cex = 2)
#   plot(capa_rectangular_bosque, add = TRUE, border = 3, lwd = 3)
#   
#   ## Identificar cuadriculas que intersectan los bosques
#   cuadriculas_comunes_int <- raster::intersect(grid_poligono, capa_rectangular_bosque)
#   cuadriculas_comunes <- grid_poligono[which(grid_poligono$layer %in% cuadriculas_comunes_int$layer),]
#   plot(cuadriculas_comunes, add = TRUE, border = 4, lwd = 2)
#   
#   
#   ## Creamos buffer para las zonas comunes
#   grid_buffer <- rgeos::gBuffer(cuadriculas_comunes, byid = TRUE, width = 1000)
#   plot(grid_buffer, add = TRUE, border = 5)
#   grid_buffer$buffid <- cuadriculas_comunes$layer
#   
#   
#   # Separamos cuadriculas con y sin superposición
#   cuadriculas_comunes$tipo <- 'cuadricula'
#   poligonos_finales <- grid_poligono
#   grid_intersect <- raster::intersect(grid_buffer, grid_poligono)
#   
#   grid_bordes <- grid_intersect[!(grid_intersect$layer.1 == grid_intersect$layer.2 ), ]
#   
#   plot(plantilla_bosque)
#   plot(grid_intersect, add = TRUE, border = 2)
#   plot(grid_bordes, add = TRUE, border = 2)
#   plot(grid_poligono[1, ], add = TRUE, border = 2)
#   
#   plot(plantilla_bosque)
#   plot(grid_intersect, add = TRUE, border = 4)
#   
#   grid_bordes$idnx <- apply(grid_bordes@data[, c('layer.1', 'layer.2')], MARGIN =1, 
#                             FUN = function(x) { 
#                               paste0( x[which.min(x)], 'x', x[which.max(x)])
#                             })
#   grid_bordes@data
#   grid_bordes$idnx
#   
#   poligonos_finales <- cuadriculas_comunes
#   IDpoligonos <- unique(grid_bordes$idnx)
#   plot(grid_intersect, add = FALSE, border = 1)
#   
#   # areas_revisar <- raster::area(grid_bordes) / (1000*1000)
#   # table(round(areas_revisar))
#   
#   for(i in 1:length(IDpoligonos)){ # i = 3 
#     IDi <- IDpoligonos[i]
#     
#     sel_poligonos <- grid_bordes[which(grid_bordes$idnx == IDi), ]
#     sel_poligonos@data
#     
#     area <- sum(raster::area(sel_poligonos) / (1000*1000))
#     
#     IDcomparar <- setdiff(IDpoligonos, IDi)
#     approved <- ifelse(area > 100, TRUE, FALSE) # quitamos los bordes pequeños circulares
#     
#     print(paste(IDi, nrow(sel_poligonos), area, approved))
#     
#     if( approved){
#       
#       plot(sel_poligonos, add = TRUE, border = 4)
#       #Sys.sleep(4)
#       #plot(sel_poligonos, add = TRUE)
#       sel_poligonos@data$layer <- IDi
#       sel_poligonos@data$tipo <- 'interseccion'
#       sel_poligonos@data <- sel_poligonos@data[, c('layer', 'tipo')]
#       poligonos_finales <- bind(poligonos_finales, sel_poligonos)
#       poligonos_finales@data
#     } else {
#       plot(sel_poligonos, add = TRUE, border = 2)
#       next
#     }
#   }
#   
#   plot(plantilla_bosque)
#   plot(poligonos_finales, add = TRUE, border = 2)
#   
#   writeOGR(obj = poligonos_finales, overwrite_layer = TRUE,
#            dsn = outDirs, 
#            layer = 'cuadricula_corte_GUIDOS', 
#            driver = 'ESRI Shapefile')
# }

#' 
#' ## Recortar las capas anuales de bosque por cada poligono y
#' ## y corregimos valores para usarlos en GUIDOS (bosque = 2, no bosque = 1)
#' (bosques_reclasificados <- list.files(path = outDirs, pattern = 'recl12.+[0-9]{4}.tif$'))
#' poligonos_finales <- readOGR(paste0(outDirs, '/cuadricula_corte_GUIDOS.shp'))
#' IDunicos <- unique(poligonos_finales@data)
#' 
#' 
#' ## Iterar para cada capa. Recortaremos las capas de bosque a un extent mas pequeno
#' for (i in 1:length(bosques_reclasificados)){ # i = 2; j = 1
#'   (bosque.i <- bosques_reclasificados[i])
#'   raster_bosque <- paste0(outDirs, '/', bosque.i)
#'   (bosque.i2 <- basename(bosque.i))
#'   (bosque.resultado <- paste0('recl12_', basename(bosque.i)))
#'   (bosque.recorte <- paste0(outDirs, '/corte_bosque_', basename(bosque.i)))
#' 
#'   (anio <- tools::file_path_sans_ext(gsub('.+_', '', bosque.i)))
#' 
#'   # poligonos_seleccionados <- poligonos_finales[
#'   #   which(poligonos_finales$layer == IDunicos$layer[j] &
#'   #           poligonos_finales$tipo == IDunicos$tipo[j]), ]
#'   # poligonos_seleccionados@bbox
#'   # (extent_numerico <- poligonos_seleccionados@bbox[c(1, 2, 3, 4)]) # Asegurar orden # xmin ymin xmax ymax
#'   # #plot(raster(bosque.i)); plot(poligonos_seleccionados, add = TRUE, border = 2)
#' 
#' 
#'   ## Iterar sobre cada polígono
#'   for (j in 1:nrow(IDunicos)){ # j = 1
#'     (bosque_corte.i <- paste0('recorte-', anio, '_',  IDunicos$layer[j], '_', IDunicos$tipo[j], '.tif'))
#'     raster_corte.i <- paste0(outDirs, '/', bosque_corte.i)
#'     poligonos_seleccionados <- poligonos_finales[
#'       which(poligonos_finales$layer == IDunicos$layer[j] &
#'               poligonos_finales$tipo == IDunicos$tipo[j]), ]
#' 
#'     plot(poligonos_seleccionados, add = TRUE, border = 3, lwd = 3)
#'     extent_corte <- poligonos_seleccionados@bbox
#'     extent_vector <- c(extent_corte['x', 'min'], extent_corte['y', 'max'],
#'                        extent_corte['x', 'max'], extent_corte['y', 'min'])
#' 
#' 
#'     if(!file.exists(raster_corte.i)){
#'       print(system.time(
#'         gdalUtilities::gdalwarp(srcfile = raster_bosque,
#'                                 dstfile = raster_corte.i,
#'                                 te = extent_vector,  # xmin ymin xmax ymax, dando 2 pixeles de tolerancia + c(-60, -60, 60, 60)
#'                                 #cutline = archivo_vect_bosque, cwhere = paste('"Cobertura"="No Monitoreado"'),
#'                                 #ot = 'Byte',
#'                                 co = c("NBITS=2", "COMPRESS=DEFLATE"),
#'                                 dstnodata = 999,
#'                                 crop_to_cutline = TRUE)
#'       )) # 400 s ~ 67 minutos
#'     }
#' 
#' 
#'     if (FALSE){
#' 
#' 
#'       ## Crear capa de bosque por anio reclasificando valores para GUIDOS
#'       if (!file.exists(nombre_raster_bosque)){
#'         if (! GDAL){
#'           ## Hecho en R
#'           raster_j <- raster::crop(raster_bosque, poligonos_seleccionados) + 1
#'           writeRaster(x = raster_j, filename = bosque_corte.i)
#' 
#'           # plot(raster_j)
#'           # plot(plantilla_bosque)
#'           # plot(poligonos_seleccionados)
#' 
#' 
#'         } else {
#'           ## Hecho con gdal_calc.py
#'           # --projwin <ulx> <uly> <lrx> <lry> || xmn ymx xmx ymn
#' 
#'           (comando <- gsub('/', '',
#'                            paste0(execGDAL,
#'                                   '--calc="1+A', '" -A ', root2, outDirs, '\\', bosque.i,
#'                                   ' --outfile=', root2, outDirs, '\\',bosque_corte.i,
#'                                   ' --quiet'
#'                                   # '--projwin ',
#'                                   # extent_corte['x', 'min'], " ",
#'                                   # extent_corte['y', 'max'], " ",
#'                                   # extent_corte['x', 'max'], " ",
#'                                   # extent_corte['y', 'min']
#'                                   #' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
#'                            ))
#'           )
#' 
#'           ## Ejecutar
#'           print(system.time( result <- system( comando , intern = TRUE) ))
#' 
#'           print(bosque_corte.i)
#'         }
#'       }
#'     }
#' 
#'   }
#' }
#' 
#' 


# ## Eliminar recortes nulos, o con ningun pixel con bosque
# outDir2 <- 'C:/GuidosToolbox/data/bper'
# tifs_recortes <- list.files(path = outDir2, pattern = 'recorte.+.tif$', full.names = TRUE)
# for( i in 1:length(tifs_recortes)){ # i = 1
#   # i = which( grepl( x = tifs_recortes ,'recorte-2020_19_cuadricula.tif'))
#   stats_raster <- capture.output(gdalinfo(tifs_recortes[i], stats = TRUE))
#   (stdev <- gsub(" ", "", grep('STATISTICS_STDDEV', x = stats_raster, value = TRUE)))
#   print(paste(i, stdev))
#   
#   if(length(stdev) == 1){
#     if (stdev == 'STATISTICS_STDDEV=0'){
#       tif_borrar <- tools::file_path_sans_ext(basename(tifs_recortes[i]))
#       archivos_borrar <- list.files(path = outDir2, pattern = tif_borrar, full.names = TRUE)
#       sapply(FUN = file.remove, archivos_borrar)
#       print(paste0('  Archivo borrado: ', tif_borrar))
#     }
#   }
#   # where = "Cobertura!='No Monitoreado'",
# }


#### Errores ----
## Envolvente de bosques

# GdalUtillities: 1.2.1
# GdalUtils: 2.0.3.2

# Error in gdal_utils("vectortranslate", src_datasource_name, dst_datasource_name,  : 
#                       gdal_utils vectortranslate: an error occured
#                     In addition: Warning message:
#                       In CPL_gdalvectortranslate(source, destination, options, oo, doo,  :
#                                                    GDAL Error 1: In ExecuteSQL(): sqlite3_prepare_v2(SELECT extent(geometry) FROM Bosque_No_Bosque_2020_Vector WHERE Cobertura <> 'No Monitoreado'):
#                                                    no such function: extent
#                                                  