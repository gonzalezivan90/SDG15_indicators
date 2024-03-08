gdalPaths <- function(depth = 2, drives = c('C'), latestQ = TRUE, help = FALSE){
  # depth = 3; drives = c('C'); help = FALSE; latestQ = TRUE; i = 1; j = 1; k = 1
  if (help){
    cat('\n\nThis function will try to find the QGIS folders and locate gdal axecutables inside them.',
        'If they are found, the function will return a list with the instrucctiones to be use on the command line.\n',
        "\nArgs:\n\tdepth: Numeric. Number of folders to search recursiverly under the root or drive. Default = 3",
        "\n\tdrives: String. The root or drives locations. Can be single or multiple values. Case sensitive. Default = c('C')",
        "\n\tlatestQ: Logical. Use the latest QGIS version in the case there's several?. Default = TRUE",
        "\n\thelp: Logical. Display or noth this message. Default = FALSE",
        "\nUsage:\n\tgdalPaths(depth = 3, drives = c('C'), help = FALSE)\n\n"   )
    
    ## 
    ans <- 'Run gdalPaths(help = FALSE) to found GDAL in your computer'
    
  } else {
    
    found <- FALSE
    arg1_exists <- arg2_exists <- FALSE
    ans <- 'no GDAL found'
    
    if (length(drives) == 0){
      error("'drives' must be a vector of different hard drives. For example: drives = c('C', 'D') or drives = c('C')")
    }
    
    for(i in 1:length(drives)){ # i = 1
      (drive <- paste0(drives[i],':/'))
      
      #for(j in 1:depth){ # j = 1
      folders <- new_folders <-  unique(c( paste0(drives[i],':/', c('Program Files', '.')), 
                                           gsub('//', '/', list.dirs(drive, recursive = FALSE) )) )
      
      if(depth >= 2 ){ # j = 2
        temp_folder <- folders
        for(n in 2:depth){
          new_folders <- unname(unlist(sapply(temp_folder, list.dirs, recursive = FALSE)))
          temp_folder <- c(temp_folder, new_folders)
        }
        folders <- temp_folder
      }
      # print(tail(temp_folder))
      # print(head(folders))
      # print(tail(folders))
      
      ## Prioritize QGIS folders
      qgisPos <- grep('QGIS', toupper( folders ))
      #grep('QGIS', toupper( folders ), value = TRUE)
      
      if ( length(qgisPos) >= 1){
        if(latestQ){  qgisPos <- rev(qgisPos) }
        folders <- c(folders[qgisPos], folders[-qgisPos])
        #(qgis <- dirs1[tail(qgisPos, 1)])
      }
      
      folders <- unique(gsub('/./', '/', folders))
      #head(folders)
      
      
      for (k in 1:length(folders)){ # k = 1
        (folder <- folders[k])
        
        # if(basename(folder) == '.'){
        #   dirs1 <- folder
        # } else {
        #   dirs1 <- list.dirs(path = folder, recursive = FALSE)
        # }
        
        OSGeo4W <- paste0(folder, '/OSGeo4W.bat')
        (arg1_exists <- file.exists(OSGeo4W))
        
        if(arg1_exists) {
          arg1 <- OSGeo4W
          o4w_env <- paste0(folder, '/bin/o4w_env.bat')
          (arg2_exists <- file.exists(o4w_env))
          if (arg2_exists){
            arg2 <- o4w_env
          } else {
            py3_env <- paste0(folder, '/py3_env.bat')
            (arg2_exists <- file.exists(py3_env))
            if (arg2_exists){
              arg2 <- py3_env
            }
          }
          
          if ( arg1_exists & arg2_exists ){
            #print(paste(i, j, k, drive, folder, arg1, arg2))
            print(paste('Analizing ', folder))
            found <- TRUE
            
            arg1_bslas <- strsplit(arg1, '/')[[1]]
            arg1_bslas[grep(' ', arg1_bslas)] <- paste0('"', arg1_bslas[grep(' ', arg1_bslas)] ,'"') 
            arg1_bslas <- paste0(paste0(arg1_bslas, collapse = '/'))
            
            arg2_bslas <- strsplit(arg2, '/')[[1]]
            arg2_bslas[grep(' ', arg2_bslas)] <- paste0('"', arg2_bslas[grep(' ', arg2_bslas)] ,'"') 
            arg2_bslas <- paste0(paste0(arg2_bslas, collapse = '/'))
            
            execGDALcalc <- gsub(fixed = TRUE, '/', '\\', paste0(arg1_bslas, ' ', arg2_bslas, ' && ', arg2_bslas, ' && gdal_calc') ) 
            (gdalcalclog <- (system(paste0(execGDALcalc, ' --help'), intern = TRUE)))
            
            ## Asignar TRUE si se encuentra respuesta del sistema
            (gdalcalcworks <- ifelse(any(grep('gdal_calc.py', gdalcalclog)), TRUE, FALSE))
            
            (execGDALsieve <- gsub('gdal_calc', 'gdal_sieve', execGDALcalc))
            (gdalsievelog <- suppressWarnings(system(paste0(execGDALsieve, ''), intern = TRUE))) ## Esta opción no necesita "--help"
            (gdalsieveworks <- ifelse(any(grep('Usage: gdal_sieve', gdalsievelog)), TRUE, FALSE))
            
            
            (execGDALproxy <- gsub('gdal_calc', 'gdal_proximity', execGDALcalc))
            (gdalproxylog <- suppressWarnings(system(paste0(execGDALproxy, ''), intern = TRUE))) ## Esta opción no necesita "--help"
            (gdalgdalproxyworks <- ifelse(any(grep('Usage: gdal_proximity.py', gdalproxylog)), TRUE, FALSE))
            
            
            ans <- list(execGDALcalc, gdalcalcworks, execGDALsieve, gdalsieveworks, execGDALproxy, gdalgdalproxyworks)
            break()
          }
          if (found) {break()} 
        }
        if (found) {break()} 
      }
      if (found) {break()} 
    }
  }
  # end while
  return(ans)
}
# 
# gdalPaths(help = TRUE)
# gdal <- gdalPaths(depth = 3, drives = c('C'), latestQ = TRUE, help = FALSE)
# 
# 
# # Evaluar si tenemos acceso a gdal_calc --
# # Validar si tenemos la libreria gdal_calc
# #  -- Dejar un espacio despues de gdal_calc --
# # execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat
# #                    'py3_env.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
# #                    '&& py3_env.bat ', # Mantener espacio. Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
# #                    '&& gdal_calc ') # Llamado de gdal_calc
# 
# ## Instrucción en QGIS 3.22.6
# ## (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && gdal_calc'))
# ## (execGDAL <- paste0('C:\"Program Files"\"QGIS 3.20.1"\OSGeo4W.bat C:\"Program Files"\"QGIS 3.3.20.1"\bin\o4w_env.bat && C:\"Program Files"\"QGIS 3.3.20.1"\bin\o4w_env.bat && gdal_calc'))
# 
# (execGDAL <- paste0('C:/OSGeo4W64/OSGeo4W.bat py3_env.bat && py3_env.bat && gdal_calc'))
# 
# ## Mirar la respuesta del sistema a conexion a gdal_calc
# (gdalcalc <- (system(paste0(execGDAL, ' --help'), intern = TRUE)))
# 
# ## Asignar TRUE si se encuentra respuesta del sistema
# (GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))
# 
# (execGDALsieve <- gsub('gdal_calc', 'gdal_sieve', execGDAL))
# (gdalsieve <- (system(paste0(execGDALsieve, ''), intern = TRUE))) ## Esta opción no necesita "--help"
# 
# (execGDALproxy <- gsub('gdal_calc', 'gdal_proximity', execGDAL))
# (gdalproxy <- (system(paste0(execGDALproxy, ''), intern = TRUE))) ## Esta opción no necesita "--help"
# 
# 
# (gdal1 <- paste0('C:/OSGeo4W64/OSGeo4W.bat py3_env.bat && py3_env.bat o-help'))
# (gdal2 <- (system(paste0(gdal1, ' --help'), intern = TRUE)))
# 
# (execGDAL <- paste0('C:/OSGeo4W64/OSGeo4W.bat py3_env.bat && py3_env.bat && gdal_calc'))
# 
# ##Aca la configuración para QGIS3.14, pero abajo hay instrucciones para otras versiones. #CAMBIAR ---
# 
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat',   'py3_env.bat && py3_env.bat && gdal_calc '))
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.20.1"\\OSGeo4W.bat', 'C:\"Program Files"\\"QGIS 3.3.20.1"\\bin\\o4w_env.bat && C:\\"Program Files"\\"QGIS 3.3.20.1"\\bin\\o4w_env.bat && gdal_calc '))
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat ','C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat && gdal_calc '))
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.28.1"\\OSGeo4W.bat ','C:\\"Program Files"\\"QGIS 3.28.1"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.28.1"\\bin\\o4w_env.bat && gdal_calc '))
# (execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.22.12"\\OSGeo4W.bat','C:\\"Program Files"\\"QGIS 3.22.12"\\bin\\o4w_env.bat && ', 'C:\\"Program Files"\\"QGIS 3.22.12"\\bin\\o4w_env.bat && gdal_calc '))
# (execGDAL <- paste0('E:\\OSGeo4W.bat E:\\bin\\o4w_env.bat && E:\\bin\\o4w_env.bat && gdal_calc '))
# 
# execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat py3_env.bat && py3_env.bat && gdal_calc ') 
# 
# 
# 
# execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.14"\\OSGeo4W.bat ', # Mantener espacio al final. Ubicación del archivo OSGeo4W.bat que es el orquestador de ejecutables GIS00
#                    'py3_env.bat ', # Mantener espacio. Ubicación de el ejecutable que configura entorno de python. Puede llamarse tambien o4w_env.bat
#                    '&& py3_env.bat ', # Mantener espacio al . Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
#                    '&& gdal_calc ') # Llamado de gdal_calc, Dejar espacio al final
# 
