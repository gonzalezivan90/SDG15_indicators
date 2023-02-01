## Asegurarse que RTools esta instalado para nuestra versión de R
# https://cran.r-project.org/bin/windows/Rtools

## Definir lista de librerias
packages <- c('devtools', 'rgdal', 'raster',  'gdalUtilities', 'gdalUtils', 'sp', 'foreign')

## Revisar librerías instaladas
installedPackages <- rownames(installed.packages())


## Instalar librerias si no están instaladas
sapply(packages, function(x) {
  if (!x %in% installedPackages){
    print(paste('Installing ', x))
    eval(expr = paste0("install.packages(", x, ")")) 
  }
})


## Contrastar si los paquetes requeridos ya están instalados
sapply(packages, function(x) {x %in% installedPackages})


## Opcion para intalar gdalUtils
devtools:::install_github("gearslaboratory/gdalUtils")


## Cargar librerías
library('raster')
library('rgdal')
library('gdalUtilities')
library('gdalUtils')
library('sp')
library('foreign')

