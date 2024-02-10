
## Definir lista de librerias
packages <- c('raster', 'rgdal', 'gdalUtilities', 'gdalUtils', 'sp', 'foreign')

## Instalar librerias
sapply(packages, install.packages)

## Revisar librerías instaladas
installedPackages <- installed.packages()

## Contrastar si los paquetes requeridos ya están instalados
packages %in% rownames(installedPackages)


## Opcion para intalar gdalUtils
install.packages("devtools")
devtools:::install_github("gearslaboratory/gdalUtils")

## If there's a problem with gdalUtils
# https://github.com/gearslaboratory/gdalUtils >> CODE (green button) >> download zip
## Unzip the file in C:/path_to_unzziped_folder_pkg or any path
# devtools:::install("C:/path_to_unzziped_folder_pkg")

## Cargar librerías
library('raster')
library('rgdal')
library('gdalUtilities')
library('gdalUtils')
library('sp')
library('foreign')
