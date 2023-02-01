
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


## Cargar librerías
library('raster')
library('rgdal')
library('gdalUtilities')
library('gdalUtils')
library('sp')
library('foreign')


