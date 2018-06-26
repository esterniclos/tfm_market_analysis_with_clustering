# main.r

# libraries <- c("openxlsx","plyr","lettercase","stringdist","stringr", "outliers","qpcR")
# install.packages(libraries, dependencies = TRUE)


library (openxlsx)
library (plyr)
library (lettercase)
library (stringdist)
library (stringr)
library (outliers)
library (qpcR)
library (plotly)

Sys.setenv(R_ZIPCMD="C:/Rtools/bin/zip") 

dspath <-paste0(getwd(),"/")


source("./src/lib/log_error.r",encoding="UTF-8")
source('./src/lib/proper_case.r',encoding="UTF-8")
source('./src/lib/export.r',encoding="UTF-8")
source('./src/lib/outliers.r',encoding="UTF-8")
source('./src/lib/filter_num.r',encoding="UTF-8")
source('./src/lib/alternatives.r',encoding="UTF-8")
source('./src/lib/na_functions.r',encoding="UTF-8")

source('./src/visualizacion/outliers_plots.r',encoding="UTF-8")


source("./src/limpieza_normalizacion_idae.r", encoding="UTF-8")
source("./src/limpieza_normalizacion_eea.r",encoding="UTF-8")
source("./src/limpieza_normalizacion_vpic.r",encoding="UTF-8")

source("./src/transformacion.r",encoding="UTF-8")


log_error_init() # Inicializar el log de errores


