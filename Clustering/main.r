library (openxlsx)
library (plyr)
library (lettercase)
library (stringdist)
library (stringr)
library (outliers)
library (plotly)
library(knitr)
library(rmarkdown)
library(cluster)
library(factoextra)
library (clValid)


Sys.setenv(R_ZIPCMD="C:/Rtools/bin/zip") 

dspath <-paste0(getwd(),"/")

source("./src/lib/log_error.r",encoding="UTF-8")
source('./src/lib/proper_case.r',encoding="UTF-8")
source('./src/lib/export.r',encoding="UTF-8")
source('./src/lib/na_functions.r',encoding="UTF-8")
source ("./src/clustering.r")
source ("./src/informes.r")


fnOriginal <- "data/Vehiculos_Comercializados_ultimo_quinquenio"


dfO <- recuperar_de_cache(fnOriginal)

variablesEstudio <- c("Largo", "Ancho", "Altura","Batalla")
docxEstudio (dfO, variablesEstudio, 1)

variablesEstudio <- c("Largo", "Altura", "Altura.libre.delantera", "Batalla", "Masa")
docxEstudio (dfO, variablesEstudio, 2)

variablesEstudio <- c("Largo", "Altura", "Altura.libre.delantera", "Batalla", "Cilindrada")
docxEstudio (dfO, variablesEstudio, 3)






