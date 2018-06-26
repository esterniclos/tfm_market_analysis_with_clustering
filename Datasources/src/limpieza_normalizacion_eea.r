# EEA

fnEeaSinNormalizar <- "01_EEA/EEA_unificado"
fnEeaSinNormalizarEs <- "01_EEA/EEA_unificado_ES"
fnEeaNormalizado <- "02_Normalizados/EEA_normalizado"
fnEeaLimp <- "03_Limpieza/EEA_limpieza_y_seleccion"
fnEeaLimp2 <- "03_Limpieza/EEA_limpieza_y_seleccion_sin_atipicos"
fnEeaCorreccionModelos <- "debug/EEA_corr_modelos.csv"
fnEEaAtipicos <- "debug/EEA_atipicos"
fnEEaModelosAtipicos <- "debug/EEA_modelos_descartados_atipicos"

#####################################################
## cargar_dforiginal_eea
#####################################################
cargar_dforiginal_eea <- function (filename){
  df <- read.table(filename, sep="\t", header = TRUE, fill =TRUE)
  # Algunos nombres de las columnas, presentan distintas grafías. Se ponen todas en mayúsculas
  colnames(df)<- gsub ("__","_",gsub('\\.', "_", str_allcaps(colnames(df))))
  return (df)
}
#####################################################
## unificar_datasets
#####################################################
unificar_datasets <- function (){

  # Cargar los datos:
  datasetv6 <- cargar_dforiginal_eea("01_EEA/CO2_passenger_cars_v6.csv")
  datasetv8 <- cargar_dforiginal_eea("01_EEA/CO2_passenger_cars_v8.csv")
  datasetv10 <- cargar_dforiginal_eea("01_EEA/CO2_passenger_cars_v10.csv")
  datasetv12 <- cargar_dforiginal_eea("01_EEA/CO2_passenger_cars_v12.csv")
  datasetv13 <- cargar_dforiginal_eea("01_EEA/CO2_passenger_cars_v13.csv")

   
  
  dfeea <- rbind.fill (datasetv6, datasetv8)
  dfeea <- rbind.fill (dfeea,datasetv10)
  dfeea <- rbind.fill (dfeea, datasetv12)
  dfeea <- rbind.fill (dfeea, datasetv13)
  
  print (paste("El total de veh?culos en estos datasets es de ",
               nrow(dfeea)))
  

  exportacion (fnEeaSinNormalizar, dfeea)
  return (dfeea)
}

#####################################################
## solo_registrados_españa
#####################################################
solo_registrados_españa <- function  (dfes) {
  filas_registros_ES <- (dfes$MS == "ES")
  dfes <- dfes [filas_registros_ES, ]
  
  print (paste0("El total de registros de España es ", nrow(dfes)))
  
  print (paste0 ("El total de marcas en España es ", length(unique (dfes$MK))))
  print (paste0 ("El total de modelos en España es ", length(unique (dfes$CN))))
  
  
  # registros con VE erroneo
  filas <- which(nchar (dfes$VE) > 100)
  dfes<- dfes [-filas, ]
  
  exportacion (fnEeaSinNormalizarEs, dfes)
  return (dfes)
}

#####################################################
## ajuste_inicial_modelos
#####################################################
ajuste_inicial_modelos <- function (dfEEAnorm){
  # bmw cooper => minicooper
  v1 <- which (grepl("bmw", dfEEAnorm$Marca, ignore.case = TRUE) & 
                 grepl ("cooper", dfEEAnorm$Modelo, ignore.case =TRUE))
  dfEEAnorm$Marca [v1] <- "BMW"
  
  # DS, antepone ds.
  v1 <- which (grepl("DS", dfEEAnorm$Marca, ignore.case = TRUE) & 
                 (! grepl ("DS", dfEEAnorm$Modelo, ignore.case =TRUE)))
  dfEEAnorm$Modelo [v1] <- paste0 ("DS", dfEEAnorm$Modelo [v1])                    
  # Regularizar el Santa Fe
  v1 <- which (grepl("Hyundai", dfEEAnorm$Marca, ignore.case = TRUE) & 
                 ( grepl ("Santa", dfEEAnorm$Modelo, ignore.case =TRUE)))
  dfEEAnorm$Modelo [v1]<- gsub ("Fe Fe", "Fe", gsub( "Santa", "Santa Fe", dfEEAnorm$Modelo[v1]))
  #
  
  v1 <- which (grepl("Mercedes", dfEEAnorm$Marca, ignore.case = TRUE) & 
                 grepl ("^(a|b|c|d|e) ", dfEEAnorm$Modelo , ignore.case = TRUE))
  dfEEAnorm$Modelo [v1] <- paste0 ("Clase ", dfEEAnorm$Modelo [v1]) 
  return (dfEEAnorm)
}


#####################################################
## normalizar_eea
#####################################################
normalizar_eea <- function (dfes, marcas_validadas, modelos_validados){
  
  dfEEAnorm <- dfes # Inicializar dataset
  
  print ("Normalización de la marca y modelo")

  print ("Renombrado de columnas")
  colnames.dfEEAnorm <- c(
    "Id","Estado Miembro", 
    "Fabricante", "Fabricante OEM", "Fabricante MS", 
    "Homologacion Tipo", "Tipo", "Variante", "Version",
    "Marca", "Modelo", "Categoria","Registros en estado miembro",
    "EmisionesCO2_g_km", "Masa",
    "Batalla","Ancho eje tracción", "Ancho eje secundario", 
    "Combustible","Monofuel",
    "Cilindrada","Kw motor eléctrico", "Consumo motor eléctrico wh_km", 
    "Tecnicas reduccion", "Reduccion cuantificada de las emisiones",
    "Grupo empresarial","IdFabricante"
  )
  
  # Traducción de las columnas:
  for (c in 1:length(colnames(dfEEAnorm))){
    print (paste( colnames(dfEEAnorm)[c], "---> ", colnames.dfEEAnorm[c]))
  }
  
  colnames(dfEEAnorm) <- colnames.dfEEAnorm
  
  # Pasar las columnas a numéricas
  print ("Normalización de columnas numéricas")
  col_numericas <- c( "Id","Registros en estado miembro", "EmisionesCO2_g_km", "Masa",
                     "Batalla","Ancho eje tracción", "Ancho eje secundario",
                     "Cilindrada","Kw motor eléctrico", "Consumo motor eléctrico wh_km")

  # Pasar a numéros o aplanar versión
  for (c in 1:ncol(dfEEAnorm)){
    # Las que son numéricas se normalizan. 
    if (colnames(dfEEAnorm)[c] %in% col_numericas) {
      print (paste0("Conversión a número columna ", c ," ",colnames(dfEEAnorm)[c]))
      dfEEAnorm[,c] <- as.numeric (dfEEAnorm[,c])
    }
    else {    # Las que no lo son, se unifica la grafía.
      print (paste0("Corrección grafía columna ",c, " ",colnames(dfEEAnorm)[c]))
      dfEEAnorm[ ,c]<-proper_case (dfEEAnorm[ ,c])
    }
  }
  dfEEAnorm <- ajuste_inicial_modelos(dfEEAnorm)
  # Traducción de combustibles
  print ("Traducción de combustibles.")
  dfEEAnorm$Combustible <- gsub("Petrol", "Gasolina", dfEEAnorm$Combustible)
  dfEEAnorm$Combustible <- gsub("Diesel", "Diésel", dfEEAnorm$Combustible)
  dfEEAnorm$Combustible <- gsub("Electric", "Eléctrico", dfEEAnorm$Combustible)
  dfEEAnorm$Combustible <- gsub("Diesel-Electric", "Híbrido diésel", dfEEAnorm$Combustible)
  dfEEAnorm$Combustible <- gsub("Petrol-Electric", "Híbrido gasolina", dfEEAnorm$Combustible)
  dfEEAnorm$Combustible <- gsub("Lpg", "GLP", dfEEAnorm$Combustible)
  dfEEAnorm$Combustible <- gsub("e85", "Etanol", dfEEAnorm$Combustible)
  
  print ("Traducción Motor")
  dfEEAnorm$Monofuel <- gsub("m$", "Monofuel", dfEEAnorm$Monofuel)
  dfEEAnorm$Monofuel <- gsub("b$", "Bifuel", dfEEAnorm$Monofuel)
  dfEEAnorm$Monofuel <- gsub("e$", "Eléctricos", dfEEAnorm$Monofuel)
  dfEEAnorm$Monofuel <- gsub("f$", "Etanol", dfEEAnorm$Monofuel)
  
  dfEEAnorm <- eeaCorreccionesIniciales(dfEEAnorm)
  
  # añadir la alternativa IDAE y si están o no incluidos en el modelo IDAE:
  v1 <- alternativa_idae_modelos (dfEEAnorm$Marca, dfEEAnorm$Modelo,
                                   marcas_validadas, modelos_validados)
  dfEEAnorm$Alternativa_Idae <- v1
  

  exportacion (fnEeaNormalizado, dfEEAnorm)
  return (dfEEAnorm)
  
}

#####################################################
## Correcciones iniciales EEA
#####################################################
eeaCorreccionesIniciales <- function  (dfEEAnorm) {
  filas <- (dfEEAnorm$Marca == "Land Rover")
  dfEEAnorm[filas, which ()] <- "Landrover"
  return (dfEEAnorm)
}


#####################################################
## limpieza_eea
#####################################################

limpieza_eea <- function (dfEEAnorm){
  
  dfEELimp <- dfEEAnorm
  
#   columnas <- c("Tipo","Variante","Version","Marca","Modelo", "EmisionesCO2_g_km",
#  "Masa","Batalla","Ancho.eje.tracción","Ancho.eje.secundario",
#  "Combustible","Monofuel","Cilindrada","Kw.motor.eléctrico",
#  "Consumo.motor.eléctrico.wh_km","Modelo_IDAE","Alternativa_Idae")
#  dfEELimp <- dfEELimp[,colnames(dfEELimp) %in% columnas]

    # Modelos que pertenecen al modelo Idae:
  dfEELimp <- dfEELimp[!is.na(dfEELimp$Alternativa_Idae),]
  
  # Eliminar valores (son erróneos) de Cilindrada en eléctricos
  es_electrico <- dfEELimp$Combustible == "Eléctrico"
  es_monofuel <- (dfEELimp$Monofuel == "Monofuel")
  dfEELimp$Cilindrada[es_electrico] <- NA
  # Eliminar valores (son erróneos) de potencia eléctrica en monofuel
  dfEELimp$Kw.motor.eléctrico [!es_electrico] <- NA
  dfEELimp$Consumo.motor.eléctrico.wh_km[!es_electrico] <- NA
  
  
 
  exportacion (fnEeaLimp, dfEELimp)
  return (dfEELimp)
}

#####################################################
## eeaEstudioAtipicos
#####################################################


eeaEstudioAtipicos <- function (dfEELimp){
  
  dfr<- info_atipicos(dfEELimp$Batalla, "Batalla")
  dfr <- rbind (dfr, info_atipicos(dfEELimp$Masa, "Masa"))
  
  # Cilindradas en no_electrícos
  es_motor_electrico <- grepl ("Eléctrico", dfEELimp$Combustible)
  Cilindrada_atmosferico <- dfEELimp$Cilindrada [!es_motor_electrico]
  Cilindrada_atmosferico <- filterNum(Cilindrada_atmosferico,min=30)
  dfr <- rbind (dfr, info_atipicos(Cilindrada_atmosferico, "Cilindrada combustibles fósiles"))
  # kw motor eléctricos
  KW_motor_electrico <- dfEELimp$Kw.motor.eléctrico [es_motor_electrico]
  KW_motor_electrico <- filterNum(KW_motor_electrico,min=5)
  dfr <- rbind (dfr, info_atipicos(KW_motor_electrico, "Kw Motor eléctricos"))
  
  
  exportacion ( fnEEaAtipicos, dfr)
  return (dfr)
}

#####################################################
## eea_correccion_atipicos
#####################################################

eea_correccion_atipicos <- function (dfEELimp){
  
  dfEEALimp2 <- dfEELimp
  
  modelos_a_descartar <- (dfEELimp$Batalla > 3241) # Descartar por exceso de batalla -> furgonetas
  descartados <- dfEELimp[modelos_a_descartar,]
  
  modelos_a_descartar <- (dfEELimp$Cilindrada > 2500)
  descartados <- rbind(descartados, dfEELimp[modelos_a_descartar,])
  
  modelos_a_descartar <- (dfEELimp$Masa > 2400)
  descartados <- rbind(descartados, dfEELimp[modelos_a_descartar,])
  
  exportacion (fnEEaModelosAtipicos, descartados)
  
  dfEEALimp2 <-  dfEELimp[!modelos_a_descartar,]
  
  # Eliminación de registros duplicados
  dfEEALimp2 <- dfEEALimp2[! duplicated(dfEEALimp2),  ]
  
  exportacion ( fnEeaLimp2, dfEEALimp2)
  return (dfEEALimp2)
  
}

#####################################################
## MAIN
#####################################################

# Creación de los datasets:
dfeea<- unificar_datasets ()
dfes <- solo_registrados_españa (dfeea)
dfEEAnorm <- normalizar_eea(dfes, dfIdae_norm$Marca, dfIdae_norm$Modelo)
dfEELimp <- limpieza_eea (dfEEAnorm)
eeaEstudioAtipicos(dfEEAnorm)
dfEEALimp2 <- eea_correccion_atipicos(dfEELimp)

