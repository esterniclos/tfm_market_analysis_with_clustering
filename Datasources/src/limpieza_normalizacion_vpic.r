# limpieza_normalizacion_vpic.r


fnVpicSRC <- "01_VPIC/TECH_SPEC_2013-2017.csv"
fnVpicNorm <- "02_Normalizados/Vpic-Normalizado"
fnVpicLimp <- "03_Limpieza/Vpic_limpieza_y_seleccion"
fnVpicLimp2 <- "03_Limpieza/Vpic_limpieza_y_seleccion_sin_atipicos"
fnVpicAtipicos <- "debug/vpic_atipicos"

fnVpicCorreccionModelos <-  "debug/VPIC_correccion_modelos.csv"
fnVpicModelosAtipicos <-  "debug/VPIC_modelos_descartados"

#########################################
# vpic_cargar_dforiginal
#########################################

vpic_cargar_dforiginal <- function (filename){
  
  df <- read.table(filename, sep=",", header = TRUE, fill =TRUE)
  # Algunos nombres de las columnas, presentan distintas grafías. Se ponen todas en mayúsculas
  colnames(df)<- gsub ("__","_",gsub('\\.', "_", str_allcaps(colnames(df))))
  return (df)
}

#########################################
# vpic_normalizar
#########################################

vpic_normalizar <- function (dfVpicOrig, marcas_validadas, modelos_validados){
  dfVpicnorm <- dfVpicOrig
  # eliminar títulos extras
  dfVpicnorm <- dfVpicnorm[!(dfVpicnorm$MAKE == "MAKE"),]
  
  print ("Vpic: Renombrado de columnas")
  colnames.dfVpicnorm <- c(
    "Marca", "Modelo", "Año", "Largo", "Ancho", "Altura", "Batalla", "CW",
    "Distancia Parachoques y base parabrisas",
    "Distancia parachoques trasero y centro luz trasera",
    "Altura vertical ventanillas",
    "Altura de panel inferior a base ventanillas",
    "Ancho de techo",
    "Altura libre delantera",
    "Altura libre trasera",
    "Ancho vía delantero",
    "Ancho vía trasero",
    "Distribución del peso"
  )
  for (c in 1:length(colnames(dfVpicnorm))){# Traducción de las columnas:
    print (paste( colnames(dfVpicnorm)[c], "---> ", colnames.dfVpicnorm[c]))
  }
  colnames(dfVpicnorm) <- colnames.dfVpicnorm
  
  dfVpicnorm$Año <- paste ("20", dfVpicnorm$Año, sep ="") # Completar dígitos del año.
  
  # Normalizar grafías y columnas numéricas.
  col_numericas <-   colnames.dfVpicnorm <- c(
    "Largo", "Ancho", "Altura", "Batalla", "CW",
    "Distancia Parachoques y base parabrisas",
    "Distancia parachoques trasero y centro luz trasera",
    "Altura vertical ventanillas",
    "Altura de panel inferior a base ventanillas",
    "Ancho de techo",
    "Altura libre delantera",
    "Altura libre trasera",
    "Ancho vía delantero",
    "Ancho vía trasero",
    "Distribución del peso"
  )
  
  for (c in 1:ncol(dfVpicnorm)){
    # Las que son numéricas se normalizan. 
    if (colnames(dfVpicnorm)[c] %in% col_numericas) {
      print (paste0("Conversión a número columna ", c ," ",colnames(dfVpicnorm)[c]))
      dfVpicnorm[,c] <- as.numeric (as.character(dfVpicnorm[,c])) 
      dfVpicnorm[,c] <- dfVpicnorm[,c] * 10 # Medido en CM
    }
    else {    # Las que no lo son, se unifica la grafía.
      print (paste0("Corrección grafía columna ",c, " ",colnames(dfVpicnorm)[c]))
      dfVpicnorm[ ,c]<-proper_case (dfVpicnorm[ ,c])
    }
  }
  
  # añadir la alternativa IDAE y si están o no incluidos en el modelo IDAE:
  df1 <- alternativa_idae_modelos (dfVpicnorm$Marca, dfVpicnorm$Modelo,
                                   marcas_validadas, modelos_validados)
  dfVpicnorm <-   cbind   (dfVpicnorm,df1)
  
  exportacion(fnVpicNorm, dfVpicnorm)
}


###########################################
#vpic_correccion_inicial_modelos
###########################################


vpic_correccion_inicial_modelos<- function  (dfVpicnorm){
  
  # Correccion de marcas para converger con IDAE
  marca_a_corregir <- grepl("Alfa Romeo", dfVpicnorm$Marca)
  dfVpicnorm$Marca[marca_a_corregir] <-  "Alfa-Romeo"
  marca_a_corregir <- grepl("Land Rover", dfVpicnorm$Marca)
  dfVpicnorm$Marca[marca_a_corregir] <-  "Landrover"
  marca_a_corregir <- grepl("Mercedes-Benz", dfVpicnorm$Marca)
  dfVpicnorm$Marca[marca_a_corregir] <-  "Mercedes"
  
  
  # Corrección en mercedes
  modelos_a_corregir <- grepl("Mercedes", dfVpicnorm$Marca) &
                        grepl("^. Class", dfVpicnorm$Modelo, ignore.case = TRUE)
  dfVpicnorm$Modelo[modelos_a_corregir] <-  
    paste ("Clase", str_replace (dfVpicnorm$Modelo[modelos_a_corregir],"Class ", ""))
  
  
  # Corrección en BMW
  modelos_a_corregir <- grepl("Bmw", dfVpicnorm$Marca, ignore.case = TRUE) &
    grepl("^. Series", dfVpicnorm$Modelo, ignore.case = TRUE)
  dfVpicnorm$Modelo[modelos_a_corregir] <-  
    paste ("Serie", str_replace (dfVpicnorm$Modelo[modelos_a_corregir],"Series ", ""))
  
  # Cabriolet pasa a Cabrio que es la cadena que usa IDAE
  modelos_a_corregir <-  grepl("Bmw", dfVpicnorm$Marca, ignore.case = TRUE) &
            grepl("cabriolet", dfVpicnorm$Modelo, ignore.case = TRUE)
  dfVpicnorm$Modelo[modelos_a_corregir] <-  
     str_replace (dfVpicnorm$Modelo[modelos_a_corregir],"Cabriolet", "Cabrio")
  
  # Cabriolet pasa a Cabrio que es la cadena que usa IDAE
  modelos_a_corregir <-  grepl("Audi", dfVpicnorm$Marca, ignore.case = TRUE) &
    grepl("Hatchback", dfVpicnorm$Modelo, ignore.case = TRUE)
  dfVpicnorm$Modelo[modelos_a_corregir] <-  
    str_replace (dfVpicnorm$Modelo[modelos_a_corregir],"Hatchback", "Sportback")
  
  # Corrección del ford fusion que en Europa es mondeo a partir de una longitud 4.880m
  modelos_a_corregir <-  grepl("Ford", dfVpicnorm$Marca, ignore.case = TRUE) &
    grepl("fusion", dfVpicnorm$Modelo, ignore.case = TRUE)
  df_tmp <- dfVpicnorm[modelos_a_corregir,]
  df_tmp$Modelo <- str_replace_all(df_tmp$Modelo, "Fusion", "Mondeo")
  dfVpicnorm <- rbind (dfVpicnorm, df_tmp)
  
  return (dfVpicnorm)
  
}


###########################################
# vpic_limpieza
###########################################

vpic_limpieza <- function (dfVpicnorm) {

  dfvpicLimp <- dfVpicnorm
  # Eliminar los que no tienen correspondencia IDAE
  dfvpicLimp<- dfvpicLimp[dfvpicLimp$Modelo_IDAE,]
  
  exportacion (fnVpicLimp, dfvpicLimp)
}
###########################################
# vpic_limpieza
###########################################

vpicEstudioAtipicos <- function (dfvpicLimp){
  
  dfr<- info_atipicos(dfvpicLimp$Largo, "Largo")
  for (c in 5:17) {
    dfr<- rbind (dfr, info_atipicos(dfvpicLimp[,c], 
                                    str_replace_all(colnames(dfvpicLimp)[c], '\\.', " ")))
  }
  
  exportacion ( fnVpicAtipicos, dfr)
  return (dfr)
}

#####################################################
## vpic_correccion_atipicos
#####################################################

vpic_correccion_atipicos <- function (dfvpicLimp){
  dfvpicLimp2 <- dfvpicLimp
  
  # Eliminados por batalla:
  modelos_a_descartar <- (dfvpicLimp2$Batalla > 3241)
  descartados <- dfvpicLimp2[modelos_a_descartar,]
  exportacion (fnVpicAtipicos, descartados)
  
  dfvpicLimp2 <- dfvpicLimp2 [!modelos_a_descartar, ]
  
  
  # Eliminación de registros duplicados
  dfvpicLimp2 <- dfvpicLimp2[! duplicated(dfvpicLimp2),  ]
  
  exportacion ( fnVpicLimp2, dfvpicLimp2)
  return (dfvpicLimp2)

}
  

#
# dfVpicOrig <- vpic_cargar_dforiginal (fnVpicSRC)
# dfVpicnorm <-vpic_normalizar(dfVpicOrig)
# dfvpicLimp <-  vpic_limpieza (dfVpicnorm)
# vpicEstudioAtipicos(dfvpicLimp)
# dfvpicLimp2 <- vpic_correccion_atipicos (dfvpicLimp)


# dfVpicnorm <- recuperar_de_cache(fnVpicNorm)
# dfvpicLimp <- recuperar_de_cache(fnVpicLimp)
dfvpicLimp2 <- recuperar_de_cache(fnVpicLimp2)