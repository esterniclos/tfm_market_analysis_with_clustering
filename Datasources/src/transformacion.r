# transformacion.r

fnTransformado <- "04-transformacion/Vehiculos_Comercializados_ultimo_quinquenio"
fnClustering <- "../Clustering/data/Vehiculos_Comercializados_ultimo_quinquenio"


transformacion <- function (dfEEALimp2, dfvpicLimp2){


  log_transformacion_cabecera()
  
  dfr <- data.frame()
  
  for (i in  1:nrow(dfEEALimp2)){
      print (paste0("Buscando características físicas de motor dfvpicLimp2[",i,"]"))
      motorizacion <- dfEEALimp2[i,]
      dfmotorizacion <-  componer_vehiculo(motorizacion, dfvpicLimp2)
      if (! is.null( dfmotorizacion)) dfr <- rbind (dfr, dfmotorizacion)
      print (paste("Tenemos ya ", nrow(dfr), "vehículos"))
  }
  dfr.bkp <- dfr # debug
  
  
  dfr <- dfr.bkp
  
  
  dfr <- dfr[!is.na(dfr$Ancho.eje.tracción),] # Quitar el vehículo sin ancho de eje. Es un mal cruce de datos.
  nrow(dfr)
  dfr <- dfr[!duplicated(dfr),] # Reducir duplicados
  nrow(dfr)
  
  # eliminar vehículos con una tasa de valores faltantes mayor del 25% 
  dfr <- dfr[rowSums(is.na(dfr)) < 6,]
  
  nrow(dfr)
  
  dfr <- anonimizar_modelo(dfr)
  
  exportacion(fnTransformado, dfr)
  exportacion(fnClustering,dfr)
  
  log_transformacion_valores_unicos()
}



componer_vehiculo <- function (motorizacion,dfvpicLimp2){
  
  # Comparar marca:
  filas_marca <- (dfvpicLimp2$Marca == motorizacion$Marca)
  modelos_marca_Vpic <- dfvpicLimp2$Alternativa_IDAE[filas_marca]
  
  # Comparar modelo:
  m1_Vpic <- modelos_a_distancia_lv_minima (motorizacion$Marca, motorizacion$Alternativa_Idae,
                                            dfvpicLimp2$Marca[filas_marca], modelos_marca_Vpic)
  if (is.null(m1_Vpic)){
    log_error (ERROR, paste("Motorizacion sin vehículo", motorizacion$Marca, motorizacion$Modelo))
    log_transformacion_modelo(motorizacion$Marca, motorizacion$Modelo,TRUE,FALSE)
    return (NULL)
  }
  
  filas_carrocerias <- (dfvpicLimp2$Alternativa_IDAE %in% m1_Vpic)
  df2 <- dfvpicLimp2[filas_carrocerias, ]
  
  df3 <- df2 # Inicializar df3
  # Si tenemos datos: comparar batalla
  if (!is.na(motorizacion$Batalla)){
    vdmin_batalla <-abs (df2$Batalla - motorizacion$Batalla)
    dmin_batalla <- min (vdmin_batalla)
    filas_batalla <- vdmin_batalla == dmin_batalla
    df3 <- df2[filas_batalla,]
  }
  
  # Comparar ancho de via, si tenemos el dato
  df4 <- df3 # Inicializar df4
  df4$Traccion <- ""
  if (!is.na(motorizacion$Ancho.eje.tracción )){
    
    vdmin_ancho_via_delantero <-abs(df4$Ancho.vía.delantero 
                                    - motorizacion$Ancho.eje.tracción)
    dmin_ancho_via_delantero <- min.na (vdmin_ancho_via_delantero)
    
    vdmin_ancho_via_trasero <-abs(df4$Ancho.vía.trasero - motorizacion$Ancho.eje.tracción)
    dmin_ancho_via_trasero <- min.na (vdmin_ancho_via_trasero)
    
    if (!is.na(dmin_ancho_via_delantero) & !is.na(dmin_ancho_via_trasero)){
      if (dmin_ancho_via_trasero < dmin_ancho_via_delantero) {
        df4$Traccion <- "Tracción trasera"
        filas_ancho_via <- ((vdmin_ancho_via_trasero  == dmin_ancho_via_trasero) |
                        is.na(vdmin_ancho_via_trasero))
        }
      else
      {
        df4$Traccion <- "Tracción delantera"
        filas_ancho_via <- ((vdmin_ancho_via_delantero  == dmin_ancho_via_delantero) |
        is.na(vdmin_ancho_via_delantero))
      }
    }
    df4 <- df4 [filas_ancho_via,]
  }
  
  
  ####
  # Con los modelos seleccionados, componemos sobre df4, los modelos que tienen
  # posible esta motorizacion:
  ####
  df5 <- cbind (df4, motorizacion)
  log_transformacion_modelo(motorizacion$Marca, motorizacion$Modelo,TRUE,TRUE)
    
  return (df5)
  
}


anonimizar_modelo <- function (dfr){
  
  dfr <- dfr.bkp
  
  columnas_a_quitar <- c("Id",
                         "Modelo",
                         "Año",
                         "Tipo", 
                         "Version", 
                         "Homologacion.Tipo",
                         "Variante",
                         "Grupo.empresarial",
                         "IdFabricante",
                         "Categoria",
                         "Registros.en.estado.miembro"
                         )

  columnas_a_quitar <- c("Id",
                         "Modelo",
                         "Año",
                         "Variante",
                         "Homologacion.Tipo",
                         "Grupo.empresarial",
                         "IdFabricante",
                         "Categoria",
                         "Registros.en.estado.miembro"
  )
  
  df2 <- dfr [ ,! (colnames(dfr) %in% columnas_a_quitar)]
  df2 <- df2[!(duplicated(df2)), ]
  nrow(df2)
  return (df2)
  }


# transformacion(dfEEALimp2, dfvpicLimp2)