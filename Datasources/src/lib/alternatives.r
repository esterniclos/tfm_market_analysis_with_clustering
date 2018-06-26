# Hallar alternativa Idae


###########################################
#distancia_lv_minima
###########################################

distancia_lv_minima <- function (marca, modelo, marcas_validadas, modelos_validados)
{ 
  # Modelos solo de mi marca
  modelos_de_mi_marca <- modelos_validados[marcas_validadas==marca]
  # Distancia de Levenshtein
  distancia_a_los_validados <- stringdist(modelo, modelos_de_mi_marca, "lv")
  # Distancia mínima:
  distancia_minima <- min.na(distancia_a_los_validados)  
  
  return (distancia_minima)
}


###########################################
# modelo_a_distancia_lv_minima
###########################################

modelos_a_distancia_lv_minima <- function (marca, modelo, marcas_validadas, modelos_validados)
{ 
  if (is.null( marcas_validadas) | (length(marcas_validadas) == 0 )) return (NULL)
  if (is.null(modelos_validados)| (length(modelos_validados) == 0 )) return (NULL)
    
  
  # Modelos solo de mi marca
  modelos_de_mi_marca <- modelos_validados[marcas_validadas==marca]
  # Distancia de Levenshtein
  distancia_a_los_validados <- stringdist(modelo, modelos_de_mi_marca, "lv")
  # Distancia mínima:
  distancia_minima <- min.na(distancia_a_los_validados)
  
  
  
  # Elementos que cumplen la distancia mínima:
  m1 <- modelos_de_mi_marca [distancia_a_los_validados == distancia_minima]
  
  # Si cumplen el umbral los devuelvo, si no devuelvo null
  if (distancia_minima < nchar(modelo) - 2)
      return (m1) # Si sólo hay un elemento

  return (NULL)
}


###########################################
# modelo_a_distancia_lv_minima
###########################################

modelo_a_distancia_lv_minima <- function (marca, modelo, marcas_validadas, modelos_validados)
{ 
  # Modelos solo de mi marca
  modelos_de_mi_marca <- modelos_validados[marcas_validadas==marca]
  # Distancia de Levenshtein
  distancia_a_los_validados <- stringdist(modelo, modelos_de_mi_marca, "lv")
  # Distancia mínima:
  distancia_minima <- min.na(distancia_a_los_validados)
  
  # Elementos que cumplen la distancia mínima:
  m1 <- modelos_de_mi_marca [distancia_a_los_validados == distancia_minima]
  
  if (length(m1) > 1) 
    return (m1[1]) # Si es un vector
  return (m1) # Si sólo hay un elemento
}


test <- function (){
  r <- 107
  marca <- dfVpicnorm$Marca[r]
  modelo <- dfVpicnorm$Modelo[r]
  seleccionar_modelo_validado (marca, modelo, marcas_validadas, modelos_validados)
}

###########################################
#vpic_seleccionar_modelo_corregido
###########################################

seleccionar_modelo_validado <- function (marca, modelo, marcas_validadas, modelos_validados)
{
  log_error(INFO, paste0( " - Correccion de ", marca, " ", modelo))
  
  # Si no existe su marca, no es posible validar nada
  if (!(marca %in% marcas_validadas)) {
    log_error(ERROR, "----> Sin corrección. Sin marca.")
    return (NA) # Imposible de validar.
  }
  
  
  modelo_desglosado <- unlist(strsplit(modelo , " "))
  
  # Alternativas más usuales:
  vAlternativas <- c(modelo_desglosado[1],
                     paste(modelo_desglosado[1],modelo_desglosado[2]),
                     paste(modelo_desglosado[1],modelo_desglosado[3]),
                     paste(modelo_desglosado[1],modelo_desglosado[2],modelo_desglosado[3]),
                     paste(modelo_desglosado[1],modelo_desglosado[2],modelo_desglosado[length(modelo_desglosado)-1]),
                     modelo_desglosado[length(modelo_desglosado)])
  vAlternativas <- vAlternativas [!grepl (" NA$", vAlternativas)]
  vAlternativas <- vAlternativas [!grepl (" NA $", vAlternativas)]
  vAlternativas <- unique(vAlternativas)
  vDist <- c(rep (60, length(vAlternativas)))
  
  for (r in 1: length(vAlternativas)){
    alternativa <- vAlternativas[r]
    vDist[r] <- distancia_lv_minima (marca, alternativa,marcas_validadas, modelos_validados)
    print (paste0("Alternativa  ", r, " ", alternativa))
    if (vDist[r] == 0){
      log_vpic_correccion_modelo(marca,modelo,alternativa,0,TRUE)
      return (alternativa)
    }
  }
  
  distancia_minima = min (vDist)
  alternativa_a_estudiar <- vAlternativas[which (vDist ==distancia_minima)]
  print (paste0("Alternativa seleccionada ", r, " ", alternativa_a_estudiar))
  mejor_alternativa_validada <- modelo_a_distancia_lv_minima (marca, alternativa_a_estudiar, marcas_validadas, modelos_validados)
  
  
  
  
  if (distancia_minima < max (1, nchar(mejor_alternativa_validada) - 2)){
    log_vpic_correccion_modelo(marca,modelo,mejor_alternativa_validada,distancia_minima,TRUE)
    return (mejor_alternativa_validada)
  }
  
  # No encuentra nada:
  log_vpic_correccion_modelo(marca,modelo,mejor_alternativa_validada,distancia_minima,FALSE)
  log_error(ERROR, "----> Sin corrección posible")
  return (NA) # No sabe qué devolver
}

##########################
#
# alternativa_idae_modelos
#
# Devuelve una lista de alternativas o NA si no ha encontrado nada.
#- Alternativa IDAE
#########################

alternativa_idae_modelos <- function (marcas, modelos, 
                                      marcas_validadas, modelos_validados){
  
  vmodelos_para_corregir <- !(modelos %in% modelos_validados)
  log_eea_cabecera() # Fichero con los resultados de las correcciones
  for (r in 1:length(modelos))
  {
    if (vmodelos_para_corregir[r]){ # Si está marcado para corregir, buscar la mejor opción.
      print (paste("R=",r))
      modelo <- modelos [r]
      marca <- marcas[r]
      modelos[r] <- seleccionar_modelo_validado (marca, modelo, marcas_validadas, modelos_validados)
      }
  }
 
  return (modelos)
}


