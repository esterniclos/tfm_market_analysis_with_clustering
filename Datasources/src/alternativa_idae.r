# alternativa_idae.r




#####################################################
## seleccionar_modelo_corregido
#####################################################
seleccionar_modelo_corregido <- function (marca, modelo, marcas_validadas, modelos_validados){
  
  log_error(INFO, paste( " - Correccion de ", marca, " ", modelo))
  # Distancia de Levenshtein
  distancia_a_los_validados <- stringdist(modelo, modelos_validados, "lv")
  # Asegurar que sea de la misma marca : Calculo una puntuación para excluir los de otras marcas
  puntuacion_para_otra_marca <- max(distancia_a_los_validados) + 1 
  distancia_a_los_validados[marcas_validadas!=marca] <- puntuacion_para_otra_marca
  
  
  distancia_minima <- min(distancia_a_los_validados)  
  posibles_modelos <- modelos_validados[which (distancia_a_los_validados == distancia_minima)]
  # Si el número de cambios es igual o superior al número de letras posible
  # no tiene alternativa.
  corregido <- (distancia_minima < nchar(modelo) - 1) && (distancia_minima< 10) # Umbral de corrección
  alternativa <- posibles_modelos[1]
  log_eea_correccion_modelo(marca,modelo,alternativa,distancia_minima,corregido)
  if (! corregido){
    
    log_error(ERROR, paste("---->  Sin corrección posible", marca , modelo, posibles_modelos[1]))
    return (modelo)
  }
  return (alternativa) # Devuelve la alternativa al modelo
}