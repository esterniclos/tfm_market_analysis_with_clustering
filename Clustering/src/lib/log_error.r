# log

fnErrorLog = "debug/errors.log"
INFO <-    "Info   "
WARNING <- "Warning"
ERROR <-   "Error  "


log_error_init <- function(){
  linea <- paste0 (INFO, "[",date() , "] ", "Inicializado")
  cat (linea, file=fnErrorLog, append=FALSE, sep="\r\n")
}


log_error <- function (level, message){
  
  linea <- paste0 (level, "[",date() , "] ", message)
  cat (linea, file = fnErrorLog, append= TRUE, sep = "\r\n")
  print (linea)
  
  
}



log_vpic_correccion_modelo <- function (marca, modelo,alternativa, distancia_minima,corregido )
{
  
  linea <- paste(marca, modelo,alternativa, distancia_minima,corregido, sep=";")
  cat (linea, file=fnVpicCorreccionModelos,append=TRUE, sep="\r\n")
}
log_vpic_cabecera <- function ()
{
  linea <- paste("Marca","Modelo","Alternativa","Distancia","Corregido", sep=";")
  cat (linea, file=fnVpicCorreccionModelos, append = FALSE, sep="\r\n")
}




log_eea_cabecera <- function ()
{
  linea <- paste("Marca","Modelo","Alternativa","Distancia","Corregido", sep=";")
  cat (linea, file=fnEeaCorreccionModelos, append = FALSE, sep="\r\n")
}

log_eea_correccion_modelo <- function (marca, modelo,alternativa, distancia_minima,corregido )
{
  
  linea <- paste(marca, modelo,alternativa, distancia_minima,corregido, sep=";")
  cat (linea, file=fnEeaCorreccionModelos,append=TRUE, sep="\r\n")
  
}

################################
# Logs de la fase de transformacion
#
################################

fnTransformacionCorrelacionTmp <- "debug/transformacion_tienen_correlacion_tmp.csv"
fnTransformacionCorrelacion <- "debug/transformacion_tienen_correlacion"


log_transformacion_cabecera <- function ()
{
  linea <- paste("Marca","Modelo","tiene_definicion_eea","tiene_dimensiones_vpic", sep=";")
  cat (linea, file=fnTransformacionCorrelacionTmp, append = FALSE, sep="\r\n")
}

log_transformacion_modelo <- function (marca, modelo, esta_en_eea = TRUE, tiene_dimensiones=TRUE )
{
  linea <- paste(marca, modelo,esta_en_eea, tiene_dimensiones, sep=";")
  cat (linea, file=fnTransformacionCorrelacionTmp,append=TRUE, sep="\r\n")
}

log_transformacion_valores_unicos <- function (){
  df <- read.csv(fnTransformacionCorrelacionTmp, sep =";")
  df <- unique (df)
  exportacion(fnTransCorrelacion,df)
}



