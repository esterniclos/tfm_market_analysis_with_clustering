# limpieza normalizacion idae
fnIdaeOrig <- "01_idae/modelos_idae"
fnIdaeNorm <- "02_Normalizados/idae_normalizado"

limpieza_idae <- function  (dfIdae)
{
  dfIdae_norm <- dfIdae[dfIdae$Catalogado.TT == 0,]
  
  dfIdae_norm$Marca <- proper_case(dfIdae_norm$Marca)
  dfIdae_norm$Modelo <- proper_case(dfIdae_norm$Modelo)
  
  exportacion (fnIdaeNorm, dfIdae_norm)

  return (dfIdae_norm)
}

# Limpieza y normalizacion
dfIdae <- read.csv ("01_idae/modelos_idae.csv", sep=",")
dfIdae_norm <- limpieza_idae(dfIdae)

marcas_validadas <- dfIdae_norm$Marca
modelos_validados<- dfIdae_norm$Modelo