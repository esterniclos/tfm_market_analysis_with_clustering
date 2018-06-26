# am 14 2018

clusterizar_am14 <- function (dfO, dfSegmentos){

  dfSegmentos <- read.xlsx("data/AM14.xlsx")
  colnames(dfSegmentos) <- c("Cluster",
                             "Largo min",
                             "Largo max",
                             "Cilindrada min",
                             "Cilindrada max",
                             "Altura min",
                             "Altura max",
                             "Altura.libre.min",
                             "Batalla min",
                             "Combustible")
  
  clusters <- dfSegmentos[-1,1] # Lista de clusters.
  
  vClusters <- rep ("Sin asignar", rep(nrow(dfO)))
  
  for (c in clusters){
    condiciones <- dfSegmentos [c + 1,]
    cluster <- paste0("Cluster ",c)
    
    v <- rep(TRUE, nrow(dfO)) # Todos los vehículos se suponen que valen
    which(v)
    v <- v & filtrar (dfO$Largo, condiciones$`Largo min`,  condiciones$`Largo max`)
    v <- v & filtrar (dfO$Cilindrada, condiciones$`Cilindrada min`,  condiciones$`Cilindrada max`)
    which(v)
    v <- v & filtrar (dfO$Altura, condiciones$`Altura min`,condiciones$`Altura max`)
    which(v)
    v <- v & filtrar (dfO$Altura.libre.delantera, condiciones$Altura.libre.min, NA)
    which(v)
    v <- v & filtrar (dfO$Batalla, condiciones$`Batalla min`, NA)
    which(v)
    v <- v & filtrar_combustible (dfO$Combustible, condiciones$Combustible)
    which(v)
    
    vClusters [v] <- cluster
  }
  
  write.xlsx (count(vClusters), "informes/Am14_vehPorCluster.xlsx", asTable=TRUE)
  
  dflic <- data.frame (vClusters, dfO$Marca)
  licitadores_por_cluster <- count(unique(dflic[,1:2]) [,1])
  colnames(licitadores_por_cluster) <- c("Cluster", "Numero de licitadores")
  write.xlsx (licitadores_por_cluster, "informes/Am14_licitadoresPorCluster.xlsx", asTable=TRUE)

  
  
  
}



filtrar <- function (columnaDfO, min, max) {
  
  v <- rep(TRUE, length(columnaDfO)) # Todos los vehículos se suponen que valen
  columnaDfO <- as.numeric(columnaDfO)
  if (!is.na(min)){
    v <- v & (columnaDfO > min)
  }
  
  if (!is.na(max)){
    v <- v & (columnaDfO < max)
  }
  return (v)
  
}
  


filtrar_combustible <- function (colCombustible, cod_combustible) {
  v <- rep(FALSE, length(colCombustible)) # Asumo todos falsos se van añadiendo.

  if (is.na(cod_combustible))
    return (v)
  
  if (cod_combustible == "G/D"){
    v <- v |  grepl("^Diésel$", colCombustible)
    v <- v |  grepl("^Gasolina$", colCombustible)
    v <- v |  grepl("^GLP$", colCombustible)
  }
  else if (cod_combustible == "H/E"){
    v <- v |  grepl("Gasolina-Eléctrico", colCombustible)
    v <- v |  grepl("Diésel-Eléctrico", colCombustible)
    v <- v |  grepl("Eléctrico", colCombustible)
  }
  else if (cod_combustible == "G"){
    v <- v |  grepl("^Gasolina$", colCombustible)
    v <- v |  grepl("^GLP$", colCombustible)
  }
  else if (cod_combustible == "D"){
    v <- v |  grepl("^Diésel$", colCombustible)
  }
  else if (cod_combustible == "D/H"){
    v <- v |  grepl("^Diésel$", colCombustible)
    v <- v |  grepl("Gasolina-Eléctrico", colCombustible)
    v <- v |  grepl("Diésel-Eléctrico", colCombustible)
  }
  else if (cod_combustible == "G/H/E"){
    v <- v |  grepl("^Gasolina$", colCombustible)
    v <- v |  grepl("^GLP$", colCombustible)
    v <- v |  grepl("Gasolina-Eléctrico", colCombustible)
    v <- v |  grepl("Diésel-Eléctrico", colCombustible)
  }
  else if (cod_combustible == "D/E"){
    v <- v |  grepl("^Diésel$", colCombustible)
    v <- v |  grepl("Eléctrico", colCombustible)
  }
  else if (cod_combustible == "G/HG/E"){
    v <- v |  grepl("^Gasolina$", colCombustible)
    v <- v |  grepl("^GLP$", colCombustible)
    v <- v |  grepl("Gasolina-Eléctrico", colCombustible)
    v <- v |  grepl("Eléctrico", colCombustible)
  }
  else if (cod_combustible == "D/HD/E"){
    v <- v |  grepl("^Diésel$", colCombustible)
    v <- v |  grepl("Diésel-Eléctrico", colCombustible)
    v <- v |  grepl("Eléctrico", colCombustible)
  }
  else if (cod_combustible == "G/HG"){
    v <- v |  grepl("^Gasolina$", colCombustible)
    v <- v |  grepl("^GLP$", colCombustible)
    v <- v |  grepl("Gasolina-Eléctrico", colCombustible)
  }
  else if (cod_combustible == "G/H"){
    v <- v |  grepl("^Gasolina$", colCombustible)
    v <- v |  grepl("^GLP$", colCombustible)
    v <- v |  grepl("Gasolina-Eléctrico", colCombustible)
    v <- v |  grepl("Diésel-Eléctrico", colCombustible)
  }
  else {
    print (paste("Codigo de combustible sin alternativa: ", cod_combustible))
  }
    return (v)
}
    