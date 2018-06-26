# clara.r

dfParaEstudio <- function (dfO, variablesEstudio){
  df <-  na.omit(dfO[,colnames(dfO)%in% variablesEstudio])
  return (df)
}



resultados_clara <-  function (k, df) {
  
  # Escalar los datos, para poder usar clustering con Clara
  df.scaled <- scale (df)
  clara.res <- clara(df.scaled, k, metric = "euclidean", correct.d=TRUE, pamLike = TRUE)

  #
  # Los objetos medoids que representan a los cluster con esta opción son:
  # 
  return (clara.res)  
}

siluetas <- function (clara.res, df){
  df.scaled <- scale (df)
  sil <- silhouette(clara.res$cluster, dist(df.scaled))
  return (sil)
}

valor_medio_siluetas <-  function (sil){
  media_CI <- round(mean(sil[,3]),2)
}


dibujar_siluetas <- function (sil, conjunto, k){
  # Valor medio para el coeficiente de siluetas
  media_CI <- valor_medio_siluetas(sil)
  titulo <- paste("Coeficiente de siluetas para conjunto de estudio", conjunto)
  subtitulo <- paste("N Clusters= ", k, "\n",
                    "Media (CI) =", media_CI, sep="" )
  Encoding(subtitulo) <- "UTF-8"
  # Diagrama de siluetas
  fviz_silhouette(sil) + 
    labs(title = titulo,
         subtitle = subtitulo) +
    ylab ("Coeficiente de siluetas")
  
  
}


graficas_clusters <- function(df, clara.res){
  
  combinaciones_posibles <- combn(colnames(df), 2)
  
  cluster <- paste ("Cluster", clara.res$clustering)
  
  for (col in 1:ncol(combinaciones_posibles)) {
    
    variable1 = combinaciones_posibles[1,col]
    variable2 = combinaciones_posibles[2,col]
    data1 = df[, which(colnames(df)==variable1)]
    data2 = df[, which(colnames(df)==variable2)]
    
    # Visualización de las variables:
    p1 <- ggplot (df, aes( data1, 
                     data2, 
                     color = cluster)) +
      geom_point ()  +
      labs (x=variable1, y=variable2, fill = "cluster")
    print (p1)
  }
  
}


dibujar_formas <- function (df, clara.res){
  
  # Diagrama de formas:
  fviz_cluster(object = clara.res,
               data = df,
               ellipse.type = "norm",
               geom = "point",
               palette = "jco",
               pointsize = 1,
               ggtheme = theme_classic())
}


minimos_y_maximos <- function (df, clara.res){
 
  
  variablesEstudio <- colnames(df)
  clusters <- unique( clara.res$clustering)
  

   dfr <- data.frame()   
  
   for (cluster in clusters){
    df2 <- df[clara.res$clustering == cluster, ] # Solo los de mi cluster.
    
    dfmin <- apply(df2, 2, min)
    dfmax <- apply(df2, 2, max)
    
    v <- vector()
    for (i in 1:length(variablesEstudio)){
      v <- c(v, dfmin[i], dfmax[i])
    }
    
    dfr <- rbind (dfr, v)
   }
  
  dfr <- cbind (clusters, dfr) # Agregar información del cluster:
  
  # Renombrar columnas:
  v <- vector()
  for (i in 1:length(variablesEstudio)){
    v <- c(v, paste(variablesEstudio[i],"min"), paste (variablesEstudio[i],"max"))
  }
  colnames(dfr) <- c("Cluster", v)
  
  # Ordenar
  dfr <- dfr[order(dfr$`Largo min`),]
  
  return (dfr)
}

contar_licitadores <- function (dfO, variablesEstudio,  clara.res){
  v1 <- c("Marca", variablesEstudio)
  
  clPorMarcas  <- unique(cbind(paste("cluster",clara.res$clustering), dfParaEstudio(dfO, v1)$Marca))
  dfr <- count(clPorMarcas[,1])
  colnames(dfr)  <- c("Cluster", "N de licitadores")
 
  return (dfr)
}



grafica_numero_optimo_clusters <- function (df){
  
  fviz_nbclust(df, clara, method = "silhouette", k.max= 12)+
    theme_classic()
  
}
