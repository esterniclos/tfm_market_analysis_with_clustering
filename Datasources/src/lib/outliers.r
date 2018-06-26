# v.atipicos()
# Elimina outliers si son significativos. Usa test de Grubbs
# Comprueba si la distribución es normal
# x: Vector que contiene los datos
# Eliminar: Si es 0 devuelve un vector con los outliers, cualquier otro valor devuelve el vector original sin los outliers
# p.o: nivel de significación para eliminar outliers
# p.n: nivel de significación para el test de distribución normal

es_normal <- function (x, p.n = 0.01){
  # norm <- shapiro.test(x) #Para más muestras que 5000 debemos usar kolmogorov-smirnov
  norm <- ks.test(x,y='pnorm',alternative='two.sided')
  
  resultado <- norm$p.value <= p.n 
  if (!resultado) {
    cat("La distribución no es normal. No hay detección posible\n")
  }
  return(resultado)
}

info_atipicos <- function (x, titulo ="x1") {
  
  # Medidas estadísticas
  n <- as.numeric(x[!is.na(x)]) # Elimino los no numéricos
  t1 <- c("Min", "Media", "Mediana", "Max", "Desv. Típica", "Mínimo rango típico", "Máximo rango típico")
  t2 <- c(min(n), mean(n), median(n), max(n), as.numeric(sd(n)),0,0)
  t2[6] <- max(round(t2[3]-3*as.numeric(t2[5]),2),0)
  t2[7] <- round(t2[3]+3*as.numeric(t2[5]),2)
  t2[5] <- round (t2[5],2)
  t2[2] <- round(t2[2],2)
  
  dfr <- data.frame (rbind(t1,t2))
  dfr <- cbind (rep(titulo,2), dfr)
  dfr <- dfr [-1,] # Se repiten los números.
  colnames(dfr) <- c("Criterio",t1)
  print (dfr)
  print ("--")
  
  
  print (paste0 ("Son atípicos los que no están en el rango [", 
                 t2[6],
                 ",",
                 t2[7],
                 "]"))
  
  return (dfr)
}



son_atipicos <- function (x, titulo = "x1"){
  
  if(!is.vector(x) || !is.numeric(x)) {
    cat("El argumento debe ser un vector numérico\n")
    return(invisible(NULL))
  }
  
  # Medidas estadísticas
  n <- x[!is.na(x)] # Elimino los no numéricos
  t1 <- c(titulo,"Min", "Media", "Mediana", "Max", "Desv. Típica")
  t2 <- c(titulo,min(n), mean(n), median(n), max(n), sd(n))
  
  dfr <- data.frame (rbind(t1,t2))
  colnames(dfr) <- t1
  print (dfr)
  print ("--")
  
  print (paste0 ("Son atípicos los que no están en el rango [", 
                round(t2[4]-3*t2[6],2),
                ",",
                round(t2[4]+3*t2[6],2),
                "]"))
  
  
  if (!es_normal (n))
    return (NULL)
  
  x1 <- rm.outlier(x)
  
  resultados <- (! x %in% x1)
}

