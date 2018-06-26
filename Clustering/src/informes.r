#
# Clustering exhaustivo - Kmeans o pam y silhouette.
#
# 


docxNumeroOptimoClusters <- function (dfO, variablesEstudio, fichero_salida){
  
  df <- dfParaEstudio(dfO, variablesEstudio)
  
  env2 <- new.env()
  env2$variablesEstudio <- variablesEstudio
  env2$df <- df
  
  rmarkdown::render('rmd/numero_optimo_clusters.rmd',
                    encoding="UTF-8", 
                    envir = env2, 
                    output_file = fichero_salida)
  
}



docxAlternativaSegmentacion <- function (dfO, k, variablesEstudio, conjunto ){
  
  df <- dfParaEstudio(dfO, variablesEstudio)
  clara.res <- resultados_clara (k,  df)
  sil <- siluetas (clara.res, df)
  
  env2 <- new.env()
  env2$k <- k
  env2$variablesEstudio <- variablesEstudio
  env2$conjunto <- conjunto
  env2$df <- df
  env2$sil <- sil
  env2$clara.res <- clara.res
  env2$media_ci <- valor_medio_siluetas(sil)
  env2$dfLicitadoresPorCluster <- contar_licitadores (dfO, variablesEstudio, clara.res)
   
   fichero_salida <- paste0("../informes/conjunto",conjunto,"_k",k,".docx")
   rmarkdown::render('rmd/main.rmd',
                     encoding="UTF-8",
                     envir = env2,
                     output_file = fichero_salida)

  
  fichero_salida <- paste0("informes/conjunto",conjunto,"_minmax_k",k,".xlsx")
  df1 <- minimos_y_maximos(df,clara.res)
  write.xlsx(df1, fichero_salida, asTable=TRUE)
  
}


docxEstudio <- function (df, variablesEstudio, conjunto){
  
  of <- paste0("informes/conjunto",conjunto,"_numero_optimo_clusters.docx")
  # docxNumeroOptimoClusters(df, variablesEstudio,of)
  for (k in seq(4,12,2)) {
    docxAlternativaSegmentacion(dfO, k, variablesEstudio, conjunto)
  }
  k<-9
  docxAlternativaSegmentacion(dfO, k, variablesEstudio, conjunto)
  k<- 11
  docxAlternativaSegmentacion(dfO,k , variablesEstudio, conjunto)
}
