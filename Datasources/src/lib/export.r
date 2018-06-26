# Exportacion

exportacion <- function (filename, df){
  
  output_csv <- paste0(filename,".csv")
  write.csv(df, output_csv)
  
  
  output_xlsx <- paste0(filename,".xlsx")
  
  tryCatch({
    write.xlsx(df, output_xlsx, asTable=TRUE)
  }
  , error = function(err) {
    write.xlsx(df, output_xlsx, asTable=FALSE)
  }
  )
  print (paste("Exportación ", filename))
}

# Para avanzar más rápido, algunos archivos se almacenan en intermedios como si fuera una cache.
recuperar_de_cache <- function (filename){
  read.xlsx( paste0 (filename, ".xlsx"))
}