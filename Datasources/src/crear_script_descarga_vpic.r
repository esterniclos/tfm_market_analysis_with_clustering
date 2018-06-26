library (openxlsx)
# Crear script de descarga vpic


marcas_vpic <- read.xlsx("Traduccion_Datasources.xlsx", sheet = 2)
marcas_vpic <- marcas_vpic [!is.na(marcas_vpic$VPIC),1]

script_descarga_vpic <- "VPIC/descarga_datos_vpic.sh"
output.file <- file(script_descarga_vpic, "wb")

write ("#!/bin/bash", output.file, append = FALSE)

for (m in marcas_vpic ){
  for (y in 2013:2017){ 
    url <- as.vector (paste0("https://vpic.nhtsa.dot.gov/api/vehicles/GetCanadianVehicleSpecifications/?year=",y,"&make=",m,"&Model=&units=&format=csv"))
    output <- gsub("&", "", gsub (" ", "_", paste0(m,"-",y,".csv")))
    
    test <- paste0("[[ -e ", output," ]] ")
    
    wget <- paste0("wget '", url, "' -O ", output)
    
    sleep <- paste0 ("sleep ", sample(10:34, 1),"s")
    
    write (paste(test, " || (",  wget, " && ", sleep, ")" ), output.file, append = TRUE)
    
    
    
    
    
    
  }
}

close(output.file)
