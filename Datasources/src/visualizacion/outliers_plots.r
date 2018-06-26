# plots_eea.r

f <- list(
  family = "Courier New, monospace",
  size = 8,
  color = "#7f7f7f"
)
xAxis <- list(
  title = "",
  titlefont = f
)
yAxis <- list(
  title = "",
  titlefont = f
)


df_para_boxplot <- function (dforiginal, c_seleccionadas){
  
  mr <- matrix(ncol = 2, nrow=0) # Matriz resultado para visualización.
  
  for (c1 in c_seleccionadas) {
    # Matriz para esta columna seleccionada
    m1 <- matrix ( data = c(rep (c1,nrow(dforiginal)) , 
                            dforiginal[,colnames(dforiginal) == c1]), ncol=2)
    mr <- rbind (mr, m1) # Añadir a resultados.
  }
  
  df1 <- data.frame (mr)
  colnames(df1 ) <- c("Variable", "Y")
  df1 <- df1[!is.na(df1$Y),] # Elimino valores NA
  v1 <- as.numeric (as.character(df1$Y)) # Convertir en números.
  df1$Y <- v1
  
  
  
  return (df1)
}


eea_outliers_boxplot <- function (dfEELimp )
{
  c_seleccionadas <- c ("Cilindrada" , "Batalla" , "Masa")
  df1 <- df_para_boxplot (dfEELimp,c_seleccionadas)
  
  
  titulo = "Estudio atípicos EEA"
  p <- plot_ly (df1, y=~Y, x=~Variable, color = ~Variable, type = "box") %>%
    layout (title = titulo, xaxis=xAxis, yaxis= yAxis)
  p
  
}


vpic_outliers_boxplot <- function (dfvpicLimp)
{
 
  df <- dfvpicLimp2
  
  c_seleccionadas <- c (
  "Largo",                              
  "Batalla"                
  )
  titulo = "Estudio atípicos vPIC: Largo y batalla"
  
  df1 <- df_para_boxplot (df,c_seleccionadas)
  p1 <- plot_ly (df1, y=~Y, x=~Variable, color = ~Variable, type = "box") %>%
    layout (title = titulo, xaxis=xAxis, yaxis= yAxis)
  p1
  
  c_seleccionadas <- c (
    "Ancho",
    "Ancho.vía.delantero",
    "Ancho.vía.trasero",
    "Ancho.de.techo"
  )
  titulo = "Estudio atípicos vPIC: Anchuras"
  
  df1 <- df_para_boxplot (df,c_seleccionadas)
  p1 <- plot_ly (df1, y=~Y, x=~Variable, color = ~Variable, type = "box") %>%
    layout (title = titulo, xaxis=xAxis, yaxis= yAxis)
  p1
  
  c_seleccionadas <- c (
    "Altura",                                            
    
     "Altura.vertical.ventanillas",
     "Altura.de.panel.inferior.a.base.ventanillas",
     "Altura.libre.delantera",
     "Altura.libre.trasera")
  
  titulo = "Estudio atípicos vPIC: Alturas"
  
  # Ajustando para la impresión:
  df1 <- df_para_boxplot (df,c_seleccionadas)
  p1 <- plot_ly (df1, x=~Y, y=~Variable, color = ~Variable, type = "box") %>%
    layout (title = titulo, xaxis=xAxis, yaxis= yAxis)
  p1
  
  
  c_seleccionadas <- c (
  "Distancia.Parachoques.y.base.parabrisas",  
  "Distancia.parachoques.trasero.y.centro.luz.trasera"  )
  titulo = "Estudio atípicos vPIC: Distancias desde parachoques"
  df1 <- df_para_boxplot (df,c_seleccionadas)
  p1 <- plot_ly (df1, y=~Y, x=~Variable, color = ~Variable, type = "box") %>%
    layout (title = titulo, xaxis=xAxis, yaxis= yAxis)
  p1
  
  
  
  
}