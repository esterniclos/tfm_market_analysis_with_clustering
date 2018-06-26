# filtrado intermedio

filterNum <- function (x, min = NULL, max = NULL){
  
  x1 <- x[!is.na(x)]
  if (!is.null(min))
    x1 <- x1 [x1 >= min ]
  if (!is.null(max))
    x1 <- x1 [x1 <= max ]
  
  return (as.numeric(x1))
}