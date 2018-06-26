# Funciones con na

min.na <- function (x) {
  # All na:
  if (all(is.na(x)))
    return (NA)
  
  return (min (x[!is.na(x)]) )
}


max.na <- function (x) {
  # All na:
  if (all(is.na(x)))
    return (NA)
  return (max (x[!is.na(x)]) )
}