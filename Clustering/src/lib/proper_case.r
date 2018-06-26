
proper_case_value <- function (strValue) {
  
  if ((strValue =="") || (is.na(strValue))){
    return (strValue)
  }
  
  tmp <- trimws(str_ucfirst(str_lowercase((strValue))), which="both")
  
  if (is.na(tmp)) {
    return (strValue)
  }
    
  return (tmp)
}

proper_case <- function (v1){
  
    v2 <- as.character(v1)
    for (r  in 1:length(v2)) {
        
        v2[r] <-  proper_case_value (v2[r])
    }
    return (v2)
}
