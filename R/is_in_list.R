is_in_list <- function(el, list){
  return(any(sapply(list, function(x) identical(x, el))))
}
