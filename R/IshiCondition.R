vector_in_span <- function(v, A) {
  augmented_matrix <- cbind(A, v)
  return(rankMatrix(augmented_matrix)[1] == rankMatrix(A)[1])
}


isAinSpanBMs <- function(A, BMs) {
  B <- matrix(c(BMs), ncol = dim(BMs)[3])
  return(vector_in_span(c(A), B))
}

IshiCondition <- function(BMs) {
  ncol <- dim(BMs)[3]
  tmp <- TRUE
  for (i in 2:ncol) {
    for (j in i:ncol) {
      tmp <- tmp & isAinSpanBMs(BMs[, , i] %*% BMs[, , j], BMs)
    }
  }
  return(tmp)
}
