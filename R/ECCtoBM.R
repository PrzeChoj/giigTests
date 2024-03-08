E <- function(pair, p) {
  J <- matrix(0, p, p)
  J[pair[[1]], pair[[2]]] <- 1
  J[pair[[2]], pair[[1]]] <- 1
  return(J)
}


ECCtoBM <- function(ECC, p) {
  baseMatrices <- array(0, c(p, p, length(ECC) + 1))
  baseMatrices[, , 1] <- diag(p)

  for (i in seq_along(ECC)) {
    tmp <- matrix(0, p, p)
    for (j in seq_along(ECC[[i]])) {
      if (length(ECC[[i]][[j]]) > 1) {
        tmp <- tmp + E(ECC[[i]][[j]], p)
      } else {
        tmp <- E(ECC[[i]], p)
        break
      }
    }
    baseMatrices[, , i + 1] <- tmp
  }

  return(baseMatrices)
}
