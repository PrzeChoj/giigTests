#' Get the base matrices for a given coloring
#'
#' @noRd
#'
#' @examples
#' p <- 4
#' edges <- construct_edges(p)
#' allPartitions <- partitions::listParts(p*(p-1)/2) # All possible colorings of edges
#' myPartition <- allPartitions[[7]] # this is the partition of indices of the edges
#' ECC <- PartitionToEdgeColorClasses(myPartition, edges) # this is the partition of the edges
#'
#' ECCtoBM(ECC, p)
ECCtoBM <- function(ECC, p) {
  baseMatrices <- array(0, c(p, p, length(ECC) + 1))
  baseMatrices[, , 1] <- diag(p) # we have only one color of the vertices

  for (i in seq_along(ECC)) {
    tmp <- matrix(0, p, p)
    for (j in seq_along(ECC[[i]])) {
      if (length(ECC[[i]][[j]]) > 1) {
        if (length(ECC[[i]][[j]]) > 2){
          rlang::abort("There is error in the code. Get back to the original code with E() function.")
        }
        vertex_1 <- ECC[[i]][[j]][1]
        vertex_2 <- ECC[[i]][[j]][2]

        tmp[vertex_1, vertex_2] <- 1
        tmp[vertex_2, vertex_1] <- 1
      } else {
        tmp <- matrix(0, p, p)
        tmp[ECC[[i]][[1]], ECC[[i]][[2]]] <- 1
        tmp[ECC[[i]][[2]], ECC[[i]][[1]]] <- 1

        break
      }
    }
    baseMatrices[, , i + 1] <- tmp
  }

  baseMatrices
}
