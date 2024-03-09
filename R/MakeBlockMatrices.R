#' Change the colors of Vertices and colors of edges to base matrices
#'
#' @noRd
#'
#' @examples
#' p <- 5
#'
#' allVertexPartitions <- partitions::listParts(p)
#' allEdges <- construct_edges(p)
#' allEdgePartitions <- partitions::listParts(p * (p - 1) / 2) # Take some seconds
#'
#' vPartition <- allVertexPartitions[[9]]
#' ePartition <- allEdgePartitions[[1693]]
#'
#' my_BM <- partitions_to_base_matrices(vPartition, ePartition)
#' is_Ishi_space(n, cumsum_n, matrix_list) # TRUE
partitions_to_base_matrices <- function(vPartition, ePartition) {
  n <- unname(sapply(vPartition, length))
  cumsum_n <- cumsum(n)

  allEdges <- construct_edges(cumsum_n[length(cumsum_n)])

  # structure of matrix_list
  matrix_list <- list()
  r <- length(n)
  for (l in 1:r) {
    matrix_list[[l]] <- list()
    for (k in 1:l) {
      matrix_list[[l]][[k]] <- list()
    }
  }

  # vertex colours
  for (l in 1:length(n)) {
    matrix_list[[l]][[l]][[1]] <- diag(n[l])
  }

  # edge colours
  for (e in ePartition) {
    edges_e <- matrix(numeric(2 * length(e)), ncol = 2)
    vertex_colour_e <- matrix(numeric(2 * length(e)), ncol = 2)
    for (i in 1:length(e)) {
      edges_e[i, ] <- allEdges[[e[i]]]
      vertex_colour_e[i, ] <- c(inWhichPartIsIt(cumsum_n, allEdges[[e[i]]][1]), inWhichPartIsIt(cumsum_n, allEdges[[e[i]]][2]))
    }
    if (length(unique(c(vertex_colour_e))) > 2) {
      rlang::abort("ePartition and vPartition not compatable")
    }
    proper_vertex_colour <- sort(vertex_colour_e[1, ])
    for (i in 1:length(e)) { # could be from 2, but that woud require additional if for length(e) == 1
      if (!all(proper_vertex_colour == sort(vertex_colour_e[i, ]))) {
        rlang::abort("ePartition and vPartition not compatable; not edge regular")
      }
    }

    # ePartition and vPartition are compatable. Can add to matrix:
    proper_vertex_colour <- unique(proper_vertex_colour)
    if (length(proper_vertex_colour) == 1) {
      previous_vertex_number <- ifelse(proper_vertex_colour == 1, 0, cumsum_n[proper_vertex_colour - 1])

      new_base_matrix <- matrix(numeric(n[proper_vertex_colour] * n[proper_vertex_colour]), nrow = n[proper_vertex_colour])
      for (i in 1:length(e)) {
        new_base_matrix[edges_e[i, 1] - previous_vertex_number, edges_e[i, 2] - previous_vertex_number] <- 1
        new_base_matrix[edges_e[i, 2] - previous_vertex_number, edges_e[i, 1] - previous_vertex_number] <- 1
      }

      new_base_matrix_index <- length(matrix_list[[proper_vertex_colour]][[proper_vertex_colour]]) + 1
      matrix_list[[proper_vertex_colour]][[proper_vertex_colour]][[new_base_matrix_index]] <- new_base_matrix
    }
    if (length(proper_vertex_colour) == 2) {
      l <- proper_vertex_colour[1]
      k <- proper_vertex_colour[2]
      if (k > l) {
        l <- proper_vertex_colour[2]
        k <- proper_vertex_colour[1]
      }

      row_previous_vertex_number <- ifelse(l == 1, 0, cumsum_n[l - 1])
      col_previous_vertex_number <- ifelse(k == 1, 0, cumsum_n[k - 1])

      new_base_matrix <- matrix(numeric(n[k] * n[l]), nrow = n[l])
      for (i in 1:length(e)) {
        if (vertex_colour_e[i, 1] == l) {
          v <- edges_e[i, 1]
          w <- edges_e[i, 2]
        } else {
          v <- edges_e[i, 2]
          w <- edges_e[i, 1]
        }

        new_base_matrix[v - row_previous_vertex_number, w - col_previous_vertex_number] <- 1
      }

      new_base_matrix_index <- length(matrix_list[[l]][[k]]) + 1
      matrix_list[[l]][[k]][[new_base_matrix_index]] <- new_base_matrix
    }
  }

  matrix_list
}


inWhichPartIsIt <- function(cumsum_n, v) {
  # TODO: This is highly inefficient
  for (i in 1:length(cumsum_n)) {
    if (v <= cumsum_n[i]) {
      return(i)
    }
  }
}





# Old code:

#' Get the base matrices for a given coloring
#'
#' @noRd
#'
#' @examples
#' p <- 4
#' edges <- construct_edges(p)
#' allPartitions <- partitions::listParts(p * (p - 1) / 2) # All possible colorings of edges
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
        if (length(ECC[[i]][[j]]) > 2) {
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
