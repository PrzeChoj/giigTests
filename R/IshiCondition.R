#' Is the given vector spanned by the columns of the matrix
#'
#' @noRd
#'
#' @examples
#' vector_in_span(
#'   c(2, 2, 4),
#'   matrix(c(
#'     1, 2,
#'     1, 2,
#'     3, 2
#'   ), nrow = 3, byrow = T)
#' ) # TRUE
vector_in_span <- function(v, A) { # TODO: This can be done faster for all 0-1 matrices. Remember the (M1) is not for 0-1 matrices, but (M0) and (M2) are.
  augmented_matrix <- cbind(A, v)
  return(rankMatrix(augmented_matrix)[1] == rankMatrix(A)[1])
}


#' Checks if the matrix A is spanned by a list of BaseMatrices
#'
#' @param A matrix
#' @param BaseMatrices a list of matrices
#'
#' @noRd
#'
#' @examples
#' BaseMatrices <- list(matrix(c(1, 0, 0, 1), nrow = n[1], byrow = TRUE), matrix(c(0, 1, 1, 0), nrow = n[1], byrow = TRUE))
#' A <- -17.3 * BaseMatrices[[1]] + 0.4 * BaseMatrices[[2]]
#' isAinSpanBaseMatrices_list(A, BaseMatrices) # TRUE
isAinSpanBaseMatrices_list <- function(A, BaseMatrices) {
  if (length(BaseMatrices) == 0) {
    return(dim(A)[1] == 0) # the empty matrix is spanned by the empty bases
  }

  space_dim <- dim(BaseMatrices[[1]])[1] * dim(BaseMatrices[[1]])[2]
  Base <- matrix(numeric(space_dim * length(BaseMatrices)), nrow = space_dim)

  for (i in 1:length(BaseMatrices)) {
    Base[, i] <- c(BaseMatrices[[i]])
  }

  return(vector_in_span(c(A), Base))
}

#' Check whether (M0) condition is satisfied
#'
#' So, check if the identity matrices can be spanned by diagonal
#' @noRd
is_M0_condition_satisfied <- function(n, cumsum_n, matrix_list) {
  r <- length(cumsum_n)
  for (k in 1:r) {
    if (!isAinSpanBaseMatrices_list(diag(n[k]), matrix_list[[k]][[k]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Check whether (M1) condition is satisfied
#' @noRd
is_M1_condition_satisfied <- function(n, cumsum_n, matrix_list) {
  r <- length(cumsum_n)
  for (k in 1:r) {
    for (l in k:r) {
      list_of_interest <- matrix_list[[l]][[k]]
      target_space <- matrix_list[[l]][[l]]
      if (length(list_of_interest) == 0) {
        next
      }
      for (i in 1:length(list_of_interest)) {
        for (j in i:length(list_of_interest)) {
          multiplied_matrix <- list_of_interest[[i]] %*% t(list_of_interest[[j]])
          if (!isAinSpanBaseMatrices_list(multiplied_matrix + t(multiplied_matrix), target_space)) { # Watch out if `vector_in_span()` will be done speciffic for 0-1 matrices. This is not a 0-1 matrix
            return(FALSE)
          }
        }
      }
    }
  }
  return(TRUE)
}

#' Check whether (M2) condition is satisfied
#' @noRd
is_M2_condition_satisfied <- function(n, cumsum_n, matrix_list) {
  r <- length(cumsum_n)
  if (r < 2) {
    return(TRUE)
  }
  for (k in 1:(r - 1)) {
    for (l in k:(r - 1)) {
      for (m in (l + 1):r) {
        list_of_interest_1 <- matrix_list[[m]][[k]]
        list_of_interest_2 <- matrix_list[[l]][[k]]
        target_space <- matrix_list[[m]][[l]]
        if (length(list_of_interest_1) == 0 || length(list_of_interest_2) == 0) {
          next
        }
        for (i in 1:length(list_of_interest_1)) {
          for (j in 1:length(list_of_interest_2)) {
            multiplied_matrix <- list_of_interest_1[[i]] %*% t(list_of_interest_2[[j]])
            if (!isAinSpanBaseMatrices_list(multiplied_matrix, target_space)) {
              return(FALSE)
            }
          }
        }
      }
    }
  }
  return(TRUE)
}

#' Check if the given space is Ishi space
#'
#' @param vPartition partition of the vertices
#' @param ePartition partition of the edges
#'
#' @export
#'
#' @examples
#' vPartition <- list(c(1, 2, 3, 4), c(5, 6), c(7))
#' ePartition <- list(c(1, 12), c(4, 9, 14, 17), c(19), c(20, 21))
#'
#' is_Ishi_space(vPartition, ePartition)
is_Ishi_space <- function(vPartition, ePartition) {
  n <- sapply(vPartition, length)
  cumsum_n <- cumsum(n)

  matrix_list <- tryCatch(
    partitions_to_base_matrices(vPartition, ePartition),
    notEdgeRegular = function(c) NULL
  )

  if(is.null(matrix_list)){
    return(FALSE)
  }

  is_M0_condition_satisfied(n, cumsum_n, matrix_list) &&
    is_M1_condition_satisfied(n, cumsum_n, matrix_list) &&
    is_M2_condition_satisfied(n, cumsum_n, matrix_list)
}


# Old code:
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
