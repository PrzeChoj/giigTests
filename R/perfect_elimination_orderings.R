#' Find out if the vertev v is simplicial
#'
#' @inheritParams perfect_elimination_orderings
#' @param v vertex of interest
#'
#' @export
#'
#' @examples
#' M <- matrix(c(
#'   0, 0, 0, 0,
#'   0, 0, 1, 0,
#'   0, 1, 0, 1,
#'   0, 0, 1, 0
#' ), nrow = 4) # 1 2-3-4
#' sapply(1:4, function(v) {
#'   is_simplicial(v, M)
#' }) # TRUE, TRUE, FALSE, TRUE
is_simplicial <- function(v, adj) { # adj - macierz wymiaru dim, v to element 1:dim
  ne <- which(adj[v, ] == 1)
  for (x in ne) {
    for (y in ne) {
      if (x != y & adj[x, y] == 0) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#' Find all the Perfect Elimination Orderings (PEOs)
#'
#' @param adj Adjacency matrix of a graph of interest
#'
#' @export
#'
#' @examples
#' M <- matrix(c(
#'   0, 0, 0, 0,
#'   0, 0, 1, 0,
#'   0, 1, 0, 1,
#'   0, 0, 1, 0
#' ), nrow = 4) # 1 2-3-4
#' perfect_elimination_orderings(M) # list of 16 PEOs
perfect_elimination_orderings <- function(adj) { # adj - macierz wymiaru co najmniej 2x2
  n <- nrow(adj)
  if (n == 2) {
    return(list(c(1, 2), c(2, 1)))
  }

  orders <- list()
  for (v in 1:n) {
    if (is_simplicial(v, adj)) {
      suborders <- perfect_elimination_orderings(adj[-c(v), -c(v)])
      # Znajdujemy doskonale porzadki w grafie bez wierzcholka v
      # Usuniecie wierzcholka v zaburza nam numeracje
      # Do kazdego z otrzymanych porzadkow chcemy dopisac wierzcholek v na poczatku
      # Musimy uwzglednic zmiane numeracji
      for (j in 1:length(suborders)) {
        for (k in 1:length(suborders[[j]]))
        { # uwzglednienie przenumerowania wierzcholkow
          if (suborders[[j]][k] >= v) {
            suborders[[j]][k] <- suborders[[j]][k] + 1
          }
        }
        order <- c(v, suborders[[j]]) # na poczatek kladziemy wierzcholek simplicjalny
        orders[[length(orders) + 1]] <- order
      }
    }
  }
  return(orders)
}
