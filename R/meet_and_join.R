#' Make meet of two graphs according to Gehrmann
#'
#' @export
#' @examples
#' vPartition1 <- list(c(1, 3), c(2, 4))
#' vPartition2 <- list(c(1, 3), c(2), c(4))
#' ePartition1 <- list(c(1, 6), c(3, 4))
#' ePartition2 <- list(c(1, 4), c(3, 6), c(2))
#'
#' meet(vPartition1, ePartition1, vPartition2, ePartition2)
meet <- function(vPartition1, ePartition1, vPartition2, ePartition2) {
  vertices <- union(vPartition1, vPartition2)
  p <- max(unlist(vertices))
  e <- p * (p - 1) / 2
  edges1 <- unique(unlist(ePartition1))
  edges2 <- unique(unlist(ePartition2))
  edges <- intersect(edges1, edges2)
  for (i in 1:length(ePartition1)) {
    if (!all(ePartition1[[i]] %in% edges)) {
      id <- which(!ePartition1[[i]] %in% edges)
      if (length(ePartition1[[i]] == 1)) {
        ePartition1[[i]] <- NULL
      } else {
        ePartition1[[i]][id] <- NULL
      }
    }
  }
  for (i in 1:length(ePartition2)) {
    if (!all(ePartition2[[i]] %in% edges)) {
      id <- which(!ePartition2[[i]] %in% edges)
      if (length(ePartition2[[i]] == 1)) {
        ePartition2[[i]] <- NULL
      } else {
        ePartition2[[i]][id] <- NULL
      }
    }
  }


  while (any(table(unlist(vertices)) > 1)) {
    i <- which(table(unlist(vertices)) > 1)[1]
    el <- as.numeric(names(table(unlist(vertices)))[i])
    which_union <- c()
    for (j in 1:length(vertices)) {
      if (any(el %in% vertices[[j]])) {
        which_union <- append(which_union, j)
      }
    }
    for (k in which_union[-1]) {
      vertices[[which_union[1]]] <- union(vertices[[which_union[1]]], vertices[[k]])
      vertices[[k]] <- NULL
    }
  }


  edgeColors <- union(ePartition1, ePartition2)
  while (any(table(unlist(edgeColors)) > 1)) {
    i <- which(table(unlist(edgeColors)) > 1)[1]
    el <- as.numeric(names(table(unlist(edgeColors)))[i])
    which_union <- c()
    for (j in 1:length(edgeColors)) {
      if (any(el %in% edgeColors[[j]])) {
        which_union <- append(which_union, j)
      }
    }
    for (k in which_union[-1]) {
      edgeColors[[which_union[1]]] <- union(edgeColors[[which_union[1]]], edgeColors[[k]])
      edgeColors[[k]] <- NULL
    }
  }
  return(list(vertices, edgeColors))
}


#' Make join of two graphs according to Gehrmann
#'
#' @export
#' @examples
#' vPartition1 <- list(c(1, 3), c(2, 4))
#' vPartition2 <- list(c(1, 3), c(2), c(4))
#' ePartition1 <- list(c(1, 6), c(3, 4))
#' ePartition2 <- list(c(1, 4), c(3, 6), c(2))
#'
#' join(vPartition1, ePartition1, vPartition2, ePartition2)
join <- function(vPartition1, ePartition1, vPartition2, ePartition2) {
  vertices <- intersect(vPartition1, vPartition2)
  vert <- union(unlist(vPartition1), unlist(vPartition2))
  if (!all(vert %in% unlist(vertices))) {
    id <- vert[which(!vert %in% unlist(vertices))]
    vertices <- append(vertices, as.list(id))
  }
  edges1 <- unique(unlist(ePartition1))
  edges2 <- unique(unlist(ePartition2))
  edge <- union(edges1, edges2)
  eg1 <- append(ePartition1, as.list(setdiff(edges2, edges1)))
  eg2 <- append(ePartition2, as.list(setdiff(edges1, edges2)))
  edgeColours <- intersect(eg1, eg2)
  if (!all(edge %in% unlist(edgeColours))) {
    id <- edge[which(!edge %in% unlist(edgeColours))]
    edgeColours <- append(edgeColours, as.list(id))
  }
  return(list(vertices, edgeColours))
}
