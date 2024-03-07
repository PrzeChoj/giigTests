is_simplicial <- function(v, adj) { #adj - macierz wymiaru dim, v to element 1:dim
  ne <-   which(adj[v,]==1)
  for (x in ne) {
    for (y in ne) {
      if (x!=y & adj[x,y]==0) { return(FALSE) }
    }
  }
  return(TRUE)
}

perfect_elimination_orderings <- function(adj) {  #adj - macierz wymiaru co najmniej 2x2
  n <- nrow(adj)
  if(n==2) { return(list(c(1,2), c(2,1))) }

  orders <- list()
  for(v in 1:n){
    if(is_simplicial(v,adj)){
      suborders <- perfect_elimination_orderings(adj[-c(v),-c(v)])
      #Znajdujemy doskonale porzadki w grafie bez wierzcholka v
      #Usuniecie wierzcholka v zaburza nam numeracje
      #Do kazdego z otrzymanych porzadkow chcemy dopisac wierzcholek v na koncu
      #Musimy uwzglednic zmiane numeracji
      for (j in 1:length(suborders)){
        for (k in 1:length(suborders[[j]]))
        { #uwzglednienie przenumerowania wierzcholkow
          if (suborders[[j]][k] >= v) { suborders[[j]][k]  = suborders[[j]][k]  +1}
        }
        order <- c(suborders[[j]],v) # na koniec kladziemy wierzcholek simplicjalny
        orders[[length(orders) + 1]] <- order
      }
    }
  }
  return(orders)
}
