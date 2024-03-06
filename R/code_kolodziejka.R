library(partitions)
library(gRbase)
library(igraph)
library(Matrix)

p <- 3

edges <- construct_edges(p)

allPartitions <- partitions::listParts(p*(p-1)/2)

myPartition <- allPartitions[[2]]


for(i in 2:length(allPartitions)) { #dla i=1 sie sypie ECC
  myPartition <- allPartitions[[i]]

  ECC <- PartitionToEdgeColorClasses(myPartition,edges)

  baseMatrices <- ECCtoBM(ECC,p)

  print(ECC) #full graph
  print(IshiCondition(baseMatrices))

  #listing graphs which are decomposable and 1:p is their peo
  A <- matrix(1, ncol=p, nrow = p)-diag(p)
  for(j in 2:dim(baseMatrices)[3]){
    #AdjacencyMatrix <- A #full graph
    AdjacencyMatrix <- A - baseMatrices[,,j]
    is_1p_peo <- is_in_list(as.numeric(1:p),perfect_elimination_orderings(AdjacencyMatrix))
    if(is_1p_peo){
      print(ECC[-j])
      print(IshiCondition(baseMatrices[,,-j])) #cos za czesto FALSE na moj gust
    }
  }
}


##################
construct_edges <- function(p) {
  pairs_list <- list()

  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      pairs_list <- c(pairs_list, list(c(i, j)))
    }
  }
  return(pairs_list)
}
##################
PartitionToEdgeColorClasses <- function(partition, edges) {
  sapply(partition, function(p) edges[p])
}
##################
E <- function(pair,p) {
  J <- matrix(0, p, p)
  J[pair[[1]],pair[[2]]] <- 1
  J[pair[[2]],pair[[1]]] <- 1
  return(J)
}
##################
ECCtoBM <- function(ECC, p) {
  baseMatrices <- array(0, c(p, p, length(ECC) + 1))
  baseMatrices[,,1] <- diag(p)

  for (i in seq_along(ECC)) {
    tmp <- matrix(0, p, p)
    for (j in seq_along(ECC[[i]])) {
      if(length(ECC[[i]][[j]])>1)
      {tmp <- tmp + E(ECC[[i]][[j]], p)}
      else {tmp <- E(ECC[[i]],p); break}
    }
    baseMatrices[,, i + 1] <- tmp
  }

  return(baseMatrices)
}


##################
isAinSpanBMs <- function(A, BMs) {
  B <- matrix(c(BMs), ncol = dim(BMs)[3])
  return(vector_in_span(c(A),B))
}
##################
IshiCondition <- function(BMs){
  ncol <- dim(BMs)[3]
  tmp <- TRUE
  for(i in 2:ncol) {
    for( j in i:ncol) {
      tmp <- tmp & isAinSpanBMs(BMs[,,i] %*%  BMs[,,j], BMs)
    }
  }
  return(tmp)
}

##################
vector_in_span <- function(v, A) {
  augmented_matrix <- cbind(A, v)
  return(rankMatrix(augmented_matrix)[1] == rankMatrix(A)[1])
}
##################
is_simplicial <- function(v, adj) { #adj - macierz wymiaru dim, v to element 1:dim
  ne <-   which(adj[v,]==1)
  for (x in ne) {
    for (y in ne) {
      if (x!=y & adj[x,y]==0) { return(FALSE) }
    }
  }
  return(TRUE)
}
##################
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
##################
is_in_list <- function(el, list){
  return(any(sapply(list, function(x) identical(x, el))))
}
