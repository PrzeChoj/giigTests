# install.packages("remotes")
# remotes::install_github("PrzeChoj/giigTests")
# library(giigTests)
# 
# vPartition1 <- list(c(1, 3), c(2, 4))
# vPartition2 <- list(c(1, 3), c(2), c(4))
# ePartition1 <- list(c(1, 6), c(3,4))
# ePartition2 <- list(c(1, 4), c(3,6),c(2))
# 
# is_Ishi_space(vPartition1, ePartition1)
# is_Ishi_space(vPartition2, ePartition2)

join <- function(vPartition1,ePartition1,vPartition2,ePartition2){
  vertices <- intersect(vPartition1, vPartition2)
  vert <- union(unlist(vPartition1),unlist(vPartition2))
  if (!all(vert %in% unlist(vertices))){
    id <- vert[which(!vert %in% unlist(vertices))]
    vertices <- append(vertices, as.list(id))
  }
  edges1 <- unique(unlist(ePartition1))
  edges2 <- unique(unlist(ePartition2))
  edge <- union(edges1, edges2)
  eg1 <- append(ePartition1, as.list(setdiff(edges2,edges1)))
  eg2 <- append(ePartition2, as.list(setdiff(edges1,edges2)))
  edgeColours <- intersect(eg1, eg2)
  if (!all(edge %in% unlist(edgeColours))){
    id <- edge[which(!edge %in% unlist(edgeColours))]
    edgeColours <- append(edgeColours, as.list(id)) 
  }
  return(list(vertices, edgeColours))
}
# 
# join(vPartition1,ePartition1,vPartition2,ePartition2)
# 
# v1 <- list(c(1,2,3),c(4))
# e1 <- list(c(1,2,4),c(3),c(5,6))
# v2 <- list(c(1,2),c(3),c(4))
# e2 <- list(c(1),c(2,4),c(3,5),c(6))
# is_Ishi_space(v1,e1) # TRUE
# is_Ishi_space(v2,e2) # TRUE
# 
# v <- join(v1,e1,v2,e2)[[1]]
# e <- join(v1,e1,v2,e2)[[2]]
# is_Ishi_space(v,e)
