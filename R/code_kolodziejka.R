# library(partitions)
# library(gRbase)
# library(igraph)
# library(Matrix)
#
#
# p <- 4
#
# edges <- construct_edges(p)
#
# allPartitions <- partitions::listParts(p*(p-1)/2) #podział na kolory krawędzi
#
# myPartition <- allPartitions[[1]]
#
#
# for(i in 1:length(allPartitions)) { #dla i=1 sie sypie ECC
#   myPartition <- allPartitions[[i]]
#
#   ECC <- PartitionToEdgeColorClasses(myPartition,edges)
#
#   baseMatrices <- ECCtoBM(ECC,p)
#   if(IshiCondition(baseMatrices)){
#   print(ECC) #full graph
#     print('-----------------------')
#   #print(IshiCondition(baseMatrices))
#   }
#
#   #listing graphs which are decomposable and 1:p is their peo
#   # A <- matrix(1, ncol=p, nrow = p)-diag(p)
#   # for(j in 2:dim(baseMatrices)[3]){
#   #   #AdjacencyMatrix <- A #full graph
#   #   AdjacencyMatrix <- A - baseMatrices[,,j]
#   #   is_1p_peo <- is_in_list(as.numeric(1:p),perfect_elimination_orderings(AdjacencyMatrix))
#   #   if(is_1p_peo&IshiCondition(baseMatrices[,,-j])){
#   #     print(ECC[-j])
#   #     print('ooooooooooooooooo')
#   #     #print(IshiCondition(baseMatrices[,,-j])) #cos za czesto FALSE na moj gust
#   #   }
#   # }
#   # decomposable is satisfied from delta-regularity
# }
#
