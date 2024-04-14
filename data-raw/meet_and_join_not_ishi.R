library(giigTests)

all_ishi_full_graphs <- all_ishi_full_graphs_p_4
p <- 4
allVertexPartitions <- partitions::listParts(p) # podział na kolory wierzcholkow
allEdges <- construct_edges(p)
allEdgePartitions <- partitions::listParts(p * (p - 1) / 2) # podział na kolory krawędzi

ile <- 0
for (i in 1:(nrow(all_ishi_full_graphs) - 1)) {
  this_graph_i <- all_ishi_full_graphs[i, ]
  for (j in (i + 1):nrow(all_ishi_full_graphs)) {
    this_graph_j <- all_ishi_full_graphs[j, ]
    meeted_graphs <- join(
      allVertexPartitions[[this_graph_i[[1]]]], allEdgePartitions[[this_graph_i[[2]]]],
      allVertexPartitions[[this_graph_j[[1]]]], allEdgePartitions[[this_graph_j[[2]]]]
    )

    if(!is_Ishi_space(meeted_graphs[[1]], meeted_graphs[[2]])) {
      ile <- ile + 1
      ic(i)
      ic(j)
      print("")
    }
  }
}
ile


# meeting of 2 Ishi that is not Ishi
# i = 9
# j = 11
this_graph_i <- all_ishi_full_graphs[9, ]
allVertexPartitions[[this_graph_i[[1]]]]
allEdgePartitions[[this_graph_i[[2]]]]
is_Ishi_space(allVertexPartitions[[this_graph_i[[1]]]], allEdgePartitions[[this_graph_i[[2]]]])

this_graph_j <- all_ishi_full_graphs[11, ]
allVertexPartitions[[this_graph_j[[1]]]]
allEdgePartitions[[this_graph_j[[2]]]]
is_Ishi_space(allVertexPartitions[[this_graph_j[[1]]]], allEdgePartitions[[this_graph_j[[2]]]])

meeted_graphs <- meet(
  allVertexPartitions[[this_graph_i[[1]]]], allEdgePartitions[[this_graph_i[[2]]]],
  allVertexPartitions[[this_graph_j[[1]]]], allEdgePartitions[[this_graph_j[[2]]]]
)

is_Ishi_space(meeted_graphs[[1]], meeted_graphs[[2]])


# joining of 2 Ishi that is not Ishi
# i = 3
# j = 11
this_graph_i <- all_ishi_full_graphs[3, ]
allVertexPartitions[[this_graph_i[[1]]]]
allEdgePartitions[[this_graph_i[[2]]]]
is_Ishi_space(allVertexPartitions[[this_graph_i[[1]]]], allEdgePartitions[[this_graph_i[[2]]]])

this_graph_j <- all_ishi_full_graphs[11, ]
allVertexPartitions[[this_graph_j[[1]]]]
allEdgePartitions[[this_graph_j[[2]]]]
is_Ishi_space(allVertexPartitions[[this_graph_j[[1]]]], allEdgePartitions[[this_graph_j[[2]]]])

joined_graphs <- join(
  allVertexPartitions[[this_graph_i[[1]]]], allEdgePartitions[[this_graph_i[[2]]]],
  allVertexPartitions[[this_graph_j[[1]]]], allEdgePartitions[[this_graph_j[[2]]]]
)

is_Ishi_space(joined_graphs[[1]], joined_graphs[[2]])


