extract_subgraph <- function(graph, nodes, distance = 1, directed = TRUE) {
  vertices <- ego(graph, nodes = nodes, order = distance)[[1]]
  igraph_subgraph <- induced_subgraph(graph, vids = vertices)
  nodes_df <- as_data_frame(igraph_subgraph, what = "vertices")
  edges_sf <- as_data_frame(igraph_subgraph, what = "edges")
  tbl_graph(nodes=nodes_df, edges=edges_sf, directed=directed)
}