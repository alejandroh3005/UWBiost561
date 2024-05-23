#' Title
#'
#' @param adj_mat adjacency matrix
#' @param alpha edge density
#'
#' @return a list
#' @export
compute_maximal_partial_clique13 <- function (adj_mat, alpha) {
  stopifnot(is.matrix(adj_mat),
            identical(dim(adj_mat)[1], dim(adj_mat)[2]),
            all(adj_mat >= 0),
            all(adj_mat <= 1),
            all(adj_mat%%1 == 0),
            alpha >= 0.5, alpha <= 1)
  g1 <- igraph::graph_from_adjacency_matrix(adj_mat, mode="undirected")
  clique_all <- igraph::cliques(g1, min = 3)

  max_clique <- igraph::largest_cliques(g1)


  #discuss with classmate, their suggestion is to further find
  #combined clique compared with the max_clique
  #however, I couldn't develop the method before the deadline
  #for (i in 1:ncol(comb_nodes)) {
  #comb_index <- c(comb_index, list(unique(c(as.numeric(comb_nodes[1, i][[1]]),
  #as.numeric(comb_nodes[2, i][[1]])))))
  clique_idx <- as.vector(max_clique[[1]])

  m <- length(clique_idx)

  edge_density_numerator <- (sum(adj_mat[clique_idx,clique_idx]) - m)/2
  edge_density_denominator <- m*(m-1)/2
  edge_density <- edge_density_numerator/edge_density_denominator

  return(list(clique_idx = clique_idx,
              edge_density = edge_density))
}
