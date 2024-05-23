#' Compute the largest partial clique given an adjacency matrix
#' and a required edge density
#'
#' @param adj_mat an adjacency matrix with only values 0 or 1
#' @param alpha required edge density
#'
#' @return a list of index numbers corresponding to the nodes
#' of maximal partial clique
#' @export
compute_maximal_partial_clique2 <- function(adj_mat, alpha) {

  # Check all the arguments are correct
  stopifnot(dim(adj_mat)[1] == dim(adj_mat[2]),
            adj_mat == t(adj_mat),
            adj_mat == 0 | adj_mat == 1,
            dim(adj_mat)[1] <= 50, dim(adj_mat)[1] >= 5,
            alpha >= 0.5, alpha <= 1)

  # Clique list
  graph_adj <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected")
  clique_list <- lapply(igraph::cliques(graph_adj, min = 3), function(x){
    as.numeric(x)
  })
  current_largest_clique <- igraph::largest_cliques(graph_adj)
  clique_idx <- as.numeric(current_largest_clique[[1]])

  if (length(clique_list) != 1) {
    clique_combination <- utils::combn(length(clique_list), 2)

    for (i in 1:ncol(clique_combination)) {
      # Extract the two cliques to combine
      clique1 <- clique_list[[clique_combination[1, i]]]
      clique2 <- clique_list[[clique_combination[2, i]]]

      # Combine the cliques
      new_clique <- sort(union(clique1, clique2))
      m <- length(new_clique)

      # Check whether new clique is satisfy
      if ((sum(adj_mat[new_clique, new_clique])-m)/2 >= alpha*m*(m-1)/2 &&
          length(clique_idx) < m){
        clique_idx <- new_clique
      }
    }
  }

  nodes <- length(clique_idx)
  # calculate edge-density
  edge_density_numerator <- (sum(adj_mat[clique_idx,clique_idx]) - nodes)/2
  edge_density_denominator <- nodes*(nodes-1)/2
  edge_density <- edge_density_numerator/edge_density_denominator

  return(list(clique_idx = sort(clique_idx),
              edge_density = edge_density))
}
