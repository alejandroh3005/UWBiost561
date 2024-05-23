#' Find maximal partial clique
#'
#' @param adj_mat matrix
#' @param alpha value
#'
#' @return list
#' @export
compute_maximal_partial_clique16 <- function(adj_mat, alpha) {
  # Validate inputs
  stopifnot(is.matrix(adj_mat),
            all(adj_mat == t(adj_mat)),
            all(diag(adj_mat) == 1),
            is.null(dimnames(adj_mat)),
            nrow(adj_mat) >= 5,
            nrow(adj_mat) <= 50,
            is.numeric(alpha),
            length(alpha) == 1,
            alpha >= 0.5,
            alpha <= 1)

  n <- nrow(adj_mat)

  # Initialize best solution found
  best_clique_idx <- integer(0)
  best_edge_density <- 0

  # Test all possible subsets of nodes
  # Note: This brute-force approach is not computationally feasible for larger matrices
  for (size in n:1) {
    if (best_edge_density >= alpha) break  # Early exit if good enough solution found

    utils::combn(n, size, function(nodes) {
      submat <- adj_mat[nodes, nodes]
      num_edges <- (sum(submat) - length(nodes)) / 2
      possible_edges <- size * (size - 1) / 2
      current_density <- num_edges / possible_edges

      if (current_density >= alpha && current_density > best_edge_density) {
        best_clique_idx <<- nodes
        best_edge_density <<- current_density
      }
    }, simplify = FALSE)
  }

  # Return the result
  return(list(clique_idx = best_clique_idx, edge_density = best_edge_density))
}
