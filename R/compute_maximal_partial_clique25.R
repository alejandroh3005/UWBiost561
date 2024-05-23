#' This function identifies the largest partial clique based on the given adjacency matrix
#' and a required edge density.
#'
#' @param adj_mat A symmetric matrix (0-1) representing the adjacency relationships among nodes.
#' @param alpha A numeric value between 0.5 and 1 (inclusive) representing the required edge density.
#'
#' @return A list containing the index vector of nodes in the maximal partial clique (clique_idx)
#'         and the computed edge density (edge_density).
#' @export
#'
compute_maximal_partial_clique25 <- function(adj_mat, alpha) {
  # Check input validity
  if (!is.matrix(adj_mat) || !identical(adj_mat, t(adj_mat)) ||
      any(diag(adj_mat) != 1) || nrow(adj_mat) < 5 || nrow(adj_mat) > 50) {
    stop("adj_mat must be a symmetric matrix with dimensions between 5 and 50.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0.5 || alpha > 1) {
    stop("alpha must be a numeric value between 0.5 and 1 (inclusive).")
  }

  # Initialize variables
  n <- nrow(adj_mat)
  max_clique_size <- 1
  max_clique_idx <- 1
  max_edge_density <- 0

  # Iterate through all nodes to find the largest partial clique
  for (i in 1:n) {
    neighbors <- which(adj_mat[i, ] == 1)
    for (j in neighbors) {
      common_neighbors <- unique(intersect(neighbors, which(adj_mat[j, ] == 1)))
      m <- length(common_neighbors) + 2
      edge_density <- sum(adj_mat[common_neighbors, common_neighbors]) / (m * (m - 1) / 2)
      if (edge_density >= alpha && m > max_clique_size) {
        max_clique_size <- m
        max_clique_idx <- unique(c(i, j, common_neighbors))
        max_edge_density <- edge_density
      }
    }
  }

  if(length(max_clique_idx) == 1) max_edge_density <- 1

  # Return the results as a list
  return(list(clique_idx = max_clique_idx, edge_density = max_edge_density))
}
