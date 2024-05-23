#' Compute the Maximal Partial Clique
#'
#' This function identifies the largest subset of nodes in an adjacency matrix
#' that forms a partial clique meeting a specified minimum edge density (`alpha`).
#'
#' @param adj_mat A symmetric matrix representing the adjacency matrix of a graph.
#' @param alpha A numeric value between 0.5 and 1 indicating the minimum edge density.
#'
#' @return A list with the indices of the nodes and the edge density of the clique.
#'
#' @export
compute_maximal_partial_clique22 <- function(adj_mat, alpha) {
  n <- nrow(adj_mat)
  best_clique_idx <- NULL
  max_size <- 0
  max_density <- 0

  # Try to find maximal partial clique starting from each node
  for (start in 1:n) {
    current_clique <- start
    potential_adds <- setdiff(1:n, current_clique)

    improving <- TRUE
    while (improving && length(potential_adds) > 0) {
      improving <- FALSE
      for (node in potential_adds) {
        trial_clique <- c(current_clique, node)
        m <- length(trial_clique)
        submat <- adj_mat[trial_clique, trial_clique]
        edge_density_numerator <- (sum(submat) - m) / 2
        edge_density_denominator <- m * (m - 1) / 2
        edge_density <- edge_density_numerator / edge_density_denominator

        if (edge_density >= alpha) {
          current_clique <- trial_clique
          max_density <- edge_density
          improving <- TRUE
          break
        }
      }
      potential_adds <- setdiff(potential_adds, current_clique)
    }

    # Check if the current clique is the largest found so far
    if (length(current_clique) > max_size) {
      max_size <- length(current_clique)
      best_clique_idx <- current_clique
    }
  }

  # Calculate final edge density
  final_clique_mat <- adj_mat[best_clique_idx, best_clique_idx]
  final_edge_count <- (sum(final_clique_mat) - max_size) / 2
  total_possible_edges <- max_size * (max_size - 1) / 2
  final_edge_density <- final_edge_count / total_possible_edges

  return(list(clique_idx = best_clique_idx, edge_density = final_edge_density))
}

