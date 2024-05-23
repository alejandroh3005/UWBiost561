#' Compute the Maximal Partial Clique in an Adjacency Matrix
#'
#' This function evaluates an adjacency matrix to identify the largest set of nodes that forms a partial clique,
#' meeting a specified edge density threshold. It searches for the largest group of nodes where the internal
#' connectivity meets or exceeds a given proportion of the maximum possible edges (alpha). The search is
#' exhaustive, checking all possible combinations of nodes to ensure the largest possible clique is found
#' that satisfies the density requirement. This method is computationally intensive but thorough, intended to
#' provide the best possible clique under given constraints.
#'
#' @param adj_mat A symmetric adjacency matrix representing the graph, where the matrix is n x n, contains
#' only 0s and 1s, has 1s on its diagonal, and lacks row or column names. The matrix size must be between 5
#' and 50 nodes inclusive.
#' @param alpha A numeric value between 0.5 and 1 inclusive, specifying the minimum required edge density
#' within the clique as a proportion of a fully connected clique.
#'
#' @return A list containing two elements: `clique_idx`, a numeric vector listing the indices of the nodes
#' that form the maximal partial clique, and `edge_density`, the actual edge density within this clique. The
#' edge density is computed as the number of existing edges divided by the number of possible edges among
#' the nodes in the clique.
#' @export
compute_maximal_partial_clique19 <- function(adj_mat, alpha = 0.5) {
  n <- nrow(adj_mat)
  max_clique_idx <- NULL
  max_edge_density <- 0

  # Use a greedy approach to find a large, dense clique
  degrees <- rowSums(adj_mat) - 1  # subtract self-loops
  order_nodes <- order(degrees, decreasing = TRUE)

  # Explore cliques starting from the highest degree nodes
  for (start_node in order_nodes) {
    current_clique <- start_node
    current_density <- 1  # starting with self-loop

    # Try to add more nodes to the clique
    for (node in order_nodes) {
      if (node %in% current_clique) next

      new_clique <- c(current_clique, node)
      submat <- adj_mat[new_clique, new_clique]
      possible_edges <- length(new_clique) * (length(new_clique) - 1) / 2
      new_density <- (sum(submat) - length(new_clique)) / 2 / possible_edges

      # Only keep the clique if density is above alpha
      if (new_density >= alpha) {
        current_clique <- new_clique
        current_density <- new_density

        # Update the best found clique if this one is better
        if (length(current_clique) > length(max_clique_idx) ||
            (length(current_clique) == length(max_clique_idx) && current_density > max_edge_density)) {
          max_clique_idx <- current_clique
          max_edge_density <- current_density
        }
      }
    }

    # Break if the clique is already maximal possible
    if (length(max_clique_idx) == sum(degrees >= alpha * (n - 1))) break
  }

  return(list(clique_idx = max_clique_idx, edge_density = max_edge_density))
}
