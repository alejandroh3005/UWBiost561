#' Compute Maximal Partial Clique
#'
#' This function attempts to find the maximal partial clique with an edge density
#' at least as great as the specified alpha value in the given adjacency matrix.
#' The function searches for all possible combinations of nodes, evaluating each
#' set for the specified edge density.
#'
#' @param adj_mat A symmetric adjacency matrix where each element is either 0 or 1.
#'        This matrix represents the graph in which we are looking for the clique.
#'        The matrix should be square, with the diagonal elements set to 1.
#' @param alpha A numeric value between 0 and 1 inclusive, specifying the minimum
#'        edge density required for the partial clique. This is a threshold to decide
#'        which subsets of nodes form a valid clique based on their internal connectivity.
#'
#' @return A list containing two elements: `clique_idx`, a vector of node indices
#'         forming the maximal partial clique with the required density, and `edge_density`,
#'         the actual edge density of the found clique. If no clique meets the criteria,
#'         `clique_idx` will be an empty integer vector and `edge_density` will be 0.
#'
#' @export
compute_maximal_partial_clique9 <- function(adj_mat, alpha = 0.5) {
  n <- nrow(adj_mat)

  # Check matrix is square
  if (!is.matrix(adj_mat) || nrow(adj_mat) != ncol(adj_mat)) {
    stop("adj_mat must be a square matrix.")
  }

  # Check matrix dimensions
  if (nrow(adj_mat) < 5 || nrow(adj_mat) > 50) {
    stop("adj_mat must have between 5 and 50 rows/columns.")
  }

  # Check matrix is symmetric with elements 0 or 1
  if (!all(adj_mat == t(adj_mat)) || !all(adj_mat %in% c(0, 1))) {
    stop("adj_mat must be a symmetric matrix with elements 0 or 1.")
  }

  # Check all diagonal elements are 1
  if (!all(diag(adj_mat) == 1)) {
    stop("All diagonal elements of adj_mat must be 1.")
  }

  # Function to calculate edge density
  calculate_density <- function(nodes) {
    if (length(nodes) < 2) {
      return(NA)  # Return NA for sets of nodes that cannot possibly form a clique
    }

    subgraph <- adj_mat[nodes, nodes]
    # Remove diagonal elements for the edge count
    actual_edges <- (sum(subgraph) - length(nodes)) / 2
    possible_edges <- length(nodes) * (length(nodes) - 1) / 2

    if (possible_edges == 0) {  # This should never happen now, but just in case
      return(NA)
    }

    return(actual_edges / possible_edges)
  }

  # Try to find the maximal clique with the density above alpha
  for (size in n:1) {
    combinations <- utils::combn(n, size)
    for (i in 1:ncol(combinations)) {
      nodes <- combinations[, i]
      current_density <- calculate_density(nodes)
      if (!is.na(current_density) && current_density >= alpha) {
        return(list(clique_idx = nodes, edge_density = current_density))
      }
    }
  }

  # Return a single node if no clique found
  return(list(clique_idx = 1, edge_density = 1))
}
