#' Maximal Partial Clique Computation
#'
#' This function computes the maximal partial clique with a given adjacency matrix and a edge density threshold.
#' It iterates through the nodes to identify the largest subset where each pair of nodes is densely connected enough.
#'
#' @param adj_mat A symmetric adjacency matrix with only values 0 or 1. It must have 1’s along its diagonal, no row- or column-names, and between 5 to 50 rows/columns (inclusive).
#' @param alpha A numeric value between 0.5 and 1 (inclusive) specifying the density requirement for the maximal partial clique.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list with named elements:
#' \item{clique_idx}{A numeric vector of index numbers corresponding to the nodes in the maximum partial clique.}
#' \item{edge_density}{The percentage of edges in adj_mat among the nodes in clique_idx.}
#'
#' @export

compute_maximal_partial_clique6 <- function(adj_mat, alpha, ...) {

  # Check validity of adj_mat
  if (!is.matrix(adj_mat) || !identical(adj_mat, t(adj_mat)) ||
      any(diag(adj_mat) != 1) || any(adj_mat < 0) || any(adj_mat > 1) ) {
    stop("adj_mat must be a symmetric matrix with 1's along the diagonal and only values 0 or 1.")
  }

  if(any(rownames(adj_mat)) || any(colnames(adj_mat)) ||
     nrow(adj_mat) < 5 || nrow(adj_mat) > 50 ) {
    stop("adj_mat must have no row-/column-names and have number of rows between 5 and 50 (inclusive)")
  }

  # Check validity of alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0.5 || alpha > 1) {
    stop("Alpha must be a single numeric value between 0.5 and 1.")
  }

  # Compute maximal partial clique
  # Initialization
  n <- nrow(adj_mat)
  clique_idx <- integer(0)
  max_density <- 0

  for (i in 1:n) {

    # Initialize temporary node and temporary edge_density
    temp_idx <- c()
    temp_edge <- 0
    temp_density <- 0

    for (j in 1:n) {

      # Check if node "j" can be added to the current partial clique
      if (all(adj_mat[temp_idx, temp_idx] == 1) && # Ensures all diagonals are 1,
          all(adj_mat[temp_idx, j] == 1) && # there is an edge from the current node to node"j"
          all(adj_mat[j,temp_idx] == 1)) { # there is an edge from node "j" to the current node

        # Update the current clique
        temp_idx <- c(temp_idx, j)
        temp_edge <- temp_edge + 1
      }
    }

    # Edge density = num of edges found/max num of possible edges in current clique
    m <- length(temp_idx)
    temp_density <- (sum(adj_mat[temp_idx, temp_idx])-m)/2 / (m*(m-1)/2)

    if (length(temp_idx) > 1 && # Ensures the partial clique has at least two nodes (one edge),
        temp_density >= alpha && # the current edge density meets the threshold set by the user
        temp_density > max_density) { # and is large than the maximum edge density found
      # Update the edge density
      clique_idx <- temp_idx
      max_density <- temp_density
    }
  }

  # Compute edge density
  edge_density <- max_density

  # Return results
  return(list(clique_idx = clique_idx, edge_density = edge_density))
}
