#' Find the maximal partial clique
#'
#' Computes the largest partial clique given an adjacency matrix \code{adj_mat}
#' and a required edge density \code{alpha}. This method computes the edge density of the partial clique
#' over all possible node combinations and selects out the one with the largest edge density.
#'
#' @param adj_mat an adjacency matrix with no row- or column-names,
#' between 5 to 50 rows/columns (inclusive).
#' @param alpha a value between 0.5 and 1 (inclusive), edge density
#'
#' @return a list with element \code{clique_ind}, a vector of index numbers of the largest partial clique,
#' and element \code{edge_density}, percentage of edges in \code{adj_mat} among the nodes in \code{clique_idx}
#' @export
compute_maximal_partial_clique21 <- function(adj_mat, alpha) {
  # Check inputs
  if (!is.matrix(adj_mat) || any(adj_mat != t(adj_mat)) || any(diag(adj_mat) != 1) ||
      any(!is.null(row.names(adj_mat))) || nrow(adj_mat) < 5 || nrow(adj_mat) > 50) {
    stop("Invalid adj_mat")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0.5 || alpha > 1) {
    stop("Invalid alpha")
  }

  # Initialize variables
  n <- nrow(adj_mat)
  max_clique_size <- 0
  max_clique <- NULL

  # Iterate over all possible node combinations to find the maximal partial clique
  for (i in 2:n) {
    for (combo in utils::combn(1:n, i, simplify = FALSE)) {
      subgraph <- adj_mat[combo, combo]

      # length of the vector of index numbers corresponding to the nodes
      m = length(combo)

      # handle a diagonal matrix
      if (sum(subgraph) == m & max_clique_size == 0) {
        max_clique_size <- 1
        max_clique <- 1
        edge_density <- 0
        max_edge_density <- edge_density
      }
      else {
        # compute edge density in current partial clique
        edge_density_numerator <- (sum(subgraph) - m) / 2
        edge_density_denominator <- m * (m - 1) / 2
        edge_density <- edge_density_numerator / edge_density_denominator
      }

      # Check if the current combination is a valid partial clique:
      # edge density >= alpha
      if (edge_density >= alpha && m > max_clique_size) {
        max_clique_size <- m
        max_clique <- combo
        max_edge_density <- edge_density
      }
    }
  }

  if(length(max_clique) == 1) max_edge_density <- 1

  # Return the result as a list
  return(list(clique_idx = max_clique, edge_density = max_edge_density))
}
