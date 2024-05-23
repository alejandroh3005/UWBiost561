#' Compute the maximal partial clique function
#'
#' This function computes the largest partial clique given an adjacency matrix
#' \code{adj_mat} and a required edge density \code{alpha}.
#'
#' @param adj_mat An adjacency matrix representing the connections between nodes
#' in the graph. The matrix should be symmetric, with ones along the diagonal
#' representing self-loops and zeros or ones elsewhere representing absence or
#' presence of edges between nodes, respectively.
#' @param alpha The required edge density among the nodes in the maximal partial
#' clique. This value should be a numeric scalar between 0.5 and 1, inclusive.
#'
#' @return A list containing the index numbers of the nodes forming the maximal
#' partial clique (\code{clique_idx}) and the corresponding edge density
#' (\code{edge_density}) among these nodes.
#'
#' @details
#' The function iteratively searches for the largest partial clique in the graph
#' by removing nodes that do not meet the required edge density criterion.
#' It initializes the partial clique with all nodes in the graph and gradually
#' removes nodes until the remaining subset of nodes forms a partial clique with
#' an edge density greater than or equal to the specified \code{alpha}.
#' The resulting partial clique is returned along with its calculated edge density.
#'
#' @export
#'
compute_maximal_partial_clique10 <- function(adj_mat, alpha) {

  # Check all the arguments are correct
  stopifnot(is.matrix(adj_mat), is.numeric(alpha), length(alpha) == 1,
            all(adj_mat == t(adj_mat)),  # Check symmetry
            all(diag(adj_mat) == 1),     # Check diagonal
            is.null(rownames(adj_mat)),  # Check row names
            is.null(colnames(adj_mat)),  # Check column names
            nrow(adj_mat) >= 5, nrow(adj_mat) <= 50,
            ncol(adj_mat) >= 5, ncol(adj_mat) <= 50,
            alpha >= 0.5, alpha <= 1)

  # Define function to calculate edge density
  calculate_edge_density <- function(clique_idx) {
    m <- length(clique_idx)
    if (m <= 1) return(1)  # If only one node, edge density is 1
    edge_density_numerator <- (sum(adj_mat[clique_idx, clique_idx]) - m) / 2
    edge_density_denominator <- m * (m - 1) / 2
    return(edge_density_numerator / edge_density_denominator)
  }

  # Initialize clique_idx with all nodes
  clique_idx <- 1:nrow(adj_mat)
  edge_density <- calculate_edge_density(clique_idx)

  # Iterate through nodes to find maximal partial clique
  for (i in 1:nrow(adj_mat)) {
    # Remove node i from clique_idx
    new_clique_idx <- clique_idx[-i]
    new_edge_density <- calculate_edge_density(new_clique_idx)

    # Update clique_idx if edge density satisfies alpha
    if (new_edge_density >= alpha && length(new_clique_idx) > 1) {
      clique_idx <- new_clique_idx
      edge_density <- new_edge_density
    }
  }

  if(edge_density < alpha) {
    clique_idx <- 1
    edge_density <- 1
  }

  return(list(clique_idx = clique_idx, edge_density = edge_density))
}
