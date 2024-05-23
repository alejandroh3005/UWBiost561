#' Compute the Maximal Partial Clique
#'
#' This function calculates the largest subset of nodes within an adjacency matrix that forms a partial clique meeting a specified edge density threshold. The function employs a heuristic approach to handle the computationally challenging nature of the task.
#'
#' @param adj_mat A square symmetric adjacency matrix where each element is either 0 or 1, and the diagonal elements are 0.
#' @param alpha A numeric value between 0.5 and 1 that specifies the minimum required edge density for the partial clique.
#' @param verbose boolean
#'
#' @return A list containing:
#' \itemize{
#'   \item{clique_idx}{A numeric vector of indices representing the nodes in the maximal partial clique.}
#'   \item{edge_density}{The edge density of the found clique.}
#' }
#'
#' @export
compute_maximal_partial_clique20 <- function(adj_mat, alpha, verbose = 0) {
  if(verbose > 1) print(paste("Alpha:", alpha))  # This will show what alpha value is being tested
  if(verbose > 1) print(paste("Is alpha numeric and of length 1?", is.numeric(alpha) && length(alpha) == 1))
  if(verbose > 1) print(paste("Is alpha within range?", alpha >= 0.5 && alpha <= 1))
  stopifnot(is.matrix(adj_mat))  # Check if input is a matrix
  stopifnot(length(alpha) == 1 && is.numeric(alpha) && alpha >= 0.5 && alpha <= 1)
  stopifnot(all(adj_mat == 0 | adj_mat == 1), diag(adj_mat) == 1)
  stopifnot(nrow(adj_mat) == ncol(adj_mat)) # Ensure square matrix

  # Ensuring it's a binary matrix with zero diagonal
  diag(adj_mat) <- 0

  # Early exit conditions for edge cases
  if (is.null(adj_mat) || nrow(adj_mat) <= 0) {
    return(list(clique_idx = NULL, edge_density = NA))
  }

  n <- nrow(adj_mat)
  best_clique <- NULL
  best_density <- 0

  # Iterate over all subsets of nodes
  for (size in n:1) {
    for (nodes in utils::combn(n, size, simplify = FALSE)) {
      subgraph <- adj_mat[nodes, nodes]
      num_edges <- sum(subgraph) / 2
      max_edges <- size * (size - 1) / 2
      density <- if (max_edges > 0) num_edges / max_edges else 0 # Avoid division by zero

      if (!is.na(density) && density >= alpha) {
        if (is.null(best_clique) || length(nodes) > length(best_clique)) {
          best_clique <- nodes
          best_density <- density
        }
      }
    }
    if (!is.null(best_clique)) break # Early exit if a clique has been found
  }

  return(list(clique_idx = best_clique, edge_density = best_density))
}
