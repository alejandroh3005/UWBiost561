#' Compute maximal partial clique
#'
#'Computes the maximal partial clique of a given adjacency matrix
#'
#' @param adj_mat adjacency matrix - symmetric matrix of 0s and 1s with 1s along the diagonal and between 5 to 50 rows/columns
#' @param alpha edge density
#'
#' @return vector with index numbers of nodes in maximal partial clique along with the edge density
#' @export
compute_maximal_partial_clique4 <- function(adj_mat, alpha) {
  # check if adj_mat is a symmetric matrix with only values 0 or 1
  stopifnot(is.matrix(adj_mat), all(adj_mat == t(adj_mat)), all(adj_mat %in% c(0, 1)))

  # check if adj_mat has 1's along the diagonal
  stopifnot(all(diag(adj_mat) == 1))

  # check if adj_mat has no row or column names
  stopifnot(is.null(rownames(adj_mat)) && is.null(colnames(adj_mat)))

  # check if adj_mat has between 5 and 50 rows/columns
  stopifnot(nrow(adj_mat) >= 5 && nrow(adj_mat) <= 50 && ncol(adj_mat) >= 5 && ncol(adj_mat) <= 50)

  # check if alpha is a single numeric value between 0.5 and 1
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha >= 0.5, alpha <= 1)

  n <- nrow(adj_mat)
  # Function to check if a set of nodes forms a valid partial clique
  is_valid_partial_clique <- function(nodes) {
    m <- length(nodes)
    edge_density_numerator <- (sum(adj_mat[nodes, nodes]) - m) / 2
    edge_density_denominator <- m * (m - 1) / 2
    edge_density <- edge_density_numerator / edge_density_denominator
    return(edge_density >= alpha)
  }

  # Initialize clique_idx with a single node (node 1)
  clique_idx <- c(1)

  # Iterate over each node to find the maximal partial clique
  for (i in 2:n) {
    # Check if adding node i maintains the validity of the partial clique
    if (is_valid_partial_clique(c(clique_idx, i))) {
      clique_idx <- c(clique_idx, i)
    }
  }

  # Calculate the edge density of the maximal partial clique
  m <- length(clique_idx)
  edge_density_numerator <- (sum(adj_mat[clique_idx, clique_idx]) - m) / 2
  edge_density_denominator <- m * (m - 1) / 2
  edge_density <- edge_density_numerator / edge_density_denominator

  # Return the maximal partial clique indices and edge density
  return(list(clique_idx = clique_idx, edge_density = edge_density))
}
