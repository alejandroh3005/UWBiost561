check_clique_12 = function(mat, idx, alpha) {
  m = length(idx)
  return((sum(mat[idx, idx]) - m) / 2 >= alpha * m * (m - 1) / 2)
}

calculate_edge_density_12 = function(adj_mat, clique_idx) {
  if(length(clique_idx) == 1) return(1)

  num_nodes <- length(clique_idx)
  num_edges <- (sum(adj_mat[clique_idx, clique_idx]) - num_nodes) / 2
  density <- num_edges / (num_nodes * (num_nodes - 1) / 2)
  return(density)
}

#' Title
#'
#' @param adj_mat matrix
#' @param alpha value
#'
#' @return list
#' @export
compute_maximal_partial_clique12 <- function(adj_mat, alpha) {
  # Check adj_mat
  if (!is.matrix(adj_mat) || nrow(adj_mat) != ncol(adj_mat) || any(adj_mat != t(adj_mat)) ||
      any(diag(adj_mat) != 1) || any(adj_mat < 0) || any(adj_mat > 1)) {
    stop("adj_mat must be a symmetric matrix with values 0 or 1 and 1s along the diagonal.")
  }
  if (nrow(adj_mat) < 5 || nrow(adj_mat) > 50) {
    stop("adj_mat must have between 5 to 50 rows/columns (inclusive).")
  }

  # Check alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0.5 || alpha > 1) {
    stop("alpha must be a single numeric value between 0.5 and 1 (inclusive).")
  }

  n = dim(adj_mat)[1]
  mpc_idx = c(1:n)
  most_sparse = 0
  curr_clique = adj_mat[mpc_idx, mpc_idx]

  while(!check_clique_12(adj_mat, mpc_idx, alpha)) {
    most_sparse = mpc_idx[which.min(rowSums(adj_mat[mpc_idx,mpc_idx]))]
    mpc_idx = setdiff(mpc_idx, most_sparse)
  }

  edge_density = calculate_edge_density_12(adj_mat, mpc_idx)

  return(list(clique_idx = mpc_idx, edge_density = edge_density))
}
