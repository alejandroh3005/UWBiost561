#' Compute the largest partial clique
#'
#' This function computes the largest partial clique given an adjacency matrix and a required edge density alpha.
#'
#' @param adj_mat The adjacency matrix representing the graph.
#' @param alpha The required edge density for the partial clique, must be between 0 and 1.
#'
#' @return A list containing:
#' \item{node_ids}{A vector containing the indices of the vertices in the computed maximal partial clique.}
#' \item{edge_density}{The edge density of the computed maximal partial clique.}
#'
#' @export
compute_maximal_partial_clique23 <- function(adj_mat, alpha) {
  # Check inputs
  .check_inputs_23(adj_mat, alpha)

  n <- nrow(adj_mat)

  # Make adj_mat diagonal = 0 so they don't count as edges
  diag(adj_mat) <- 0

  # Initialize variables to store the maximal clique
  maximal_clique <- numeric(0)
  maximal_size <- 0

  # Function to check if a subset satisfies the edge density condition
  is_partial_clique <- function(subset_vertices) {
    if (length(subset_vertices) == 1) {
      subset_density <- 1
    } else {
      subset_edges <- sum(adj_mat[subset_vertices, subset_vertices]) / 2
      subset_density <- subset_edges / (length(subset_vertices) * (length(subset_vertices) - 1) / 2)
    }
    return(subset_density >= alpha)
  }

  # Backtracking function to find maximal partial clique
  find_maximal_clique <- function(curr_vertex, curr_clique) {

    for (next_vertex in (curr_vertex + 1):n) {
      new_clique <- c(curr_clique, next_vertex)

      if (is_partial_clique(new_clique)) {
        if(length(new_clique) >= maximal_size) {
          maximal_clique <<- new_clique
          maximal_size <<- length(new_clique)
          if(next_vertex < n){
            find_maximal_clique(next_vertex, new_clique)
          }
        }
      }
    }
  }

  # Start the backtracking process
  find_maximal_clique(0, numeric(0))

  edge_density_numerator <- (sum(adj_mat[maximal_clique,maximal_clique]))/2
  edge_density_denominator <- maximal_size*(maximal_size-1)/2
  edge_density <- edge_density_numerator/edge_density_denominator
  return(list(clique_idx = maximal_clique, edge_density = edge_density))
}

.check_matrix_symmetric_23 <- function(adj_mat) {
  is_symmetric <- identical(adj_mat, t(adj_mat))

  return(is_symmetric)
}

.check_matrix_values_23 <- function(adj_mat) {
  all_values_zero_or_one <- all(adj_mat == 0 | adj_mat == 1)
  return(all_values_zero_or_one)
}

.check_matrix_size_23 <- function(adj_mat) {
  num_rows <- nrow(adj_mat)
  num_cols <- ncol(adj_mat)

  is_within_range <- num_rows >= 5 && num_rows <= 50 && num_cols >= 5 && num_cols <= 50

  return(is_within_range)
}

.check_no_row_or_column_names_23 <- function(adj_mat) {
  no_row_names <- is.null(rownames(adj_mat))
  no_col_names <- is.null(colnames(adj_mat))

  return(no_row_names && no_col_names)
}

.check_diagonal_values_23 <- function(adj_mat) {
  # Get the diagonal elements of the matrix
  diagonal_values <- diag(adj_mat)

  # Check if all diagonal values are either 0 or 1
  all_values_zero_or_one <- all(diagonal_values == 0 | diagonal_values == 1)

  return(all_values_zero_or_one)
}

.check_alpha_values_23 <- function(alpha) {
  # Check if alpha is a single numeric value
  is_single_numeric <- is.numeric(alpha) && length(alpha) == 1

  # Check if alpha is within the specified range [0.5, 1]
  is_within_range <- alpha >= 0.5 && alpha <= 1

  return(is_single_numeric && is_within_range)
}

.check_inputs_23 <- function(adj_mat, alpha) {
  if (!.check_matrix_symmetric_23(adj_mat)) {
    stop("Adjacency matrix must be symmetric")
  }

  if (!.check_matrix_values_23(adj_mat)) {
    stop("Adjacency matrix must have all values 0 or 1")
  }

  if (!.check_diagonal_values_23(adj_mat)) {
    stop("Adjacency matrix must have all 1 along the diangonal")
  }

  if (!.check_no_row_or_column_names_23(adj_mat)) {
    stop("Adjacency matrix must have null row and column names")
  }

  if (!.check_matrix_size_23(adj_mat)) {
    stop("Adjacency matrix must have between 5 to 50 rows/columns (inclusive)")
  }

  if(!.check_alpha_values_23(alpha)) {
    stop("Alpha argument must be: 1) a single numeric (i.e., a length of 1), and 2) has a value between 0.5 and 1 (inclusive)")
  }
}

# set.seed(0)
# adj_mat <- generate_partial_clique(5, 0.5, 0.9)$adj_mat
# compute_maximal_partial_clique(adj_mat, 0.9)

