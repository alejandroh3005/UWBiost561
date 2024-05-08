<<<<<<< HEAD
<<<<<<< HEAD
#' Find the largest partial clique within a given adjacency matrix
=======
#' Find the largest partial clique within a given an adjacency matrix
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
=======
#' Find the largest partial clique within a given an adjacency matrix
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
#'
#' @param adj_mat an adjacency matrix where within the largest partial clique will be searched
#' @param alpha the minimum required edge density of the partial clique
#'
#' @return a list containing indices of the nodes within the clique and the actual edge density of the return partial clique
#' @export
compute_maximal_partial_clique <- function (adj_mat,
                                            alpha) {
  # check arguments
  stopifnot(
  ## check adjacency matrix is valid
    # no row or column names)
    is.null(colnames(adj_mat)), is.null(rownames(adj_mat)),
    # has between 5 and 50 (inclusive) rows/columns
    5 <= min(dim(adj_mat)), max(dim(adj_mat)) <= 50,
    # is numeric
    all(is.numeric(adj_mat)),
    # elements are 0 or 1
    all(adj_mat == 0 | adj_mat == 1),
    # 1's along the diagonal
    all(diag(adj_mat) == 1),
    # symmetric matrix (equal to its transpose)
    all.equal(adj_mat, t(adj_mat)),
  ## check alpha is valid
    # check alpha is a single numeric value
    length(alpha) == 1, is.numeric(alpha),
    # check alpha is within [0.5, 1]
    0.5 <= alpha, alpha <= 1
  )

<<<<<<< HEAD
<<<<<<< HEAD
  # identify the partial clique according to minimum edge density alpha

  n <- nrow(adj_matrix) # total number of nodes in the matrix
  # iterate over all possible clique sizes, from the largest possible to 1
  for (size in seq(n, 1, by = -1)) {
    # organize all possible combinations of nodes of variable size
    # each of these will be assessed as a candidate partial clique
    combinations <- combn(nrow(adj_matrix), size)
    # iterate through all sub-matrices of a given size
    for (i in 1:ncol(combinations)) {
      subset <- combinations[,i] # nodes in current sub-matrix
      submatrix <- adj_matrix[subset, subset] # adjacency matrix of sub-matrix
      # if this sub-matrix satisfies our partial clique requirements,
      # because we are working from largest to smallest sub-matrix,
      # we are done!
      if (is_partial_clique(submatrix, alpha)) {
        # compute the actual edge density of our partial clique
=======
=======
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
  # indices of nodes within the maximum partial clique
  subset = NULL
  # percentage of edges within the maximum partial clique
  edge_density = NULL

  n <- nrow(adj_matrix)
  # iterate over all possible clique sizes, from the largest
  for (size in seq(n, 1, by = -1)) {
    combinations <- combn(nrow(adj_matrix), size)
    # iterate over all possible cliques of given size
    for (i in 1:ncol(combinations)) {
      subset <- combinations[,i]
      submatrix <- adj_matrix[subset, subset]
      # if partial clique found, exit and return
      if (is_partial_clique(submatrix, alpha)) {
<<<<<<< HEAD
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
=======
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
        m <- nrow(submatrix)
        max_edges <- m*(m-1)/2
        actual_edges <- (sum(submatrix) - m) / 2
        edge_density <- round(actual_edges / max_edges, 2)
<<<<<<< HEAD
<<<<<<< HEAD
        # mark the indices of nodes within the partial clique subset
        clique_idx <- rep(0, n)
        clique_idx[subset] <- 1
        return(list(clique_idx=clique_idx,
                    edge_density=edge_density,
                    subset=subset,
                    partial_clique=submatrix))
        # a partial clique will always be achieved, even if it is a single node
      }
    }
  }
=======
=======
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
        return(list(clique_idx=subset,
                    edge_density=edge_density,
                    partial_clique=submatrix))
      }
    }
  }

  # # check returns are valid
  # stopifnot(
  #   # edge density is a single numeric value greater than or equal to alpha
  #   # and within 0 and 1
  #   length(edge_density) == 1, is.numeric(edge_density), edge_density >= alpha,
  #   # clique indices is a vector of zeros and ones and not larger than the
  #   # size of the matrix
  #   is.numeric(clique_idx), all(clique_idx == 0 | clique_idx == 1),
  #   length(clique_idx) <=  nrow(adj_mat))
  #
  # return(list(clique_idx = clique_idx,
  #             edge_density = edge_density))

<<<<<<< HEAD
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
=======
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
}



#' Helper function to check if a set of nodes forms a partial clique
#'
<<<<<<< HEAD
<<<<<<< HEAD
#' @param submatrix a sub-matrix formed by a set of nodes
#' @param alpha the minimum required edge density of the partial clique
#'
#' @return TRUE or FALSE depending on whether the given sub-matrix forms a partial clique
=======
=======
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
#' @param submatrix a submatrix formed by a set of nodes
#' @param alpha the minimum required edge density of the partial clique
#'
#' @return TRUE or FALSE depending on whether the given submatrix forms a partial clique
<<<<<<< HEAD
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
=======
>>>>>>> 5fcb456f678efdbb507bc1d0586e00ed731c583f
#' @export
is_partial_clique <- function(submatrix, alpha = 1) {
  m <- nrow(submatrix)
  max_edges <- m*(m-1)/2
  min_edges <- alpha * max_edges
  actual_edges <- (sum(submatrix)-m) / 2
  if (actual_edges >= min_edges) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
