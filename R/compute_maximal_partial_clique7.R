#' Find the largest partial clique within a given adjacency matrix
#' @param adj_mat an adjacency matrix where within the largest partial clique will be searched
#' @param alpha the minimum required edge density of the partial clique
#'
#' @return a list containing indices of the nodes within the clique and the actual edge density of the return partial clique
#' @export
compute_maximal_partial_clique7 <- function (adj_mat,
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

  # identify the partial clique according to minimum edge density alpha

  n <- nrow(adj_mat) # total number of nodes in the matrix
  # iterate over all possible clique sizes, from the largest possible to 1
  for (size in seq(n, 1, by = -1)) {
    # organize all possible combinations of nodes of variable size
    # each of these will be assessed as a candidate partial clique
    combinations <- utils::combn(nrow(adj_mat), size)
    # iterate through all sub-matrices of a given size
    for (i in 1:ncol(combinations)) {
      subset <- combinations[,i] # nodes in current sub-matrix
      submatrix <- adj_mat[subset, subset] # adjacency matrix of sub-matrix
      # if this sub-matrix satisfies our partial clique requirements,
      # because we are working from largest to smallest sub-matrix,
      # we are done!
      if (is_partial_clique_7(submatrix, alpha)) {
        # compute the actual edge density of our partial clique
          # indices of nodes within the maximum partial clique
          subset = NULL
          # percentage of edges within the maximum partial clique
          edge_density = NULL

          n <- nrow(adj_mat)
          # iterate over all possible clique sizes, from the largest
          for (size in seq(n, 1, by = -1)) {
            combinations <- utils::combn(nrow(adj_mat), size)
            # iterate over all possible cliques of given size
            for (i in 1:ncol(combinations)) {
              subset <- combinations[,i]
              submatrix <- adj_mat[subset, subset]
              # if partial clique found, exit and return
              if (is_partial_clique_7(submatrix, alpha)) {
                m <- nrow(submatrix)
                max_edges <- m*(m-1)/2
                actual_edges <- (sum(submatrix) - m) / 2
                edge_density <- round(actual_edges / max_edges, 2)
                # mark the indices of nodes within the partial clique subset
                clique_idx <- rep(0, n)
                clique_idx[subset] <- 1
                return(list(clique_idx=which(clique_idx == 1),
                            edge_density=edge_density,
                            subset=subset,
                            partial_clique=submatrix))
                # a partial clique will always be achieved, even if it is a single node
              }
            }
          }
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
}



#' Helper function to check if a set of nodes forms a partial clique
#'
#' @param submatrix a sub-matrix formed by a set of nodes
#' @param alpha the minimum required edge density of the partial clique
#'
#' @return TRUE or FALSE depending on whether the given sub-matrix forms a partial clique
#' @param submatrix a submatrix formed by a set of nodes
#' @param alpha the minimum required edge density of the partial clique
#'
#' @return TRUE or FALSE depending on whether the given submatrix forms a partial clique
is_partial_clique_7 <- function(submatrix, alpha = 1) {
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
