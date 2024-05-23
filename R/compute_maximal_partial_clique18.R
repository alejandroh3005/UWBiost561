#' Generate combinations while fixing the first k elements
#'
#' @param vec vector to choose from
#' @param c number of elements to choose
#' @param k number of elements to fix
#'
#' @return A matrix of combinations
#'
.fixed_combn_18 = function(vec, c, k){
  fixed = vec[1:k]
  remainder = vec[-(1:k)]
  n = length(remainder)
  cols = choose(n, c-k)

  basis = fixed %*% t(rep(1, cols))
  return(rbind(basis, utils::combn(remainder, c-k)))
}


#' A function to compute the upper bound of the clique size
#'
#' Given an adjacency matrix, this function uses the sum of the adjacency matrix to upper bound the possible clique size
#'
#' @param adj_mat A symmetric and diagonal adjacency matrix
#' @param alpha The threshold for the maximum number of edges in the partial clique
#'
#' @return The upper bound of the clique size
#'
.calculate_maximum_candidate_18 = function(adj_mat, alpha){
  C = (sum(adj_mat) - nrow(adj_mat))*(alpha)^(-1)

  return((1 + sqrt(1 + 4*C))/2)
}


#' A function that further restricts the candidate nodes for a partial clique
#'
#' Given a set of candidate nodes, this function further restricts the candidate nodes for a partial clique
#'
#' @param adj_mat A symmetric and diagonal adjacency matrix
#' @param candidates A set of candidate nodes
#' @param c The size of the partial clique
#' @param at_least_total The minimum number of edges in the partial clique
#'
#' @return A minimum number of edges per node
#'
.minimum_searcher_18 = function(adj_mat, candidates, c, at_least_total){
  colsums = colSums(adj_mat)[candidates]
  ordered_colsums = colsums[order(colsums, decreasing = T)]
  unique_colsums = sort(unique(ordered_colsums))
  unique_n = length(unique_colsums)

  for(i in 1:unique_n){
    if(i > length(unique_colsums)){
      break
    }
    # Loop Initialization: This loop iterates over the indices from 1 to the length of `unique_colsums`.

    first_i = unique_colsums[1:i]
    # Subset Creation: For each iteration of the loop, we create a subset of `unique_colsums` containing the first `i` elements.
    # This subset represents the cumulative sum of the first `i` unique column sums.

    remove_if_pos = at_least_total - sum(ordered_colsums[1:max(c-i, 1)]) - sum(first_i)
    # Calculation of Removal Condition:
    # We calculate the difference between the `at_least_total` (the minimum total sum required) and the sum of the first `c - i` elements of `ordered_colsums` the sum of `first_i`.
    # If this difference is positive, it means we need to remove elements from `unique_colsums` to meet the minimum total requirement.

    if(remove_if_pos > 0){
      unique_colsums = unique_colsums[-1]
    }else{
      next
    }
  }

  return(unique_colsums[1])
}


#' Prunes columns that can be a part of a partial clique of size c
#'
#' @param adj_mat A symmetric and diagonal adjacency matrix
#' @param alpha The threshold for the maximum number of edges in the partial clique
#' @param c The size of the partial clique
#'
#' @return Candidate rows that may be a part of the partial clique
#'
#'
#'
.pruned_cols_18 = function(adj_mat, alpha, c){
  at_least_per_col = c - floor(c^2 * (1-alpha))/2
  at_least_total = ceiling(c^2 * alpha)
  colsums = colSums(adj_mat)
  ordered_colsums = sort(colsums, decreasing = T)

  candidates = which(colsums >= at_least_per_col)
  if ((length(candidates) < c) | (sum(ordered_colsums[1:c]) < at_least_total)){
    return(NULL)
  }else{
    min_edges = .minimum_searcher_18(adj_mat, candidates, c, at_least_total)
    return(candidates[colsums[candidates] >= min_edges])
  }
}


#' A function to compute the size of the maximal partial clique of an adjacency matrix
#'
#' Computes the maximum size of a partial clique in an adjacency matrix
#'
#' @param adj_mat A symmetric and diagonal adjacency matrix
#' @param alpha The threshold for the maximum number of edges in the partial clique
#'
#' @return An index vector showing the first maximal partial clique that exceeds the threshold, where the "first" is decided by the order of possible cliques determined by the \code{utils::combn} function. Also returns the edge density of that particular partial clique
#' @export
compute_maximal_partial_clique18 = function(adj_mat, alpha){
  #Your function should check that the adj_mat argument is:
  # 1) a symmetric matrix with only values 0 or 1,
  # 2) has 1’s along its diagonal,
  # 3) has no row- or column-names, and
  # 4) will have between 5 to 50 rows/columns (inclusive).
  stopifnot((is.matrix(adj_mat)),
            (dim(adj_mat) >= 5 & dim(adj_mat) <= 50),
            (diag(adj_mat) == 1),
            (adj_mat == t(adj_mat)),
            (is.null(rownames(adj_mat))),
            (is.null(colnames(adj_mat))))
  # Your function should check that the alpha argument is:
  # 1) a single numeric (i.e., a length of 1), and
  # 2) has a value between 0.5 and 1 (inclusive).
  stopifnot(is.numeric(alpha),
            length(alpha) == 1,
            alpha >= 0.5,
            alpha <= 1)

  # First get number of nodes
  n = nrow(adj_mat)

  # No search needed if we have an identity matrix, or a full 1 matrix
  if(all(adj_mat == diag(rep(1, n)))){return(list(clique_idx = 1, edge_density = 1))}

  if(all(adj_mat == 1)){return(list(clique_idx = 1:n, edge_density = 1))}

  # Order the adjacency matrix by the sum of columns
  ordered_mat = adj_mat[order(colSums(adj_mat), decreasing = T), order(colSums(adj_mat), decreasing = T)]

  # 1. Compute maximum clique size that is possible given the adjacency matrix:
  # upper_bound = .calculate_maximum_candidate_18(adj_mat, alpha)

  # 2. Based on that maximum candidate, do a reverse search from that maximum down to 2.
  # Given a clique size c, ONLY consider the columns that may be a part of a clique of that size
  # i.e., if the colSum is too small, that column may not be a part of that clique
  # candidates = .pruned_cols_18(adj_mat, alpha, c): to extract all "candidate" columns

  # 3. Extract ALL(ideally) or a SUBSET(practically) of possible combinations of those columns
  # .fixed_combn_18(candidates, c, c-k): Fix first c-k columns in terms of colSum and vary the remaining k
  # TODO: This part limits the number of computations to reduce the time complexity
  # Practically, most of the time, the actual clique was formed amongst the top colSums columns
  # So, I think it should still work for most cases.


  # 4. Once we find a clique that exceeds the threshold(alpha), we break the loop


  # Sum of adjacency matrix - n >= clique_size^2 - clique_size
  upper_bound = floor(.calculate_maximum_candidate_18(adj_mat, alpha))
  # cat("Upper Bound: ", upper_bound, "\n")


  for (c in upper_bound:2){
    # You can be missing at most: c^2 * (1-alpha)
    # Only look at columns that have at least c^2 * (1-alpha)/2 colsums
    candidates = .pruned_cols_18(adj_mat, alpha, c)


    # If there are no candidates, move on to the next c
    if(is.null(candidates)){
      # cat("No candidates for c = ", c, "\n")
      next
    }

    # cat("Candidates: ", candidates, "\n")

    # Generate all possible combinations of the candidates
    # Order the candidates by colSums. Usually, I observed most cliques are at the top of the colSums
    comb = .fixed_combn_18(candidates[order(colSums(adj_mat[candidates, candidates]), decreasing = T)], c, c-2)
    # comb = utils::combn(candidates[order(colSums(adj_mat[candidates, candidates]), decreasing = T)], c)


    for (ij in 1:ncol(comb)){
      # Loop through all possible combinations
      break_loop = F

      block = adj_mat[comb[,ij], comb[,ij]]
      density = sum(block)/(c^2)
      # cat(c, density, "\n")
      # Move on to the next c as soon as density > alpha
      if(density > alpha){
        # cat("Threshold Met: Search Complete\n")
        break_loop = T
        max_clique_size = c
        clique_idx = comb[,ij]
        break
      }
    }
    if (break_loop){
      break
    }
  }
  edge_density_numerator = (sum(block) - max_clique_size) / 2
  edge_density_denominator = max_clique_size * (max_clique_size - 1) / 2
  edge_density = edge_density_numerator / edge_density_denominator

  return(list(clique_idx = clique_idx,
              edge_density = edge_density))
}
