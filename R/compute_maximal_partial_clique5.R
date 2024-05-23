#' Compute maximal partial clique
#'
#' This function first computes the maximal full clique and records the index numbers,
#' then it tries adding an additional node into the maximal full clique.
#' If there is one node that makes the new clique satisfy the requirement,
#' the function tries adding 2 additional nodes to the previous maximal full clique
#' and repeats the procedure to test different nodes combinations.
#' The procedure goes on until in the case where it's adding y nodes, no combination of y nodes could satisfy the requirement.
#' Then it returns the index numbers of the maximal full clique as well as y-1 nodes selected previously as clique_idx.
#'
#'
#' @param adj_mat An adjacency matrix
#' @param alpha A required edge density
#'
#' @return A numeric vector of index numbers of the the maximum partial clique
#' @export
compute_maximal_partial_clique5 <- function(adj_mat, alpha){
  stopifnot(adj_mat == t(adj_mat), adj_mat == 0 | adj_mat == 1,
            diag(adj_mat) == 1,
            is.null(rownames(adj_mat)) & is.null(colnames(adj_mat)),
            nrow(adj_mat) >=5, nrow(adj_mat) <=50,
            length(alpha) == 1, is.numeric(alpha), alpha >= 0.5, alpha <= 1)
  n <- nrow(adj_mat)
  # Find the largest full clique
  graph <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected")
  largest_full_cliques <- igraph::largest_cliques(graph)

  find_maximal_partial_clique <- function(full_clique){
    idx_vec <- full_clique
    left_idx_vec <- (1:n)[-idx_vec]
    my_idx <- idx_vec
    for (i in 1:length(left_idx_vec)) {
      signal <- 0
      combination <- utils::combn(left_idx_vec, i)
      for (j in 1 : ncol(combination)) {
        new_idx <- combination[,j]
        idx <- append(idx_vec, new_idx)
        length_idx <- length(idx)
        sum_partial_clique <- sum(adj_mat[idx,idx])
        if ((sum_partial_clique-length_idx)/2 >= alpha*length_idx*(length_idx-1)/2) {
          my_idx <- idx
          signal <- 1
          break
        }
      }
      if (signal == 0){
        break
      }
    }
    return(my_idx)
  }
  clique_idx <- find_maximal_partial_clique(as.numeric(largest_full_cliques[[1]]))
  m <- length(clique_idx)
  if (m >1) {
    edge_density_numerator <- (sum(adj_mat[clique_idx, clique_idx]) - m)/2
    edge_density_denominator <- m*(m-1)/2
    edge_density <- edge_density_numerator/edge_density_denominator
  } else {
    edge_density <- 0
  }
  return(list(clique_idx = clique_idx, edge_density = edge_density))
}
