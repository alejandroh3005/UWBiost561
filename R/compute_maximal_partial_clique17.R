#' Compute the maximal partial clique in an adjacency matrix
#'
#' Enumerate over each row to find where the edges are
#' Start with the first row as the first partial clique
#' For each subsequent row, check if it connects with any of the existing partial cliques.
#' If it does, append to the existing partial cliques; if it doesn't, create a new partial clique for this row.
#' Return the partial clique with the most nodes
#'
#' @param adj_mat a symmetric adjacency matrix
#'
#' @param alpha the edge density of the adjacency matrix
#'
#' @return a list containing the indices of the maximal partial clique, the edge density of the maximal partial clique, and all partial cliques in the adjacency matrix
#' @export
compute_maximal_partial_clique17 <- function(adj_mat, alpha){
  stopifnot((1 %in% adj_mat |  0 %in% adj_mat),
            all.equal(t(adj_mat),adj_mat),
            diag(adj_mat)==1,
            is.null(rownames(adj_mat)), is.null(colnames(adj_mat)),
            nrow(adj_mat) >= 5, nrow(adj_mat) <= 50,
            ncol(adj_mat) >= 5, ncol(adj_mat) <= 50,
            length(alpha)==1, alpha >= 0.5, alpha <= 1)
  # compute where the edges are
  edges <- list()
  for (i in 1:nrow(adj_mat)){
    edges[[i]] <- which(adj_mat[i,]==1)
  }
  # first row is its own clique
  # check if second row connect with first row, if not start its own clique
  # check if third row connect with 1 and 2, if it does merge to old cliques
  # repeat for all rows
  partial_cliques <- list()
  partial_cliques[[1]] <- edges[[1]]
  for (i in 2:nrow(adj_mat)){
    for(j in 1:length(partial_cliques)){
      if (1 %in% adj_mat[i,unique(partial_cliques[[j]])]){
        partial_cliques[[j]] <- c(partial_cliques[[j]], edges[[i]])
      }
      else{
        partial_cliques[[length(partial_cliques)+1]] <- edges[[i]]
      }
    }
  }
  partial_clique_sizes <- sapply(partial_cliques,length)
  clique_idx <- unique(partial_cliques[[which(partial_clique_sizes==max(partial_clique_sizes))]])
  # compute the edge density
  edge_count <- (sum(adj_mat[clique_idx, clique_idx]) - length(clique_idx))/2
  full_count <- length(clique_idx)*(length(clique_idx)-1)/2
  edge_density <- edge_count/full_count

  if(edge_density < alpha){
    clique_idx <- 1
    edge_density <- 1
  }

  stopifnot(edge_density>=0, edge_density<=1, edge_density>=alpha,
            max(clique_idx)<=nrow(adj_mat))
  return(list(clique_idx = clique_idx, edge_density = edge_density))
}
