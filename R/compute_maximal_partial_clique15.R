#' Find hte matximal partial clique
#'
#' Start from the first entry, and iteratively add more nodes.
#' The first moment you do not find a valid click, stop
#'
#' @param adj_mat the graph
#' @param alpha a minimum threshold on when to stop
#'
#' @return list
#' @export
compute_maximal_partial_clique15 <- function(adj_mat, alpha) {
  stopifnot(is.matrix(adj_mat), is.numeric(alpha))

  n <- nrow(adj_mat)
  clique_idx <- numeric(0)
  i <- 1

  .compute_density_adj <- function(idx){
    m <- length(idx)
    if(length(m) == 1) return(1)
    return(((sum(adj_mat[idx,idx])-m)/2)/(m*(m-1)/2))
  }

  while(TRUE){
    clique_idx <- c(clique_idx, i)
    edge_density <- .compute_density_adj(idx = clique_idx)
    if(edge_density <= alpha) break()
  }

  list(clique_idx = clique_idx,
       edge_density = edge_density)
}
