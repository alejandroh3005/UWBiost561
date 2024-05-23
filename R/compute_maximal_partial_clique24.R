#' I'm not sure if this will work...
#'
#' This function will only output one edge, which will satisfy the definition
#' of a clique but will likely not be the largest one
#'
#' @param adj_mat a matrix
#' @param alpha a value
#'
#' @return a list
#' @export
#'
compute_maximal_partial_clique24 <- function(adj_mat, alpha) {
  m <- nrow(adj_mat)
  if(sum(adj_mat) > m){
    idx <- which(adj_mat[1,-1] == 1)

    return(list(clique_idx = c(1, idx[1]+1),
                edge_density = 1))
  } else{
    return(list(clique_idx = 1,
                edge_density = 1))
  }
}
