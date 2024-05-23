#' Compute Maximal Partial Clique
#'
#'  Given an adjacency matrix adj_mat and a required edge density alpha, the program will
#'  determine if the matrix is diagonal, (meaning the nodes have no connection.)
#'  If so, it will return 1 node with clique density of 1.
#'  If the matrix is not diagonal, there is at least one edge.
#'  The program will list the first edge it finds as the largest partial clique.
#'
#' @param adj_mat adjacency matrix to be tested when loaded into the function
#' @param alpha the density level of partial clique requested
#'
#' @return a list with the clique index of the largest partial clique in the adjacency matrix, and its edge density
#' @export
compute_maximal_partial_clique8 <- function(adj_mat, alpha){
  stopifnot(isSymmetric(adj_mat) == TRUE,
            all(adj_mat == 0 | adj_mat == 1 | adj_mat == 2 ),
            diag(adj_mat == 1),
            length(rownames(adj_mat)) == 0,
            length(colnames(adj_mat)) == 0,
            (nrow(adj_mat) >= 5 & nrow(adj_mat) <= 50),
            (ncol(adj_mat) >= 5 & ncol(adj_mat) <= 50),
            length(alpha) ==1,
            alpha >= 0.5 & alpha <= 1)
  if(is.diagonal.matrix_8(adj_mat) == TRUE){
    return(list(clique_idx = 1,
                edge_density = 1))
  }
  else{
    diag(adj_mat) = 0
    clique_idx = which(adj_mat == 1, arr.ind = TRUE)
    clique_idx = as.numeric(clique_idx[1, ])
    return(list(clique_idx = clique_idx,
                edge_density = 1))
  }
}

is.diagonal.matrix_8 <- function( x, tol=1e-8 )
{
  if (!is.matrix(x) || nrow(x) !=  ncol(x))
    stop( "argument x is not a square matrix" )
  if ( !is.numeric( x ) )
    stop( "argument x is not a numeric matrix" )
  y <- x
  diag( y ) <- rep( 0, nrow( y ) )
  return( all( abs( y ) < tol ) )
}
