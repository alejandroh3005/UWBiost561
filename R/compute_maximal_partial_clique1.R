#' Compute maximal partial clique
#'
#' Determines the indices corresponding to the maximal partial clique of an adjacency matrix for a given partial clique density.
#'
#' This function uses modified versions of the Bron-Kerbosch algorithm, which is designed to compute maximal full cliques, to compute maximal partial cliques, allowing users to specify `"simple"` or "`pivot`" (Bron & Kerbosch 1973).
#' Specifically, if `alpha` < 1, the algorithm is modified so that the candidate and excluded nodes at each iteration are not restricted to being neighbors. Rather, a candidate clique is only initially reported as a maximal clique if it satisfies a density of `alpha/2` (to help find cliques for which two of the nodes alone don't satisfy alpha density, but together the clique does), and then it searches through the final results to choose the largest clique which satisfies `alpha`.
#' If `alpha = 1` is set, then the Bron-Kerbosch algorithm is applied directly.
#'
#' Using
#' @param adj_mat A symmetric adjacency matrix with 5-50 rows and columns.
#' @param alpha A numeric value between 0.5 and 1 (inclusive) indicating the minimum fraction of edges among nodes in the outputted partial clique.
#' @param method One of `"simple"` or "`pivot`". `"simple"` searches for the maximal partial clique using a modified version of the original Bron-Kerbosch algorithm, and `"pivot"` uses a modified version of the Bron-Kerbosch algorith with pivot (Bron & Kerbosch 1973).
#' @export
#' @references Bron, C., & Kerbosch, J. (1973). Algorithm 457: finding all cliques of an undirected graph. Communications of the ACM, 16(9), 575-577.

compute_maximal_partial_clique1 <- function(adj_mat, alpha,
                                            method = c("simple", "pivot")) {
  ## Check adj_mat
  stopifnot(is.matrix(adj_mat), nrow(adj_mat) == ncol(adj_mat),
            nrow(adj_mat) >= 5, nrow(adj_mat) <= 50,
            all(as.vector(adj_mat %in% c(0,1))),
            isSymmetric(adj_mat), all(diag(adj_mat) == 1),
            is.null(names(adj_mat)))

  ## Check alpha
  stopifnot(is.numeric(alpha), length(alpha) == 1,
            0.5 <= alpha, 1 >= alpha)

  ## Method
  method <- match.arg(method, c("simple", "pivot"))

  ## Find maximum clique size using Bron-Korsch
  # Initialize sets R, P, X
  R <- numeric(0)  # Current clique being constructed
  P <- 1:nrow(adj_mat)  # Set of candidate vertices to be added to R
  X <- numeric(0)  # Set of vertices that will no longer be considered

  # Find maximal cliques with Bron-Kerbosch
  if(method == "pivot" & alpha < 1){
    maximal_cliques <- find_cliques_1(R, P, X, adj_mat, alpha = alpha)
  } else if(method == "simple" & alpha < 1){
    maximal_cliques <- find_cliques2_1(R, P, X, adj_mat, alpha = alpha)
  } else if(method == "pivot" & alpha == 1){
    maximal_cliques <- find_cliques3_1(R, P, X, adj_mat)
  } else{
    maximal_cliques <- find_cliques4_1(R, P, X, adj_mat)
  }

  # Extract the largest clique if alpha = 1
  if(alpha == 1){
    clique_idx <- maximal_cliques[[which.max(lengths(maximal_cliques))]]
  } else{
    # Extract the largest clique which satisfies the criterion
    #  (sum(adj_mat[R_new, R_new])-m)/2 >= alpha*m*(m-1)/2
    # First sort maximal_cliques by length (descending). Then go one by one
    # down the list checking the condition until it is met
    sorted_indices <- order(sapply(maximal_cliques, length), decreasing = TRUE)
    maximal_cliques <- maximal_cliques[sorted_indices]
    clique_idx <- c()
    for(i in 1:length(maximal_cliques)){
      clique_candidate <- maximal_cliques[[i]]
      m <- length(clique_candidate)
      num_edges <- (sum(adj_mat[clique_candidate, clique_candidate])-m)/2
      density_threshold <- alpha*m*(m-1)/2
      if(num_edges >= density_threshold){
        clique_idx <- clique_candidate
        break
      }
    }
  }

  # Calculate edge_density
  m <- length(clique_idx)
  if(m == 1){
    edge_density <- 1
  } else{
    edge_density_numerator <- (sum(adj_mat[clique_idx,clique_idx]) - m)/2
    edge_density_denominator <- m*(m-1)/2
    edge_density <- edge_density_numerator/edge_density_denominator
  }

  return(list(clique_idx = clique_idx, edge_density = edge_density))
}

# Find maximum cliques with pivot
find_cliques_1 <- function(R, P, X, adj_mat, maximal_cliques = list(), alpha) {
  if(length(P) >= 0 & length(X) == 0){
    maximal_cliques <- c(maximal_cliques , list(R))
  }
  # Choose a pivot

  colsums_adj_mat <- colSums(adj_mat)
  colsums_adj_mat[X] <- -1
  pivot <- which.max(colsums_adj_mat)
  # pivot <- sample(union(P, X))
  pivot_neighbors <- which(adj_mat[,pivot] == 1)

  # Iterate over vertices in P adjacent to the pivot
  for (vertex in P){
    #vertex_neighbors <- which(adj_mat[,vertex] == 1)
    #vertex_neighbors <- setdiff(vertex_neighbors, vertex)
    R_new <- union(R, vertex)
    P_new <- setdiff(P, vertex)
    X_new <- intersect(X, vertex)

    # Check the density of the potential maximal clique
    subgraph <- adj_mat[R_new, R_new]
    m <- length(R_new)
    num_edges <- (sum(adj_mat[R_new, R_new])-m)/2
    density_threshold <- alpha*m*(m-1)/2

    # Check if the density meets the alpha condition
    if (num_edges >= density_threshold | m <= 4) {
      maximal_cliques <- find_cliques_1(R_new, P_new, X_new, adj_mat,
                                      maximal_cliques = maximal_cliques,
                                      alpha = alpha)
    }

    ##
    P <- setdiff(P, vertex)
    X <- union(X, vertex)
  }
  return(maximal_cliques)
}

find_cliques2_1 <- function(R, P, X, adj_mat, maximal_cliques = list(), alpha) {
  if(length(P) >= 0 & length(X) == 0){
    maximal_cliques <- c(maximal_cliques, list(R))
  }
  # Iterate over vertices in P
  for (vertex in P){
    #vertex_neighbors <- which(adj_mat[,vertex] == 1)
    #vertex_neighbors <- setdiff(vertex_neighbors, vertex)
    R_new <- union(R, vertex)
    P_new <- setdiff(P, vertex)
    X_new <- intersect(X, vertex)

    # Check the density of the potential maximal clique
    subgraph <- adj_mat[R_new, R_new]
    m <- length(R_new)
    num_edges <- (sum(adj_mat[R_new, R_new])-m)/2
    density_threshold <- alpha*m*(m-1)/2

    # Check if the density meets the alpha condition
    if (num_edges >= density_threshold | m <= 4) {
      maximal_cliques <- find_cliques2_1(R_new, P_new, X_new, adj_mat,
                                       maximal_cliques = maximal_cliques, alpha = alpha)
    }


    ##
    P <- setdiff(P, vertex)
    X <- union(X, vertex)
  }
  return(maximal_cliques)
}

# Find maximum full cliques with pivot
find_cliques3_1 <- function(R, P, X, adj_mat, maximal_cliques = list()) {
  if(length(P) == 0 & length(X) == 0){
    maximal_cliques <- c(maximal_cliques , list(R))
  }
  # Choose a pivot

  colsums_adj_mat <- colSums(adj_mat)
  colsums_adj_mat[X] <- -1
  # pivot <- sample(union(P, X))
  pivot <- which.max(colsums_adj_mat)
  pivot_neighbors <- which(adj_mat[,pivot] == 1)

  # Iterate over vertices in P adjacent to the pivot
  for (vertex in setdiff(P, pivot_neighbors)){
    vertex_neighbors <- which(adj_mat[,vertex] == 1)
    vertex_neighbors <- setdiff(vertex_neighbors, vertex)
    R_new <- union(R, vertex)
    P_new <- intersect(P, vertex_neighbors)
    X_new <- intersect(X, vertex_neighbors)
    maximal_cliques <- find_cliques3_1(R_new, P_new, X_new, adj_mat,
                                     maximal_cliques = maximal_cliques)
    P <- setdiff(P, vertex)
    X <- union(X, vertex)
  }
  return(maximal_cliques)
}

# Find maximum full cliques without pivot
find_cliques4_1 <- function(R, P, X, adj_mat, maximal_cliques = list()) {
  if(length(P) == 0 & length(X) == 0){
    maximal_cliques <- c(maximal_cliques , list(R))
  }else{

    # Iterate over vertices in P adjacent to the pivot
    for (vertex in P){
      vertex_neighbors <- which(adj_mat[,vertex] == 1)
      vertex_neighbors <- setdiff(vertex_neighbors, vertex)
      R_new <- union(R, vertex)
      P_new <- intersect(P, vertex_neighbors)
      X_new <- intersect(X, vertex_neighbors)
      maximal_cliques <- find_cliques4_1(R_new, P_new, X_new, adj_mat,
                                       maximal_cliques = maximal_cliques)
      P <- setdiff(P, vertex)
      X <- union(X, vertex)
    }
  }
  return(maximal_cliques)
}
