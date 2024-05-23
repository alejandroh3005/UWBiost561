#' Generate a adjacency matrix representing a matrix containing a partial clique
#'
#' Function that constructs an adjacency matrix for a variable-sized matrix that
#' contains a partial clique of variable-density#'
#' @param n the number of nodes in the network
#' @param clique_fraction the fraction of nodes that are in the partial clique
#' @param clique_edge_density the density of edges present between nodes in the clique
#'
#' @return an adjacency matrix containing a partial clique
#' @export
generate_partial_clique <- function (n,
                                     clique_fraction = 0.5,
                                     clique_edge_density = 0.5) {
  stopifnot(
    # check arguments are numeric
    is.numeric(c(n, clique_fraction, clique_edge_density)),
    # check n is positive integer
    n %% 1 == 0, n > 0,
    # check clique fraction is within [0,1]
    0 <= clique_fraction, clique_fraction <= 1,
    # check clique edge density is within [0,1]
    0 <= clique_edge_density, clique_edge_density <= 1)

  # Generate a matrix with a fully-connected clique

  ## The following code was authored by Kevin Lin (linnykos on GitHub) and accessed from
  ## "https://raw.githubusercontent.com/linnykos/561_s2024_public/main/HW2_files/random_graph_functions.R"
  # Generate an unsymmetric matrix
  density_low = 0.1

  adj_mat <- matrix(sample(x = c(0,1),
                           size = n^2,
                           prob = c(1 - density_low, density_low),
                           replace = TRUE),
                    nrow = n, ncol = n)
  # Symmetrize the matrix
  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat > 0] <- 1
  diag(adj_mat) <- 1
  # Form the (complete) clique
  clique_size <- ceiling(n * clique_fraction)
  adj_mat[1:clique_size, 1:clique_size] <- 1
  ##
  ## Pause in of code from Kevin Lin

  ## The following code chunk was written by Alejandro Hernandez
  ##
  # Remove edges from clique to make it partially complete
  m <- clique_size
  to_remove <- ceiling(m*(m-1)/2 * (1 - clique_edge_density))
  # iteratively remove edges until connection threshold is met
  n_removed <- 0
  while (n_removed < to_remove) {
    # randomly select a pair of nodes within the clique
    random_pair <- sample(1:m, 2)
    # if they share an edge remove it
    if (adj_mat[random_pair[1], random_pair[2]] == 1) {
      adj_mat[random_pair[1], random_pair[2]] <- 0
      adj_mat[random_pair[2], random_pair[1]] <- 0
      # iterate
      n_removed <- n_removed + 1
    }
  }
  ##
  ## End of code from Alejandro Hernandez

  ## Continuation of code from Kevin Lin
  ##
  # Randomize the order of the nodes
  sample_idx <- sample(1:n)
  adj_mat <- adj_mat[sample_idx, sample_idx]
  # Compute the appropriate reverse order
  rev_order <- sapply(1:n, function(i){
    which(sample_idx == i)
  })
  ##
  ## End of code from Kevin Lin

  ## check final adjacency matrix is valid
  stopifnot(
    # no row or column names)
    is.null(colnames(adj_mat)), is.null(rownames(adj_mat)),
    # is numeric
    all(is.numeric(adj_mat)),
    # elements are 0 or 1
    all(adj_mat == 0 | adj_mat == 1),
    # 1's along the diagonal
    all(diag(adj_mat) == 1),
    # symmetric matrix (equal to its transpose)
    all.equal(adj_mat, t(adj_mat)))

  # list of outputs
  return(list(adj_mat = adj_mat))
}
