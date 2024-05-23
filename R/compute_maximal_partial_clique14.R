#' Compute the Maximal Partial Clique
#'
#' @param adj_mat The adjacency matrix representing the graph.
#' @param alpha The minimum required edge density for the partial clique, between 0.5 and 1.
#'
#' @return A list containing `clique_idx` (numeric vector of indices) and `edge_density` (numeric proportion of edges in the partial clique).
#' @export
compute_maximal_partial_clique14 <- function(adj_mat, alpha) {
  # Input validation
  stopifnot(
    is.matrix(adj_mat),
    all(adj_mat == t(adj_mat)),  # Check symmetry
    all(adj_mat %in% c(0, 1)),  # Check for binary values
    all(diag(adj_mat) == 1),  # Ensure diagonal is all 1s (self-loops)
    is.null(rownames(adj_mat)),
    is.null(colnames(adj_mat)),
    nrow(adj_mat) >= 5 && nrow(adj_mat) <= 50,  # Check dimensions
    is.numeric(alpha), length(alpha) == 1, alpha >= 0.5 && alpha <= 1
  )

  # Helper function to calculate density of a given set of nodes
  calculate_density <- function(nodes) {
    m <- length(nodes)
    if (m <= 1) return(1)  # Single-node case returns density of 1

    subgraph <- adj_mat[nodes, nodes]
    num_edges <- (sum(subgraph) - m) / 2  # Exclude self-loops
    max_edges <- m * (m - 1) / 2

    return(num_edges / max_edges)
  }

  # Initialize BFS queue and variables
  best_clique_idx <- integer()
  best_density <- 0
  clique_queue <- list()
  clique_queue[[1]] <- integer()  # Start with an empty clique

  while (length(clique_queue) > 0) {
    current_clique_idx <- clique_queue[[1]]
    clique_queue <- clique_queue[-1]

    remaining_nodes <- setdiff(seq_len(nrow(adj_mat)), current_clique_idx)

    for (node in remaining_nodes) {
      candidate_clique_idx <- c(current_clique_idx, node)
      density <- calculate_density(candidate_clique_idx)

      # Update best clique if density is above alpha
      if (density >= alpha) {
        if (density > best_density || (density == best_density && length(candidate_clique_idx) > length(best_clique_idx))) {
          best_clique_idx <- candidate_clique_idx
          best_density <- density
        }
        # Add the new candidate to the queue only if it maintains or improves density
        clique_queue <- c(clique_queue, list(candidate_clique_idx))
      }
    }
  }

  # Calculate the final density of the resulting maximal partial clique
  edge_density <- calculate_density(best_clique_idx)

  # Return the final clique index and its edge density
  output <- list(
    clique_idx = best_clique_idx,
    edge_density = edge_density
  )

  return(output)
}
