context("Testing compute_maximal_partial_clique")


test_that("Implementation returns a list and adjacency matrix of the correct size", {
  set.seed(0)

  n <- 10
  alpha <- 0.5
  rand_mat <- UWBiost561::generate_partial_clique(n = n,
                                                  clique_fraction = 1,
                                                  clique_edge_density = alpha)
  adj_mat <- rand_mat$adj_mat
  res <- compute_maximal_partial_clique(adj_mat = adj_mat,
                                         alpha = alpha)
  res$adj_mat

  expect_true(is.list(res))
  expect_true(all(dim(res$adj_mat) == c(n,n)))
})

test_that("Implementation returns a valid adjacency matrix", {
  set.seed(0)

  n <- 10
  alpha <- 0.5
  rand_mat <- UWBiost561::generate_partial_clique(n = n,
                                                  clique_fraction = 1,
                                                  clique_edge_density = alpha)
  rand_adj_mat <- rand_mat$adj_mat
  res <- compute_maximal_partial_clique(adj_mat = rand_adj_mat,
                                         alpha = alpha)
  adj_mat <- rand_adj_mat[res$clique_idx,res$clique_idx]

  # is numeric
  expect_true(all(is.numeric(adj_mat)))
  # elements are 0 or 1
  expect_true(all(adj_mat == 0 | adj_mat == 1))
  # 1's along the diagonal
  expect_true(all(diag(adj_mat) == 1))
  # symmetric matrix (equal to its transpose)
  expect_true(all.equal(adj_mat, t(adj_mat)))
})

test_that("Implementation finds the correct answer at n=10, a=1, cf=1", {
  set.seed(0)

  n <- 10
  alpha <- 1
  cf <- 1
  rand_mat <- UWBiost561::generate_partial_clique(n = n,
                                                  clique_fraction = cf,
                                                  clique_edge_density = alpha)
  adj_mat <- rand_mat$adj_mat
  res <- compute_maximal_partial_clique(adj_mat = adj_mat,
                                         alpha = alpha)

  expect_true(all(res$subset == c(1,2,3,4,5,6,7,8,9,10)))
})

test_that("Implementation finds the correct answer at n=10, a=0.5, cf=1", {
  set.seed(0)

  n <- 10
  alpha <- 0.5
  cf = 1
  rand_mat <- UWBiost561::generate_partial_clique(n = n,
                                                  clique_fraction = cf,
                                                  clique_edge_density = alpha)
  adj_mat <- rand_mat$adj_mat
  res <- compute_maximal_partial_clique(adj_mat = adj_mat,
                                         alpha = alpha)

  expect_true(all(res$subset == c(1,2,3,4,5,6,7,9,10)))
})

test_that("Implementation is accurate at n=20, a=0.7, cf=0.5", {
  set.seed(1)

  n <- 20
  alpha <- 0.7
  cf = 0.5
  rand_mat <- UWBiost561::generate_partial_clique(n = n,
                                                  clique_fraction = cf,
                                                  clique_edge_density = alpha)

  adj_mat <- rand_mat$adj_mat
  res <- compute_maximal_partial_clique(adj_mat = adj_mat,
                                         alpha = alpha)

  expect_true(all(res$subset == c(1,2,3,4,9,11,14,17,20)))
})
