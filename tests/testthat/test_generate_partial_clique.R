context("Testing generate_partial_clique")

test_that("generate_partial_clique works", {
  set.seed(10)
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)

  expect_true(is.list(res))
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat) == c(10,10)))
})


test_that("generate_partial_clique returns valid adjacency matrix", {
  set.seed(10)
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)

  ## check final adjacency matrix is valid
  adj_mat <- res$adj_mat

  # is numeric
  expect_true(is.numeric(adj_mat))
  # elements are 0 or 1
  expect_true(all(adj_mat == 0 | adj_mat == 1))
  # 1's along the diagonal
  expect_true(all(diag(adj_mat) == 1))
  # symmetric matrix (equal to its transpose)
  expect_true(all.equal(adj_mat, t(adj_mat)))
})
