context("Testing compute_maximal_partial_clique")

test_that("compute_maximal_partial_clique works", {
  set.seed(0)

  adj_mat <- matrix(c(1,1,1,0,1,
                      1,1,1,1,1,
                      1,1,1,0,1,
                      0,1,0,1,1,
                      1,1,1,1,1),
                    nrow = 4, byrow = TRUE)

  res <- compute_maximal_partial_clique(
    adj_mat = adj_mat,
    alpha = 0.9
  )
  res

  expect_true(is.list(res))
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat) == c(10,10)))
})
