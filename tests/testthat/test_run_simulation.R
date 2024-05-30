context("Testing run_simulation")

test_that("run_simulation works", {
  rm(list = ls())
  library(UWBiost561)
  set.seed(0)
  run_simulation()

  expect_true(TRUE)
})
