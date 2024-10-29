source("llr_functions.R")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix, that all the elements are positive, that the weights are correct in simple cases where you know what the output shuold be
  z <- 2
  x <- c(1, 2, 3)
  omega <- 1
  expected_matrix <- diag(c(w_fun(abs(1 - 2) / 1), w_fun(abs(2 - 2) / 1), w_fun(abs(3 - 2) / 1)))
  
  result <- make_weight_matrix(z, x, omega)
  
  expect_true(all(result[row(result) != col(result)] == 0)) 
  
  expect_true(all(result >= 0))
  
  expect_equal(result, expected_matrix)
  
  
  
  getwd()
  
})

test_that("make_predictor_matrix works on simple cases", {
  ## write tests to check that the dimensions are correct, the first column is all 1's, etc.
  
  x <- c(5, 10, 15)
  n <- length(x)
  expected_matrix <- matrix(c(rep(1, n), x), n, 2)
  
  result <- make_predictor_matrix(x)
  
  
  expect_equal(dim(result), c(n, 2))
  
  
  expect_equal(result[, 1], rep(1, n))
  
  expect_equal(result[, 2], x)
})
