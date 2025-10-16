# test_simulations.R
context("Check simulations")
source("simulate_reads.R")

test_that("true seq creation function works", {
  # Test 1: Each site should have at least one 1
  n_snps <- 10
  n_true_seqs <- 5
  result1 <- make_true_seqs(n_snps, n_true_seqs)
  expect_true(all(colSums(result1) > 0),
              info = "Each site (column) should have at least one nonzero entry")
  
  # Test 2: No site should be entirely 1s
  n_snps2 <- 20
  n_true_seqs2 <- 3
  result2 <- make_true_seqs(n_snps2, n_true_seqs2)
  expect_true(all(colSums(result2) < n_true_seqs2),
              info = "No site (column) should be entirely 1s")
  
  # Test 3: Matrix should only contain 0s and 1s
  n_snps3 <- 15
  n_true_seqs3 <- 4
  result3 <- make_true_seqs(n_snps3, n_true_seqs3)
  expect_true(all(result3 %in% c(0, 1)),
              info = "Matrix should only contain 0s and 1s")
  
  # Test 4: Function should return a matrix
  expect_true(is.matrix(result3),
              info = "Function should return a matrix")
  
  # Test 5: New behavior — respects num_nonzero_per_site vector
  n_snps4 <- 5
  n_true_seqs4 <- 6
  num_nonzero_per_site <- c(2, 3, 1, 4, 5)
  result4 <- make_true_seqs(n_snps4, n_true_seqs4, num_nonzero_per_site)
  
  expect_equal(colSums(result4), num_nonzero_per_site,
               info = "Each column should have the specified number of 1s")
  expect_true(all(result4 %in% c(0, 1)),
              info = "Matrix should only contain 0s and 1s")
  expect_equal(dim(result4), c(n_true_seqs4, n_snps4),
               info = "Matrix dimensions should match input parameters")
  
  # Test 6: Invalid num_nonzero_per_site input should trigger an error
  n_snps5 <- 4
  n_true_seqs5 <- 3
  
  # Wrong vector length
  expect_error(
    make_true_seqs(n_snps5, n_true_seqs5, c(1, 2, 3)),
    regexp = "same length as n_snps",
    info = "Should stop if num_nonzero_per_site length is wrong"
  )
  
  # Too many 1s requested
  expect_error(
    make_true_seqs(n_snps5, n_true_seqs5, c(1, 4, 2, 3)),
    regexp = "Cannot have more 1s than the number of sequences",
    info = "Should stop if num_nonzero_per_site exceeds n_true_seqs"
  )
})