context("Check simulations")
source("simulate_reads.R")
test_that("true seq creation function works", {
  # Test 1: Ensure every site has at least one 1
  n_snps <- 10         # number of sites (columns)
  n_true_seqs <- 5     # number of sequences (rows)
  result1 <- make_true_seqs(n_snps, n_true_seqs)
  
  expect_true(all(colSums(result1) > 0),
              info = "Each site (column) should have at least one nonzero entry")
  
  # Test 2: Ensure no site is entirely 1s
  n_snps2 <- 20
  n_true_seqs2 <- 3
  result2 <- make_true_seqs(n_snps2, n_true_seqs2)
  
  expect_true(all(colSums(result2) < n_true_seqs2),
              info = "No site (column) should be entirely 1s")
  
  
  # Test 3: Ensure all entries are 0 or 1
  n_snps3 <- 15
  n_true_seqs3 <- 4
  result3 <- make_true_seqs(n_snps3, n_true_seqs3)
  
  expect_true(all(result3 %in% c(0, 1)),
              info = "Matrix should only contain 0s and 1s")
  
  # Test 4: Check that function returns a matrix type
  expect_true(is.matrix(result3),
              info = "Function should return a matrix")
  
})