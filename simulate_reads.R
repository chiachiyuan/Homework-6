# simulate_reads.R

#' @param n_snps The length of the sequence
#' @param n_true_seqs The number of sequences
#' @param num_nonzero_per_site Optional vector specifying how many 1's each site should have
#' @return A matrix of 0's and 1's with number of rows equal to n_true_seqs 
#' and number of columns equal to n_snps
make_true_seqs <- function(n_snps, n_true_seqs, num_nonzero_per_site = NULL) {
  
  # If num_nonzero_per_site is not provided, use the original behavior
  if (is.null(num_nonzero_per_site)) {
    true_seqs <- matrix(NA, nrow = n_true_seqs, ncol = n_snps)
    for (i in 1:n_snps) {
      snp_vals <- sample(0:1, size = n_true_seqs, replace = TRUE)
      if (all(snp_vals == 0)) {
        snp_vals[sample(n_true_seqs, 1)] <- 1
      } else if (all(snp_vals == 1)) {
        snp_vals[sample(n_true_seqs, 1)] <- 0
      }
      true_seqs[, i] <- snp_vals
    }
    return(true_seqs)
  }
  
  # If num_nonzero_per_site is provided, generate accordingly
  if (length(num_nonzero_per_site) != n_snps) {
    stop("num_nonzero_per_site must have the same length as n_snps")
  }
  if (any(num_nonzero_per_site > n_true_seqs)) {
    stop("Cannot have more 1s than the number of sequences")
  }
  
  true_seqs <- matrix(0, nrow = n_true_seqs, ncol = n_snps)
  for (i in 1:n_snps) {
    if (num_nonzero_per_site[i] > 0) {
      ones <- sample(1:n_true_seqs, num_nonzero_per_site[i])
      true_seqs[ones, i] <- 1
    }
  }
  return(true_seqs)
}