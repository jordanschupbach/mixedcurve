library(inline)

# Comparison function for sorting
compare_function <- function(a, b) {
  return(REAL(a)[0] < REAL(b)[0])
}

# C function implementation using inline
romano_wolf_c <- cfunction(signature(t_stats = "numeric", boot_t_stats = "matrix"), "
  int ns = LENGTH(t_stats);
  int nb = Rf_nrows(boot_t_stats); // Use Rf_nrows instead of nrows

  // Allocate memory with appropriate casting
  double *abs_t_stats = (double *) malloc(ns * sizeof(double));
  double *abs_boot_t_stats = (double *) malloc(nb * ns * sizeof(double));
  double *pinit = (double *) malloc(ns * sizeof(double));
  double *corr_padj = (double *) malloc(ns * sizeof(double));
  double *pval = (double *) malloc(ns * sizeof(double));
  int *stepdown_index = (int *) malloc(ns * sizeof(int));

  // Calculate absolute values of t_stats
  for (int i = 0; i < ns; i++) {
    abs_t_stats[i] = (REAL(t_stats)[i] < 0) ? -REAL(t_stats)[i] : REAL(t_stats)[i];
    stepdown_index[i] = i; // Initialize stepdown index
  }

  // Calculate absolute values of boot_t_stats
  for (int i = 0; i < nb; i++) {
    for (int j = 0; j < ns; j++) {
      abs_boot_t_stats[i * ns + j] = (REAL(boot_t_stats)[i + j * nb] < 0) ? -REAL(boot_t_stats)[i + j * nb] : REAL(boot_t_stats)[i + j * nb];
    }
  }

  // Sort the index based on abs_t_stats (simple bubble sort for demonstration)
  for (int i = 0; i < ns; i++) {
    for (int j = 0; j < ns - 1; j++) {
      if (abs_t_stats[stepdown_index[j]] < abs_t_stats[stepdown_index[j + 1]]) {
        int temp = stepdown_index[j];
        stepdown_index[j] = stepdown_index[j + 1];
        stepdown_index[j + 1] = temp;
      }
    }
  }

  // Initialize pinit
  for (int s = 0; s < ns; s++) {
    int idx = stepdown_index[s];
    double max_stat = 0;
    for (int i = 0; i < nb; i++) {
      if (abs_boot_t_stats[i * ns + idx] > max_stat) {
        max_stat = abs_boot_t_stats[i * ns + idx];
      }
    }

    if (s == 0) {
      pinit[s] = (max_stat >= abs_t_stats[idx]) ? 1.0 : 0.0;
    } else {
      pinit[s] = (max_stat >= abs_t_stats[idx]) ? 1.0 : pinit[s - 1] + 1.0 / (nb + 1);
    }
    pinit[s] = pinit[s] < 1.0 ? pinit[s] : 1.0; // Cap at 1.0
  }

  // Create adjusted p-values
  for (int j = 0; j < ns; j++) {
    corr_padj[j] = (j == 0) ? pinit[j] : fmax(pinit[j], corr_padj[j - 1]);
  }

  // Permutate back to original order
  for (int j = 0; j < ns; j++) {
    pval[j] = corr_padj[stepdown_index[j]];
  }

  SEXP res = PROTECT(Rf_allocVector(REALSXP, ns));
  for (int i = 0; i < ns; i++) {
    REAL(res)[i] = pval[i];
  }

  // Free allocated memory
  free(abs_t_stats);
  free(abs_boot_t_stats);
  free(pinit);
  free(corr_padj);
  free(pval);
  free(stepdown_index);

  UNPROTECT(1);
  return res;
")
