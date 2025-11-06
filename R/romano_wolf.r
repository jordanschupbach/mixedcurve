#' Romano-Wolf Stepdown Multiple Testing Procedure
#'
#' This function implements the Romano-Wolf stepdown procedure for adjusting p-values
#' in multiple hypothesis testing scenarios. It takes observed test statistics and
#' bootstrap test statistics as inputs and returns adjusted p-values.
#'
#' @param tstar A numeric vector of observed test statistics.
#' @param boot_tstar A numeric matrix of bootstrap test statistics, where each row
#' represents a bootstrap sample and each column corresponds to a test statistic.
#'
#' @return A numeric vector of Romano-Wolf adjusted p-values.
#'
#' @examples
#' # Example
#' set.seed(123)
#' tstar <- rnorm(5, mean = 2, sd = 1)
#' boot_tstar <- matrix(rnorm(1000 * 5, mean = 0, sd = 1), nrow = 1000, ncol = 5)
#' adjusted_pvals <- romano_wolf(tstar, boot_tstar)
#' print(adjusted_pvals)
#' @export
#'
#' @note: This implementation is nearly identical to the one in the 'wildrwolf'
#' package except for minor variable renames. This just to avoid dependency on that
#' package, but this package and that are both GPL3. That implementation
#' (in 'wildrwolf') is nearly identical to the one in 'hdm' package (which is MIT licensed).
#'
#' @references
#' https://github.com/s3alfisc/wildrwolf/blob/main/R/get_rwolf_pval.R
#' https://github.com/cran/hdm/blob/master/R/p_adjust.R
romano_wolf <- function(tstar, boot_tstar) {
  tstar <- abs(tstar)
  boot_tstar <- abs(boot_tstar)
  ns <- ncol(boot_tstar)
  nb <- nrow(boot_tstar)
  pinit <- numeric(ns)
  corr_padj <- numeric(ns)
  pval <- numeric(ns)
  r <- order(tstar, decreasing = TRUE)
  ro <- order(r)
  for (s in 1:ns) {
    if (s == 1) {
      max_stat <- apply(boot_tstar, 1, max)
      pinit[s] <- pmin(
        1,
        (sum(max_stat >= abs(tstar[r[s]])) + 1) / (nb + 1)
      )
    }
    if (s > 1) {
      boot_tstar_udp <- boot_tstar[, -r[1:(s - 1)],
        drop = FALSE
      ]
      max_stat <- apply(boot_tstar_udp, 1, max)
      pinit[s] <- pmin(
        1,
        (sum(max_stat >= abs(tstar[r[s]])) + 1) / (nb + 1)
      )
    }
  }
  for (j in 1:ns) {
    if (j == 1) {
      corr_padj[j] <- pinit[j]
    }
    if (j > 1) {
      corr_padj[j] <- max(pinit[j], corr_padj[j - 1])
    }
  }
  pval <- corr_padj[ro]
  pval
}
