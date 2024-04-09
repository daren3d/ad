#' Adjust for multiple comparisons.
#'
#' Possible methods include the usual suspects, `stats::p.adjust.methods`, Cook
#'  and Wiesberg (), and Oleson et al. ().  It can also use a function in the
#'  global environment.
#'
#' @param res A list, from `internal` or `external`.
#' @param method A character with *exact* match to stats::p.adjust.methods, "CW",
#'  "oleson", or a function in the global environment.
#' @param alpha The significance level.
#'
#' @return a vector of adjusted p-values.
#' @export
#'
#' @examples
#' # to do
pea_adjust <- function(res, method, alpha) {
  if(method %in% stats::p.adjust.methods) {
    apv <- stats::p.adjust(res$pval, method)
  } else if(method == "CW") {
    apv <- res$pval * res$trH / res$h
  } else if(method == "oleson") {
    n_obs <- length(res$pval)
    df <- round(n_obs - res$trH)
    rho <- suppressWarnings(bdots::ar1Solver(res$stat))
    apv <- bdots::p_adjust(res$pval, n = n_obs, alpha = alpha, df = df, 
                           rho = rho, cores = 0)
    attributes(apv) <- NULL
  } else {  # user-defined. To do.

  }
  pmin(apv, 1)
}
