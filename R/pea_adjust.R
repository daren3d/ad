#' Adjust for multiple comparisons.
#'
#' Possible methods include the usual suspects, `stats::p.adjust.methods`, Cook
#'  and Wiesberg (), and Oleson et al. ().  It can also use a function in the
#'  global environment.
#'
#' @param p A list, from `internal` or `external`.
#' @param method A character with *exact* match to stats::p.adjust.methods, "CW",
#'  "oleson", or a function in the global environment.
#'
#' @return a vector of adjusted p-values.
#' @export
#'
#' @examples
#' # to do
pea_adjust <- function(p, method){
  if(method %in% stats::p.adjust.methods){
    p2 <- stats::p.adjust(p$pval, method)
  }else if(method == "CW"){
    p2 <- p$pval * p$trH / p$h
  }else if(method == "oleson"){
    n_obs <- length(p$pval)
    df <- round(n_obs - p$trH)
    rho <- suppressWarnings(bdots::ar1Solver(p$stat))
    p2 <- bdots::p_adjust(p$pval, n = n_obs, alpha = 0.05, df = df, rho = rho,
                          cores = 0)
    attributes(p2) <- NULL
  }else{  # user-defined

  }
  p2 <- pmin(p2, 1)
  return(p2)
}
