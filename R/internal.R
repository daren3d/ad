#' Eubank's internally standardized residual test
#'
#' Figure out how to make LaTeX-like formulas.
#'
#' @param mod A `gamObject` from **mgcv**.
#'
#' @return a list with p-values, diagonal of hat matrix and their sum (aka.
#'  degrees of freedom), and the test statistic values.
#' @export
#'
#' @examples
#' # to do
internal <- function(mod){
  r <- residuals(mod)
  h <- mod$hat
  n_obs <- length(r)
  trH <- sum(h)
  sd_in <- sqrt( sum(r^2) / (n_obs - trH) )
  stat <- r / sd_in / sqrt(1 - h)
  pv <- 2 * pt(-abs(stat), df = n_obs - trH)
  out <- list(pval = pv, h = h, trH = trH, stat = stat)
  return(out)
}
