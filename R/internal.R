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

#' Implement the internal test
#'
#' The internal test is wrapped in a `for` loop.  Fits a smoothing spline under
#'  independence.
#'
#' @param data A `data.frame` with colnames "id", "x", "y".
#' @param method Character.  See `pea_adjust`.
#' @param sig Numeric.  Nominal significance level.
#'
#' @return `data` with an additional column "ana", logical on whether the
#'  observation is an anomaly.
#' @export
#'
#' @examples
#' # to do
meth.internal <- function(data, method, sig = 0.01){
  uid <- unique(data$id)
  data$ana <- NA
  for(i in 1:length(uid)){
    # subset to subject
    rn <- data$id == uid[i]
    data_i <- data[rn, ]
    # check for anomalies
    mod_i <- mgcv::gam(y ~ s(x, bs = "bs"), data = data_i)
    p_i1 <- internal(mod_i)
    p_i2 <- pea_adjust(p_i1, method = method)
    data$ana[rn] <- p_i2 <= sig
  }
  return(data)
}
