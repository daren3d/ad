#' Eubank's externally standardized residual test
#'
#' The formulas in the paper are incorrect.  The correct formulas are below.
#'  Figure out how to make LaTeX-like formulas.
#'
#' @param mod A `gamObject` from **mgcv**.
#' @param x The independent variable, usually time.
#' @param w The (decomposed) information matrix.
#'
#' @return a list with p-values, diagonal of hat matrix and their sum (aka.
#'  degrees of freedom), and the test statistic values.
#' @export
#'
#' @examples
#' # to do
ext_ind <- function(mod, x){
  n_obs <- length(x)
  ## Create hat matrix
  H <- matrix(as.numeric(NA), n_obs, n_obs)
  for(j in 1:n_obs){
    y <- numeric(n_obs)
    y[j] <- 1
    mod2 <- mgcv::gam(y ~ s(x, bs = "bs", sp = mod$sp))
    H[, j] <- fitted(mod2)
  }
  ##
  h <- mod$hat
  trH <- sum(h)
  h_a <- trH - h
  h_b <- (colSums(H) - h)^2 / (1 - h)
  trHj <- h_a + h_b
  ##
  r <- residuals(mod)
  rH <- r / (1 - h)
  sd_a <- sum(r^2) - r^2
  sd_b <- 2 * rH * (colSums(r * H) - r * h)
  sd_c <- rH^2 * (colSums(H^2) - h^2)
  sd_ex <- sqrt((sd_a + sd_b + sd_c) / (n_obs - 1 - trHj))
  ##
  stat <- r / sd_ex / sqrt(1 - h)
  pv <- 2 * pt(-abs(stat), df = n_obs - 1 - trHj)
  out <- list(pval = pv, h = h, trH = trH, stat = stat)
  return(out)
}

#' Implement the external test
#'
#' The external test is wrapped in a `for` loop.  Fits a smoothing spline under
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
meth.ext_ind <- function(data, method, sig = 0.01){
  uid <- unique(data$id)
  data$ana <- NA
  for(i in 1:length(uid)){
    # subset to subject
    rn <- data$id == uid[i]
    data_i <- data[rn, ]
    x <- data_i$x
    y <- data_i$y
    # fit model
    mod1 <- mgcv::gam(y ~ s(x, bs = "bs"))
    # check for anomalies
    p_i1 <- ext_ind(mod1, x)
    p_i2 <- pea_adjust(p_i1, method = method)
    data$ana[rn] <- p_i2 <= sig
  }
  return(data)
}
