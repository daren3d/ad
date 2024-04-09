#' Externally standardized residual test
#'
#' The formulas in the paper are incorrect.  The correct formulas are below.
#'
#' @param mod A `gamObject` from **mgcv**.
#' @param x The independent variable, usually time.
#'
#' @return A list with p-values, diagonal of hat matrix and their sum (aka.
#'  degrees of freedom), and the test statistic values.
#' @export
#'
#' @examples
#' # To do.
external <- function(mod, x) {
  n_obs <- length(x)
  r <- residuals(mod)
  h <- mod$hat
  ## Create hat matrix
  H <- matrix(as.numeric(NA), n_obs, n_obs)
  for(j in 1:n_obs) {
    y <- numeric(n_obs)
    y[j] <- 1
    mod2 <- mgcv::gam(y ~ s(x, bs = "bs", sp = mod$sp))
    H[, j] <- fitted(mod2)
  }
  ##
  trH <- sum(h)
  h_a <- trH - h
  h_b <- (colSums(H) - h)^2 / (1 - h)
  trHj <- h_a + h_b
  ##
  rH <- r / (1 - h)
  sd_a <- sum(r^2) - r^2
  sd_b <- 2 * rH * (colSums(r * H) - r * h)
  sd_c <- rH^2 * (colSums(H^2) - h^2)
  sd_ex <- sqrt((sd_a + sd_b + sd_c) / (n_obs - 1 - trHj))
  ##
  stat <- r / sd_ex / sqrt(1 - h)
  pv <- 2 * pt(-abs(stat), df = n_obs - 1 - trHj)
  list(pval = pv, h = h, trH = trH, stat = stat)
}

#' Internally standardized residual test
#'
#' @param mod A `gamObject` from **mgcv**.
#' @param x The independent variable, usually time.
#'
#' @return A list with p-values, diagonal of hat matrix and their sum (aka.
#'  degrees of freedom), and the test statistic values.
#' @export
#'
#' @examples
#' # To do.
internal <- function(mod, x) {
  n_obs <- length(x)
  r <- residuals(mod)
  h <- mod$hat
  trH <- sum(h)
  sd_in <- sqrt(sum(r^2) / (n_obs - trH))
  stat <- r / sd_in / sqrt(1 - h)
  pv <- 2 * pt(-abs(stat), df = n_obs - trH)
  list(pval = pv, h = h, trH = trH, stat = stat)
}

#' Anomaly Detection and Correction
#'
#' @param data A `data.frame` with colnames "id", "x", "y".
#' @param eu Character string specifying the residual test.  Must be "ex" or 
#' "in".
#' @param pva Character string specifying p-value adjustment.  See pea_adjust.
#' @param alpha Numeric value of nominal significance level.
#'
#' @return Original data with an additional columns: "ana" (logical on whether the
#'  observation is an anomaly), "rep" (replacement value, if necessary), and "apv" (adjusted p-value). 
#' @export
#'
#' @examples
#' # To do.
adc <- function(data, eu = "in", pva = "BY", alpha = 0.01) {
  mod <- mgcv::gam(y ~ s(x, bs = "bs"), data = data)
  if(eu == "in") {
    res <- internal(mod, data$x)
  } else if(eu == "ex") {
    res <- external(mod, data$x)
  } else {
    stop("Improper `eu` specified.")
  }
  apv <- pea_adjust(res, method = pva, alpha = alpha)  # adjusted p-value
  ana <- apv <= alpha  # anomalies
  repl <- rep(NA, length(ana))  # replacement(s)
  if(any(ana)) {
    for(a in which(ana)) {
      data2 <- data
      data2$y[a] <- NA
      mod2 <- mgcv::gam(y ~ s(x, bs = "bs", sp = mod$sp), data = data2)
      repl[a] <- predict(mod2, newdata = data.frame(data2[a, ]))
    }
  }
  data$ana <- ana  # anomaly T/F
  data$rep <- repl  # replacement value
  data$apv <- apv  # adjusted p-value
  return(data)
}