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
#'
#' @examples
#' # to do
#' 
external_ar1 <- function(mod, x, w) {
  n_obs <- length(x)
  ## Create hat matrix
  H <- matrix(as.numeric(NA), n_obs, n_obs)
  for(j in 1:n_obs){
    y <- numeric(n_obs)
    y[j] <- 1
    mod2a <- mgcv::gam(y ~ s(x, bs = "bs", sp = mod$sp), fit = FALSE)
    mod2b <- mgcv::magic(mod2a$y, mod2a$X, mod2a$sp, mod2a$S, mod2a$off,
                         rank = mod2a$rank, C = mod2a$C, w = w)
    mod_i <- magic2gam(mod2a, mod2b, w)
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

#' Update `gamObject` after `magic`
#'
#' @param gam_F A `gamObject` with `fit = FALSE`.
#' @param magic Results from `magic`.
#' @param w The (decomposed) information matrix.
#'
#' @return A `gamObject`.
#'
#' @examples
#' # to do
magic2gam <- function(gam_F, magic, w) {
  mod_gam <- mgcv::gam(G = gam_F)
  #
  mod_gam$coefficients <- magic$b
  mod_gam$sig2 <- magic$scale
  mod_gam$gcv.ubre <- magic$score
  mod_gam$sp <- magic$sp
  # ? <- magic$sp.full
  mod_gam$rV <- magic$rV  # root of Bayesian covariance matrix
  # ? <- magic$gcv.info  # a long list...
  mod_gam$R <- magic$R
  #
  magic2 <- mgcv::magic.post.proc(gam_F$X, magic, w)
  mod_gam$Vp <- magic2$Vb  # Bayesian covariance matrix.  Used by plot
  mod_gam$Ve <- magic2$Ve
  mod_gam$hat <- magic2$hat
  mod_gam$edf <- magic2$edf
  #
  magic_fitted <- as.numeric(gam_F$X %*% magic$b)
  mod_gam$fitted.values <- magic_fitted
  mod_gam$residuals <- magic_fitted - mod_gam$y
  return(mod_gam)
}

#' Implement the external test
#'
#' The external test is wrapped in a `for` loop.  Fits a smoothing spline under
#'  AR-1.  (Need to generalize to other corr structures.)
#'
#' @param data A `data.frame` with colnames "id", "x", "y".
#' @param method Character.  See `pea_adjust`.
#' @param sig Numeric.  Nominal significance level.
#'
#' @return `data` with an additional column "ana", logical on whether the
#'  observation is an anomaly.
#'
#' @examples
#' # to do
meth.external <- function(data, method, sig = 0.01) {
  uid <- unique(data$id)
  data$ana <- NA
  for(i in 1:length(uid)){
    # subset to subject
    rn <- data$id == uid[i]
    data_i <- data[rn, ]
    x <- data_i$x
    y <- data_i$y
    # fit model
    mod1 <- mgcv::gamm(y ~ s(x, bs = "bs"), correlation = nlme::corAR1())
    rho <- summary(mod1$lme)$modelStruct$corStruct
    # rho <- coef(mod1$lme$modelStruct$corStruct)  # not quite better
    # ?nlme:::coef.corAR1()
    w <- nlme::corMatrix(nlme::Initialize(nlme::corAR1(rho),
                                          data.frame(x = x))) |>
      chol() |>
      t() |>
      solve()
    mod2a <- mgcv::gam(y ~ s(x, bs = "bs"), fit = FALSE)
    mod2b <- mgcv::magic(mod2a$y, mod2a$X, mod2a$sp, mod2a$S, mod2a$off,
                         rank = mod2a$rank, C = mod2a$C, w = w)
    mod_i <- magic2gam(mod2a, mod2b, w)
    # check for anomalies
    p_i1 <- external_ar1(mod_i, x, w)
    p_i2 <- pea_adjust(p_i1, method = method)
    data$ana[rn] <- p_i2 <= sig
  }
  return(data)
}
