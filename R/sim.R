#' Create anomalous data
#'
#' Creates data for one subject.  User can specify functional form, anomaly
#' size, error and sample size.
#'
#' @param fun A character specifying functional form.  Options include "Beta",
#'  "Cos", "Exp" and "Quad".
#' @param id A unique subject identifier.
#' @param zeta Numeric; magnitude of anomaly.
#' @param sigma Numeric; noise.
#' @param n Numeric; sample size.
#'
#' @return A data.frame with columns, id, x, f, z and y.
#' @export
#'
#' @examples
#' set.seed(808)
#' dat1 <- rbind(create.data("Beta", 1),
#'               create.data("Cos", 2, 0),
#'               create.data("Exp", 3, 2),
#'               create.data("Quad", 4))
#' plot(y ~ x, data = dat1, col = id)
create.data <- function(fun, id, zeta = 1, sigma = 0.1, n = 50) {
  te <- seq(0, 1, length.out = n)  # Equally spaced t
  half_step <- (te[2] - te[1]) / 2
  te_minus <- te - half_step
  te_plus <- te + half_step
  tu <- runif(n, te_minus, te_plus)  # Unequally spaced t
  x <- (tu - min(tu)) / (max(tu) - min(tu))  # Scale back to [0,1]
  fun <- match.arg(fun, paste0(c("Beta", "Cos", "Exp", "Quad"), ".create"))
  f <- do.call(fun, list(t = x))  # Underlying function
  u <- sample(2:(n - 1), 1)  # Anomaly location
  z <- numeric(n)
  z[u] <- zeta
  e <- rnorm(n, sd = sigma)  # Noise
  y <- f + z + e
  data.frame(id = id,
             x = x,
             f = f,
             z = z,
             y = y)
}
Beta.create <- function(t){
  eb <- function(t, p, q){
    gamma(p+q) * t^(p-1) * (1-t)^(q-1) / gamma(p) / gamma(q)
  }
  y1 <- (eb(t, 10, 5) + eb(t, 7, 7) + eb(t, 5, 10)) / 3
  y2 <- y1 - min(y1)
  y3 <- y2 / max(y2)
  return(y3)
}
Cos.create <- function(t){
  y1 <- sqrt(2) / 2 * cos(8 * pi * t) + sqrt(2) * pi * t + 2
  y2 <- y1 - min(y1)
  y3 <- y2 / max(y2)
  return(y3)
}
Exp.create <- function(t){
  y1 <- 4.26 * (exp(-3.25 * t) - 4 * exp(-6.5 * t) + 3 * exp(-9.75 * t))
  y2 <- y1 - min(y1)
  y3 <- y2 / max(y2)
  return(y3)
}
Quad.create <- function(t){
  y1 <- 0.8 * t^2 - 0.5 * t + 2
  y2 <- y1 - min(y1)
  y3 <- y2 / max(y2)
  return(y3)
}
