#' Prepare results for plotting
#'
#' Reorganizes the output from [adc()] so it can be plotted.  It sorts the
#' subjects based their smallest p-values.
#'
#' @param res A `data.frame` output from [adc()].
#'
#' @return A `data.frame` that is longer and wider so that the suggested values
#'  can be plotted with a different color and symbol.
#' @export
#'
#' @examples
#' data("GermanHyperinflation")
#' dat <- data.frame(id = "a",
#'                   x = log(GermanHyperinflation$pi),
#'                   y = GermanHyperinflation$logM)
#' res  <- adc(data = dat, eu = "in", pva = "none", alpha = 0.05)
#' res2 <- preplot(res = res)
#' plot(y ~ x, data = res2, col = col, pch = pch)
preplot <- function(res) {
  if(!any(res$ana)) {
    cat("No plot because no anomalies.")
    return(res)
  }
  # Anomalous data
  res2b <- res[res$ana, , drop = FALSE]
  res2b$col <- 3
  res2b$pch <- 2
  res2b$y <- res2b$rep
  # Sort the adjusted p-values
  id <- res2b$id[order(res2b$apv)]
  id <- id[!duplicated(id)]
  # Observed data
  res2a <- res[res$id %in% id, ]
  res2a$col <- as.numeric(res2a$ana) + 1
  res2a$pch <- 1
  #
  res2d <- rbind(res2a, res2b)
  res2d$id2  <- factor(res2d$id,  levels = id )
  res2d$col2 <- factor(res2d$col, levels = 1:3)
  res2d$pch2 <- factor(res2d$pch, levels = 1:2)
  return(res2d)
}
