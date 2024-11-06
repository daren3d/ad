\name{GermanHyperinflation}
\alias{GermanHyperinflation}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Money supply and foreign exchange of Germany between 1921 and 1923
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A monthly time series of Germany's money supply and foreign exchange between 1921 and 1923.
}
\usage{data("GermanHyperinflation")}
\format{
  A data frame with 31 observations on the following 3 variables.
  \describe{
    \item{\code{date}}{A character vector.  Year / month.}
    \item{\code{logM}}{A numeric vector.  Natural log of the real money supply.}
    \item{\code{pi}}{A numeric vector.  Premium on a forward contract for foreign exchange.}
  }
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Wecker, W. E. and Ansley, C. F. (1981) Extensions and examples of the signal extraction approach to regression. To appear in \emph{Applied Time Series Analysis of Economic Data} (A. Zeilner, ed.)
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Eubank, R. (1985). Diagnostics for smoothing splines. \emph{Journal of the royal statistical society series b-methodological, 47,} 332–341.
}
\examples{
data(GermanHyperinflation)
plot(logM ~ log(pi), data = GermanHyperinflation)
}
\keyword{datasets}
