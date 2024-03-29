## code to prepare `GermanHyperinflation` dataset goes here

usethis::use_data(GermanHyperinflation, overwrite = TRUE)

# Eubank (1985) included a data analysis of the German hyperinflation.  The data
# were provided in Wecker, W. E. and Ansley, C. F. (1981) 
# Extensions and examples of the signal extraction approach to regression.
# To appear in Applied Time Series Analysis of Economic Data (A. Zeilner, ed.)
# Note: M is real money supply; pi is premium on a forward contract for foreign 
# exchange.

yr <- paste0("19", 21:23)
mo <- c(paste0("0", 1:9), 10:12)
yrmo <- paste(rep(yr, each = 12), rep(mo, times = 3), sep = "/")
GermanHyperinflation <- data.frame(
  date = yrmo[c(-1, -33:-36)],
  logM = c(6.5474, 6.5605, 6.5802, 6.5927, 6.5896, 6.5415, 6.5019, 6.5381, 
           6.4977, 6.4225, 6.4580, 6.4129, 6.2669, 6.1841, 6.0839, 6.0578, 
           6.0774, 5.9321, 5.7858, 5.5203, 5.3920, 5.1504, 5.2421, 5.1921, 
           4.9010, 5.2718, 5.4116, 5.4239, 5.4269, 4.7586, 4.7712),
  pi = c(0.1660, 0.1620, 0.3300, 0.3680, 0.5200, 0.6730, 0.5050, 0.6940, 0.7210,
         0.8300, 0.6750, 0.7340, 0.8320, 0.9620, 0.9580, 1.0000, 1.1050, 1.3970,
         3.2690, 5.1390, 13.5350, 11.9100, 6.8500, 16.3700, 22.4700, 5.8300, 
         11.4000, 13.3400, 19.2300, 50.2800, 37.2200)
)
