#' @title CDVI
#' @description Cuddy-Della Valle Index for Capturing the Instability in Time Series Data.
#'
#' @param data Name of the data taken for the study
#' @param verbose Logical. If TRUE, the function prints detailed information about its progress. Default is FALSE.

#' @examples{
#' library(CDVI)
#' Prices <- runif(15, min = 800, max = 1200)
#' data <- data.frame(Prices)
#' CDVI(data = data$Prices)
#'}
#'
#' @references
#' 1. Shankar, S. V., Chandel, A., Gupta, R. K., Sharma, S., Chand, H., Kumar, R., ... & Gowsar, S. N. (2023). Corrigendum: Exploring the dynamics of arrivals and prices volatility in onion (Allium cepa) using advanced time series techniques. Frontiers in Sustainable Food Systems, 7, 1290515. DOI: 10.3389/fsufs.2023.1208898
#'
#'@return CV, CDVI
#'@export

CDVI <- function(data,verbose = TRUE) {
  y <- ts(data)
  t <- seq((1:length(y)))

  # Linear model
  linear_model <-stats::lm(y ~ t)
  slm <- round(summary(linear_model)$adj.r.squared, 3)

  # Exponential model
  Exponential_model <-stats::lm(log(y) ~ t)
  sem <- base::round(summary(Exponential_model)$adj.r.squared, 3)

  # Quadratic model
  Quadratic_model <-stats::lm(y ~ t + I(t^2))
  sqm <- base::round(summary(Quadratic_model)$adj.r.squared, 3)

  # Cubic model
  Cubic_model <-stats::lm(y ~ t + I(t^2) + I(t^3))
  scm <- base::round(summary(Cubic_model)$adj.r.squared, 3)

  # Power model
  Power_model <- stats::lm(log(y) ~ log(t))
  spm <- base::round(summary(Power_model)$adj.r.squared, 3)

  adj_r_squared <- c(Linear = slm, Exponential = sem, Quadratic = sqm, Cubic = scm, Power = spm)
  best_model <- names(adj_r_squared)[which.max(adj_r_squared)]
  AdjR <- adj_r_squared[which.max(adj_r_squared)]

  Mean <- base::mean(y)
  Std <- stats::sd(y)
  CV <- (Std / Mean) * 100
  cdvi <- CV * sqrt(1 - AdjR)

  if (verbose) {
    message("Fitted models:")
    print(adj_r_squared)
    message("Adjusted R squared value of the best-fitted model:")
    print(AdjR)
  }

  results <- cbind("CV" = CV,"CDVI" = cdvi)
  return(results)
}
