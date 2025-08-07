# Load necessary libraries
library(splines)
library(lmtest)
library(forecast)

# Function to simulate data and fit natural spline model
fit_spline_model <- function(autocorrelation_strength) {
  n <- 100
  time <- 1:n
  x <- sin(time / 10)  # Simulating a trend
  
  # Introduce autocorrelation
  y <- stats::filter(x, sides=1, filter=c(autocorrelation_strength), 
                     circular=TRUE) + rnorm(n,sd = .02)
  
  df <- data.frame( time = time, y = y)
  
  # Fit a natural spline model
  model <- lm(y ~ ns(time, df=8), df)
  
  # Check for autocorrelation in residuals
  dw_test <- dwtest(model)
  
  # Return model and Durbin-Watson test result
  list(model = model, dw_test = dw_test, y = y)
}

# Fit models with different levels of autocorrelation
low_ac_model <- fit_spline_model(0.2)
high_ac_model <- fit_spline_model(0.9)
extreme_ac_model <- fit_spline_model(0.999)

# Compare models
summary(low_ac_model$model)
summary(high_ac_model$model)
summary(extreme_ac_model$model)

low_ac_model$model %>% effects::predictorEffects(partial.residuals = TRUE) %>% plot
high_ac_model$model %>% effects::predictorEffects(partial.residuals = TRUE) %>% plot
extreme_ac_model$model %>% effects::predictorEffects(partial.residuals = TRUE) %>% plot

# Durbin-Watson test results
low_ac_model$dw_test
high_ac_model$dw_test
extreme_ac_model$dw_test
