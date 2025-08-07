library(dplyr)
library(ggplot2)

# needs data_w_notes with predictions to make much sense

sensor_vars <- data.frame(sensor = c(),
                          lag = c(),
                          n = c())
for ( s in 1:length(unique(data_w_notes$sensor))) {

  sensors <- unique(data_w_notes$sensor)
  act_data <- data_w_notes %>% filter(sensor == sensors[s])
  
  # Define the ranges for lag and n
  lag_range <- seq(0, 4000, length.out = 100)
  n_range <- seq(20/2, 30/2, length.out = 30)
  
  # Preallocate the size of the dataframe
  total_iterations <- length(lag_range) * length(n_range)
  o <- data.frame(lag = rep(lag_range, each = length(n_range)), 
                  n = rep(n_range, times = length(lag_range)), 
                  acf_value = numeric(total_iterations))
  
  # Initialize the progress bar
  progress <- txtProgressBar(min = 0, max = total_iterations, style = 3)
  
  iteration_count <- 0
  
  for (i in seq_len(total_iterations)) {
    lag <- o$lag[i]
    n <- o$n[i]
    
    x <- act_data$resid
    p <- cos(((as.numeric(act_data$time - act_data$time[1]) + lag) / 60) / n)
    q <- sin(((as.numeric(act_data$time - act_data$time[1]) + lag) / 60) / n)
    
    o$acf_value[i] <- summary(lm(x~p))$r.squared
    
    iteration_count <- iteration_count + 1
    setTxtProgressBar(progress, iteration_count)
  }
  close(progress)
  
  sensor_vars <- bind_rows( sensor_vars,
                            data.frame(
                                  sensor = sensors[s],
                                  lag = o$lag[o$acf_value == max(o$acf_value)],
                                  n = o$n[o$acf_value == max(o$acf_value)]  
                                  ))
  
}



# Create the ggplot
ggplot(o, aes(x = lag, y = acf_value, 
              color = n,
              group = n)) + 
  geom_point(size = .5) + 
  geom_line(alpha=.3) +
  theme_minimal() + 
  labs(title = "ACF Values for Different Lags and n",
       x = "Lag",
       y = "ACF Value",
       color = "n") 


ggplot(o, aes(x = lag, y = n, fill = acf_value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(min(o$acf_value), max(o$acf_value)), 
                       space = "Lab", name="ACF Value") +
  theme_minimal() + 
  labs(title = "Heatmap of ACF Values for Different Lags and n", 
       x = "Lag", 
       y = "n") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# First, create a summary data frame that contains the max ACF value for each 'n'
max_acf_per_n <- o %>%
  group_by(n) %>%
  summarise(max_acf = max(acf_value))

# Now create the ggplot
ggplot(max_acf_per_n, aes(x = as.numeric(as.character(n)), y = max_acf)) + 
  geom_line() + 
  geom_point(size = 2) +
  theme_minimal() + 
  labs(title = "Maximum ACF Value for Each n", 
       x = "n", 
       y = "Maximum ACF Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


save( sensor_vars, file = here::here("Inst","article",
                                     "backend","sensor_resid_corr.rdata"))
