# Plotting xdrip data

# loading libraries
library(lubridate)
library(dplyr)
library(ggplot2)


load(here::here("Export","xdrip_data.Rdata"))

# Overall results
dat_out |>
  ggplot( aes( x = datetim, y = glucose)) +
  theme_minimal() +
  geom_point()

# Days/times visualization
dat_out |>
  ggplot( aes( x = datetim, y = as.numeric(TIME), color = DAY)) +
  geom_point()

# "Daily trends"
dat_out |>
  ggplot( aes( x = (as.numeric(TIME)/3600), y = glucose, color = DAY)) +
  geom_point() +
  geom_smooth(color = "red", se = FALSE) +
  geom_smooth(color = "pink", method = "loess", span = .1, se = FALSE)
  

dat_out |>
  filter( is.na(remark_factor) == FALSE) |>
  ggplot( aes( x = datetim, y = glucose, group = remark_factor)) +
  theme_minimal() +
  geom_point() +
  geom_line() +
  geom_vline(data = dat_out |> 
               group_by(remark_factor) |> 
               slice(1),
             mapping = aes( xintercept = datetim - minutes(delta_time_remark )),
             color = "red"
  ) +
  facet_wrap( facets = "remark_factor", scales = "free")

