# Attempting to read a file from xdrip

library(lubridate)
library(dplyr)
library(ggplot2)

dat <- here::here( "Data", "Xdrip", "exportCSV20230518-182448.zip") |> 
       unzip() |>
       read.csv( sep = ";") |>
       mutate( TIME = hm(TIME),
               DAY = dmy(DAY),
               datetim = DAY + TIME
       )


dat |>
  ggplot( aes( x = datetim, y = UDT_CGMS)) +
    theme_minimal() +
    geom_point()