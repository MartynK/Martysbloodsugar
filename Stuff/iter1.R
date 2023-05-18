# Attempting to read the xdrip data
# Turns out everything was exported

library(lubridate)
library(dplyr)
library(ggplot2)

dat <- here::here( "Data", "Xdrip", "exportCSV20230518-182448.zip") |> 
       unzip() |>
       read.csv( sep = ";") |>
       mutate( TIME = hm(TIME),
               DAY = dmy(DAY),
               datetim = DAY + TIME,
               glucose = UDT_CGMS / 18 # mg/dl -> mmol/L
       ) |>
       dplyr::select( !( c( "CH_GR", "BOLUS")))


dat_out <- dat |>
  filter( is.na(UDT_CGMS) == FALSE)

dat_prick <-
  dat |>
    filter( is.na(BG_LEVEL) == FALSE) |>
    select( c("datetim", "BG_LEVEL")) |>
    group_by( datetim) |>
    mutate( BG_LEVEL = mean(BG_LEVEL)) |>
    slice(1) |>
    ungroup()

dat_comment <-
  dat |>
  filter( REMARK != "") |>
  select( c("datetim", "REMARK"))

# Sapply did something funny (returned list)
# Screw the shift, putting stuff next to the results

for (i in 1:nrow(dat_prick)) {
  diffs <- dat_prick$datetim[i] - dat_out$datetim
  
  if (abs(max(diffs[diffs<0])) > min(diffs[diffs>0])) {
    index_closest  <- which(diffs == min(diffs[diffs>0]))
  } else {
    index_closest  <- which(diffs == max(diffs[diffs<0]))
  }
  dat_out$BG_LEVEL[index_closest] <- dat_prick$BG_LEVEL[i]
}

for (i in 1:nrow(dat_comment)) {
  diffs <- dat_comment$datetim[i] - dat_out$datetim
  
  if (abs(max(diffs[diffs<0])) > min(diffs[diffs>0])) {
    index_closest  <- which(diffs == min(diffs[diffs>0]))
  } else {
    index_closest  <- which(diffs == max(diffs[diffs<0]))
  }
  dat_out$REMARK[index_closest] <- dat_comment$REMARK[i]
}

# making stuff around comments

dat_out <-
  dat_out |>
    mutate( remark_factor = NA,
            delta_time_remark = NA)

for (i in 1:nrow(dat_comment)) {
  dat_out$remark_factor[dat_out$datetim < dat_comment$datetim[i] + 3600 * 1.5 &
                        dat_out$datetim > dat_comment$datetim[i] - 3600 * 0.5] <- i
}


dat_out |>
  ggplot( aes( x = datetim, y = glucose)) +
    theme_minimal() +
    geom_point()

dat |>
  ggplot( aes( x = datetim, y = as.numeric(TIME), color = DAY)) +
  geom_point()

dat |>
  ggplot( aes( x = as.numeric(TIME), y = glucose, color = DAY)) +
    geom_point() +
    geom_smooth(color = "red")

dat_out |>
  ggplot( aes( x = datetim, y = glucose)) +
    geom_point() +
    facet_wrap( facets = "remark_factor", scales = "free")
  
