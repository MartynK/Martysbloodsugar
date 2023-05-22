# Attempting to read and wrangle the xdrip data
# Turns out everything was exported
# NOTE: THE DATA IS MISCALIBRATED!!!

# loading libraries
library(lubridate)
library(dplyr)
library(ggplot2)

# Reading and transforming the data
dat <- here::here( "Data", "Xdrip", "exportCSV20230518-182448.zip") |> 
  unzip() |>
  read.csv( sep = ";") |>
  mutate( TIME = hm(TIME),
          DAY = dmy(DAY),
          datetim = DAY + TIME,
          glucose = UDT_CGMS / 18 # mg/dl -> mmol/L
  ) |>
  dplyr::select( !( c( "CH_GR", "BOLUS")))

# dat_out will be the "real" transformed dataframe
dat_out <- dat |>
  filter( is.na(UDT_CGMS) == FALSE)

# Getting data where capillary glucose was measured (averaging where needed)
dat_prick <-
  dat |>
  filter( is.na(BG_LEVEL) == FALSE) |>
  select( c("datetim", "BG_LEVEL")) |>
  group_by( datetim) |>
  mutate( BG_LEVEL = mean(BG_LEVEL)) |>
  slice(1) |>
  ungroup()

# Getting comments
dat_comment <-
  dat |>
  filter( REMARK != "") |>
  select( c("datetim", "REMARK"))

# Putting it together
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

# making "episodes" around comments
dat_out <-
  dat_out |>
  mutate( remark_factor = NA,
          delta_time_remark = NA)

for (i in 1:nrow(dat_comment)) {
  indexs_involved <- 
    which( dat_out$datetim < dat_comment$datetim[i] + 3600 * 1.5 &
           dat_out$datetim > dat_comment$datetim[i] - 3600 * 0.5)
  
  dat_out$remark_factor[indexs_involved]     <- i
  dat_out$delta_time_remark[indexs_involved] <- 
    difftime( dat_out$datetim[indexs_involved],
              dat_comment$datetim[i], 
              units = "mins")
  
}

# Putting the remark's information in the factor
dat_out$remark_factor <- factor(dat_out$remark_factor,labels = dat_comment$REMARK)

# Loading/converting the sleep data
dat_sleep <- here::here("Data","Xdrip","Sleep_times.xlsx") |>
               openxlsx::read.xlsx(startRow = 2, 
                                   detectDates = TRUE) |>
               mutate( Fell_asleep = as_datetime(Date + Fell_asleep),
                       Woken_up    = as_datetime(Date + Woken_up + 1))
# dat_out |>
#   mutate( state = if(datetim < dat_sleep$Fell_asleep[] ))


# Exporting the transformed data
openxlsx::write.xlsx(dat_out, 
                     file = here::here("Export",
                                       "transformed_data.xlsx"))
save(dat_out, file = here::here("Export",
                                "xdrip_data.Rdata"))
