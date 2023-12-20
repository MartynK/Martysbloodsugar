# Data wrangling
unreliable_series <-
  here::here("Inst","extdata","unreliable_interval.xlsx") %>%
  readxl::read_excel()

con <- dbConnect(RSQLite::SQLite(), 
                 dbname = here::here("inst","extdata",
                                     "Xdrip","export20231215-220504.sqlite"))
dbListTables(con)


data <- dbReadTable(con, "BgReadings") %>%
  mutate(time = as_datetime(timestamp/1000),
         time_of_day = hms::as_hms(format(time, "%H:%M:%S")),
         value = calculated_value / 18.016,
         value2 = ifelse(sensor == 1,
                         raw_data / 18.016,
                         calculated_value / 18.016)) %>%
  filter(value != 0)

for (i in 1:nrow(unreliable_series)) {
  data <- data %>%
    filter(time < unreliable_series$unreliable_start[i] |
             time > unreliable_series$unreliable_end[i]  
    )
}



f <- approxfun(data$time,data$value2)
tf <- approxfun(data$time, data$time_since_sensor_started / 1000 / 3600)
sf <- approxfun(data$time, data$sensor)

d_stick <- dbReadTable(con, "BloodTest") %>%
  mutate(time = as_datetime(timestamp/1000))

d_calib <- dbReadTable(con, "Calibration") %>%
  mutate(time = as_datetime(timestamp/1000))

d_stick %>%
  ggplot(aes(x=time)) +
  geom_dotplot(alpha= 0.25,color="red",binwidth = 1) +
  geom_dotplot(data=d_calib,color="blue",alpha = .25,binwidth = 1)

d_bg <- 
  bind_rows(
    d_calib %>%
      dplyr::select(raw_timestamp,bg) %>%
      `colnames<-`(c("time","bg")),
    d_stick %>%
      dplyr::select(created_timestamp,mgdl) %>%
      `colnames<-`(c("time","bg"))
  ) %>%
  distinct() %>%
  mutate(time = as_datetime(time/1000),
         time_of_day = hms::as_hms(time),
         bg = bg / 18.016,
         bg_cbgm = f(time),
         diff = abs(bg - bg_cbgm),
         bad = 0,
         time_since_sensor_started = tf(time),
         sensor = round(sf(time)) - 1) %>%
  arrange(desc(diff))

for (i in 1:nrow(unreliable_series)) {
  d_bg <- d_bg %>%
    mutate( bad = ifelse(
      time > unreliable_series$unreliable_start[i] &
        time < unreliable_series$unreliable_end[i] ,
      1,
      bad))
}

# Calibrating

mod <- lm( bg_cbgm ~ bg * sensor 
           #weights = ,
           ,d_bg %>% filter(bad==0))

data <- 
  data %>%
  mutate( value3 = ifelse(sensor == 1,
                          #(raw_data / 18.016) 
                          value2 * mod$coefficients[2] 
                          + mod$coefficients[1],
                          calculated_value / 18.016))


d_notes <- dbReadTable(con, "Treatments") %>%
  mutate(time = as_datetime(timestamp/1000)) %>%
  arrange(time)

d_notes %>%
  dplyr::select(time, notes) %>%
  writexl::write_xlsx(path = here::here("Inst","extdata",
                                        "factor_recode.xlsx"))

# Do stuff outside in the excel (recode)

d_notes_in <- 
  readxl::read_xlsx(path = here::here("Inst","extdata",
                                      "factor_recode_in.xlsx")) %>%
  mutate(notes_new      = factor(ifelse(notes_new=="NA",NA,notes_new)),
         notes_category = factor(ifelse(notes_category=="NA",NA,notes_category)),
  )


######
# MVT stuff

dat_mvt <- data.frame(
  note = d_notes$notes,
  gluc0 = NA,
  gluc45 = NA,
  gluc60 = NA,
  gluc90 = NA
)
dayz  <- unique(d_notes$time)
for ( i in 1:length(dayz)) {
  tim_act <- as_datetime(dayz[i])
  
  if ( i < length(dayz)) {
    tim_lim <- min( as_datetime(dayz[i+1]), tim_act + 3600 * 2 )
  } else {
    tim_lim <- 3600 * 2
  }
  
  tim_lim <- c(tim_act - 1800, tim_lim)
  
  tim_diff <- as.numeric(difftime(tim_lim[2], tim_act,units="secs"))
  dat_mvt[i,2]<- f(dayz[i])
  if (tim_diff >= 90*60) {
    dat_mvt$gluc90[i]<- f(dayz[i] + 90*60)
  } 
  if (tim_diff >= 60*60) {
    dat_mvt$gluc60[i]<- f(dayz[i] + 60*60)
  } 
  if (tim_diff >= 45*60) {
    dat_mvt$gluc45[i]<- f(dayz[i] + 45*60)
  }
}

dat_mvt2 <- dat_mvt[complete.cases(dat_mvt),]
dat_mvt2 <- dat_mvt2[,2:5]  - dat_mvt2[,2]
dat_mvt2 <- cbind(   dat_mvt[complete.cases(dat_mvt),1], dat_mvt2[,2:4])


#############
# Model stuff

data_w_notes <- 
  difference_left_join(data, 
                       d_notes_in %>% 
                         filter(notes_category != "sensation") %>%
                         select(time, notes_new,
                                notes_category) %>%
                         dplyr::rename(time_note = "time",
                                       notes = "notes_new"),
                       by = c("time" = "time_note"),
                       max_dist = 5*60) %>% # 7 minutes in seconds
  arrange(time) %>%
  tidyr::fill(notes, time_note, notes_category) %>%
  mutate( time_old = time_note,
          time_note = as.numeric(time - time_note),
          time_note = ifelse( is.na(time_note) == TRUE,0,time_note),
          notes = ifelse( time_note > 43200 |
                            time_note < 0 
                          , "none", as.character(notes)),
          notes = ifelse( is.na(notes) == TRUE, "none", notes),
          notes_category = ifelse( is.na(notes_category) == TRUE, 
                                   "none", as.character(notes_category))
  ) %>%
  mutate(
    time_note = ifelse( notes == "none" , 1,time_note),
    tod = as.numeric(time_of_day),
    #fourier20 = fourier20
  )

notes_max_time <- 
  data_w_notes %>%
  group_by(notes, time_old) %>%
  arrange(desc(time_note)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(notes,time_note,time_old)

data_w_notes <- 
  left_join( data_w_notes, 
             notes_max_time %>% 
               rename( time_note_max = "time_note"), 
             by= c("notes", "time_old")) %>%
  mutate( time_note = ifelse( notes == "none" , 0,time_note),
          note_time_scaled = time_note / time_note_max)


fourier20 <- 
  ts(data_w_notes$time, 
     data_w_notes$value3, 
     frequency = 20) %>% 
  forecast::fourier(K=1)

#nrow(fourier20) - nrow(data_w_notes)

