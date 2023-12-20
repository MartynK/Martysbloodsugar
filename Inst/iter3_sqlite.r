#install.packages("RSQLite")
library(RSQLite)
library(lubridate)
library(ggplot2)
library(dplyr)
library(splines)
library(fuzzyjoin)

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


d_bg %>%
  filter( is.na(sensor) == FALSE,
          bad == 0) %>%
  ggplot(aes(x=bg,y=bg_cbgm)) +
    theme_bw() +
    geom_abline(slope=1,intercept =0, color="red") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth( se = FALSE, color = "deepskyblue", 
                 method = "loess", span = 1.25) +
    facet_wrap(facets="sensor") +
    labs( title = "Calibration points per sensor",
          y = "CGM value",
          x = "Stick value")



mod <- lm( bg_cbgm ~ bg * sensor 
           #weights = ,
           ,d_bg %>% filter(bad==0))

summary(mod)
anova(mod)
mod %>% effects::predictorEffects() %>% plot

data <- 
  data %>%
    mutate( value3 = ifelse(sensor == 1,
                            #(raw_data / 18.016) 
                            value2 * mod$coefficients[2] 
                            + mod$coefficients[1],
                            calculated_value / 18.016))

data %>%
  ggplot(aes(x=value2,y=value3)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Calibrated value from app",
       y = "Calibrated value based on model")



plots <- list()
dayz  <- unique(as.Date(data$time))
for ( i in 1:length(dayz)) {
  tim_act <- as_datetime(dayz[i])

  tim_lim <- c(tim_act, tim_act + 3600 * 24)
  
  plots[[as.character(tim_lim[1])]] <-
    data %>%
      ggplot(aes(x=time,y=value2)) +
       theme_bw() +
       geom_line() +
       geom_point() +
       geom_point(data = d_bg, mapping = aes( y = bg), color = "red") +
       geom_point(data = d_bg, mapping = aes( y = bg_cbgm), color = "green") +
       scale_x_datetime(limits = tim_lim)

}
plots <- plots[order(names(plots))]

do.call(ggpubr::ggarrange, c(plots, list( ncol = 6,nrow = 3)))




data %>%
  ggplot(aes(x=time_of_day,y=value3)) +
    theme_bw() +
    geom_line(alpha=.2) +
    geom_point() +
    geom_smooth() +
    geom_smooth(method = "loess",span=.1,color="green") +
    geom_point(data = d_bg, mapping = aes( y = bg), color = "red")
    

data %>%
  ggplot(aes(x=time,y=value3)) +
    theme_bw() +
    geom_point() +
    geom_line() +
    facet_wrap(facets="sensor",scales = "free_x")


# data %>%
#   filter(sensor==1) %>%
#   ggplot(aes(x = age_adjusted_raw_value/18.016#calculated_value / 18.016
#              ,y= value3 
#              ,color=time)) +
#     geom_point()


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

mod2 <- kmeans(dat_mvt2[,2:4],centers = 3)

dat_mvt2$clust <- mod2$cluster

# 3D scatter plot
scatterplot3d::scatterplot3d(dat_mvt2[,2], dat_mvt2[,3], dat_mvt2[,4], 
              color = dat_mvt2$clust, pch = 19)

##############
# Periodogram stuff

periodogr <- 
  data.frame( time =
                seq(
                  min(data_w_notes$time),
                  max(data_w_notes$time),
                  by = 60
                ))
periodogr$val <- f(periodogr$time)
periodogr_ts <- ts(periodogr)

periodogram_b <-
  spec.pgram(periodogr_ts, 
             spans = c(7,9),
             taper = 0.005,
             #fast = FALSE
             #demean = FALSE,
             #detrend = FALSE
  )

data.frame( freq = periodogram_b$freq
            ,spec = periodogram_b$spec[,1]
) %>%
  ggplot(aes( x = 1/freq, y = spec)) +
  theme_bw() +
  scale_x_log10(limits = c(2,1500),
                breaks = c(10,20,30,60,120,240,1000,1440)) +
  scale_y_log10() +
  geom_line() +
  geom_vline(xintercept = 20, color = "salmon")



#############
# Model stuff

fourier20 <- 
  rbind(
    ts(data_w_notes$time, 
       data_w_notes$value3, 
       frequency = 20) %>% 
         forecast::fourier(K=1),
    ts(data_w_notes$time, 
       data_w_notes$value3, 
       frequency = 20) %>% 
      forecast::fourier(K=1,h=8))


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
          fourier20 = fourier20
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


# data_w_notes %>%
#   mutate(id = 1:n()) %>%
#   ggplot(aes(x = time_note, y = note_time_scaled, color = time_note_max)) +
#     geom_line()

# hist(data_w_notes$note_time_scaled,breaks = 80)

data_w_notes <- data_w_notes[c(-3604,-4626)]

mod_notes <- lm(value3 ~ notes : bs( note_time_scaled, 
                                     Boundary.knots = c(0,1),
                                     knots = seq(0,1,length.out=7)[2:6])
                + notes_category
                + ns( tod, df = 4)
                #+ fourier20[,1]
                , data_w_notes)
summary(mod_notes)
anova(mod_notes)
cooks.distance(mod_notes) %>% sort(decreasing = TRUE) %>% head(n = 10)
plot(acf(mod_notes$residuals,lag = 100))
plot(pacf(mod_notes$residuals))

data_w_notes$resid <- mod_notes$residuals
data_w_notes$pr    <- predict(mod_notes)
data_w_notes$pr_std <- predict(mod_notes, 
                               newdata = data_w_notes %>% 
                                 mutate(tod = 1))


pr <- expand.grid( notes = unique(data_w_notes$notes),
                   time_note = seq(0,43200,length.out = 120),
                   tod = seq(1,3600*24,length.out = 10))

pr <- 
  left_join( pr, 
             notes_max_time %>% 
               group_by(notes) %>%
               arrange(desc(time_note)) %>%
               slice(1) %>%
               rename( time_note_max = "time_note"), 
             by= c("notes")) %>%
  mutate(note_time_scaled = time_note / time_note_max) %>%
  filter(time_note <= time_note_max ) %>%
  mutate(bin = cut(time_note_max, 
                   breaks = 6, 
                   #labels = c("Bin1", "Bin2", "Bin3"),
                   include.lowest = TRUE)) %>% 
  filter(notes != "none") %>% 
  mutate(notes=forcats::fct_drop(notes)) %>%
  left_join( y = data_w_notes %>% select(notes,notes_category) %>%
               group_by(notes) %>% slice(1),
             by = "notes")

pr$pr. <- predict(mod_notes, newdata = pr)

pr %>%
  ggplot(aes(x = pr.)) +
  theme_bw() +
  geom_histogram(aes(y = ..density..), alpha = .5, fill = "salmon4", 
                 color = "red", binwidth = .1) +
  scale_x_continuous(limits = c(0, 15)) +
  geom_histogram(data = data_w_notes, 
                 mapping = aes(x = value3, y = ..density..),
                 fill = "deepskyblue", 
                 color = "deepskyblue3", 
                 alpha = .5, binwidth = .1) +
  labs( x = "",
        title = "Histograms of observed (blue) / predicted (red) values")


pr <- pr %>%
  filter( pr > 0,
          pr < 15)

pr %>%
  filter(time_note == 0) %>%
  ggplot(aes(x = hms::hms(tod), y = pr., color = notes_category)) +
    theme_bw() +
    geom_point() +
    geom_line() +
    labs( x = "Time of day",
          y = "Predicted value")


i <- 1
j <- i+9
notes_act <- unique(sort(pr$notes[pr$notes_category == "food"]))[i:j]
pr %>%
  filter(tod == 1) %>%
  filter(notes_category == "food") %>%
  filter( notes %in% notes_act) %>%
  ggplot( aes( x = time_note/3600, y = pr., 
               color = notes
               #, group = notes
               )) +
    theme_bw() +
    geom_line() +
  geom_point(data = data_w_notes %>% 
               filter( notes %in% notes_act), 
             mapping = aes(x = time_note/3600, 
                           y = pr_std + resid),
             alpha = .75,
             size = .75) +
  geom_hline(yintercept = 4.9, color = "salmon4") +
  facet_wrap(facets  = vars(notes),
             nrow = 2,
             ncol = 5,
             scale = "free_x") +
  labs(x = "Time since meal (hours)",
       y = "Predicted values")

