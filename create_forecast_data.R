# create fake forecast data

library(matrixStats)
library(tidyverse)

set.seed(1234)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# define time frames
date_of_event <- as.Date('2021-06-06') 

tstart <- as.Date('2021-05-23')
tpast <- as.Date(tstart - 6)
tend <- as.Date(tstart + 14)


obs <- data.frame(
  date = seq.Date(tpast, tstart, by = "1 day"),
  chl_ugl = rnorm(7, 22, 2.2)
)

n_members <- 25

fcast <-  matrix(NA, nrow = 14+1, # 14 days of forecasts, one day with the initial obs
         ncol = n_members)

mean_chl <- c(obs$chl_ugl[7], 22, 25, 23, 24, 24, 28, 32, 31, 29, 28, 28, 25, 26, 26)
sd_chl <- 2.2
for (j in 2:nrow(fcast)) {
  fcast[j,] <- rnorm(n_members, mean = mean_chl[j], sd = sd_chl )
sd_chl <- sd_chl + 0.7
  }

mean <- rowMeans(fcast)
min <- rowMins(fcast)
max <- rowMaxs(fcast)
fcast <- as.data.frame(fcast)


for (i in 1:ncol(fcast)) {
  colnames(fcast)[i] <- paste0('ens_', i)
  fcast[,i] <- as.numeric(fcast[,i])
}
fcast$mean <- mean
fcast$min <- min
fcast$max <- max
fcast[1,1:28] <- obs$chl_ugl[7]
fcast$date <- seq.Date(tstart, tend, by = "1 day")

fcast <- fcast %>% 
  select(date, mean, min, max, everything()) %>% 
  mutate(min = ifelse(min < 0, 0, min))

lab_df <- data.frame(
  date = c(as.Date(tstart - 3) ,as.Date(tstart + 4) ),
  y = 55,
  labs = c("Past", "Future")
)
cb_cols <- RColorBrewer::brewer.pal(12, "Paired")

ggplot()+
  geom_line(data = fcast, aes(date, mean)) +
  scale_y_continuous(breaks = seq(0, 100, 10))+
  #ylim(0,60) +
  #xlim(tpast, as.Date('2021-02-02'))+
  geom_point(data = obs, aes(date, chl_ugl, color = cb_cols[6]), size = 4) +
  geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max), fill = cb_cols[6], alpha = 0.3) +
  geom_vline(xintercept = as.Date(tstart), linetype = "dashed") +
  geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
  geom_label(data = lab_df, aes(date, y, label = labs), size = 12) +
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
  theme_classic(base_size = 26) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24),
        legend.position = 'none')

#write.csv(fcast, "C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day16.csv", row.names = FALSE)

# now iterate through a list of forecast days, moving the observation window and creating a new forecast

forecast_dates <- seq.Date(tstart, tend, by = "1 day")


data <- data.frame(
  date = seq.Date(tpast, tend+14, by = "1 day"),
  obs_chl_ugl = NA,
  fcast_chl_ugl = NA
)

# create 'observations' for the entire forecast period
data[1:7, 2] <-   rnorm(7, 22, 2.2)
data[8:12, 2] <-  rnorm(5, 26, 4.2)
data[13:16, 2] <- rnorm(4, 33, 3.3)
data[17:23, 2] <- rnorm(7, 31, 3.6)
data[24:30, 2] <- rnorm(7, 23, 2.6)
data[31:35, 2] <- rnorm(5, 25, 3.1)

# create a 'mean forecast' for the entire forecast period
data[1:7, 3] <-   rnorm(7, 24, 3.2)
data[8:12, 3] <-  rnorm(5, 27, 5.2)
data[13:16, 3] <- rnorm(4, 35, 3.3)
data[17:23, 3] <- rnorm(7, 30, 3.8)
data[24:30, 3] <- rnorm(7, 28, 3.6)
data[31:35, 3] <- rnorm(5, 23, 3.1)


plot(data$date, data$obs_chl_ugl, type = 'l', ylim = c(0, 40))
points(data$date, data$fcast_chl_ugl, type = 'l', col = 'red')


for (i in 1:length(forecast_dates)) {
  obs <- data[data$date<=forecast_dates[i]+14,] # subset obs to the forecast date
  sd_chl <- 2.2
  fcast <-  as.data.frame(
    matrix(NA, nrow = 14+1, # 14 days of forecasts, one day with the initial obs
           ncol = n_members))
  for (m in 1:ncol(fcast)) {
    colnames(fcast)[m] <- paste0('ens_', m)
    fcast[,m] <- as.numeric(fcast[,m])
  }
  for (j in 2:nrow(fcast)) {
    fcast[j,] <- rnorm(n_members, mean = obs$fcast_chl_ugl[j+6], sd = sd_chl )
    sd_chl <- sd_chl + 0.7
    
  }
  mean <- rowMeans(fcast)
  min <- rowMins(as.matrix(fcast))
  max <- rowMaxs(as.matrix(fcast))
  fcast <- as.data.frame(fcast)
   
    fcast$mean <- mean
    fcast$min <- min
    fcast$max <- max
    fcast[1,] <- obs$obs_chl_ugl[i+6]
    fcast$date <- seq.Date(forecast_dates[i], forecast_dates[i]+14, by = "1 day") #seq.Date(tstart, tend, by = "1 day")
    fcast$Past <- forecast_dates[i] - 3
    fcast$Future <- forecast_dates[i] + 4
    fcast$y <- 55
    fcast <- left_join(fcast, data)
    fcast <- fcast %>% 
      select(date, obs_chl_ugl, mean, min, max, everything(), -fcast_chl_ugl) %>% 
      mutate(min = ifelse(min < 0, 0, min))
    

    write.csv(fcast, paste0("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day", 15-i, ".csv"), row.names = FALSE)
    
    
}

write.csv(data, paste0("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/mock_chl_obs.csv"), row.names = FALSE)

# read in the files and make the plots to make sure the observations are assimilated appropriately 
fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day0.csv")
fcast$date <- as.Date(fcast$date)

data <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/mock_chl_obs.csv")
data$date <- as.Date(data$date)
ggplot()+
  geom_line(data = fcast, aes(date, mean)) +
  scale_y_continuous(breaks = seq(0, 100, 10))+
  #ylim(0,60) +
  xlim(min(fcast$date)-7, max(fcast$date)) +
  geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = cb_cols[6]), size = 4) +
  geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max), fill = cb_cols[6], alpha = 0.3) +
  geom_vline(xintercept = as.Date(min(fcast$date)), linetype = "dashed") +
  geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
  #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
  theme_classic(base_size = 26) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24),
        legend.position = 'none')
