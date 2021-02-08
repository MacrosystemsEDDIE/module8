library(tidyverse)

fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)

# metric forecast output, figure, bar graph
data <- data.frame(
  group=c('0-10%', '10-30%', '30-60%', '60-90%', '90-100%'),
  value=c(13,7,9,21,2)
)
# visualizing just the last horizon of the forecast
fcast <- fcast[15,]
fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
  gather(key = ensemble, value = forecast, ens_1:ens_25)

# calculate percent of ensembles within certaint groups
hist(fcast$forecast)
# low, medium, high
# 

ggplot(data = data, aes(group, value, fill = group)) +
  geom_bar(stat = 'identity') +
#  labs(title = input$figure_title, caption = input$figure_caption) +
  ylab('Number of Simulations') +
  xlab('% Likelihood of Algal Bloom') +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = NA, color = 'black'),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 15, hjust = 0))

# metric forecast output, figure, time series
#fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)

fcast$percent_over_35 <- NA

for (i in 2:nrow(fcast)) {
number <-   length(which(fcast[i,6:30] > 35))
  fcast$percent_over_35[i] <- number/25*100
}

ggplot()+
  geom_line(data = fcast, aes(date, percent_over_35), size = 2) +
  scale_y_continuous(breaks = seq(0, 100, 10))+
  ylab("% Likelihood of Algal Bloom") +
  xlab("Date") +
  #  labs(title = paste0("Time Series leading up to June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
  theme_classic(base_size = 24) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24),
        legend.position = 'none')


# raw forecast output, figure, bar graph (histogram)
# visualizing just the last horizon of the forecast
#fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)
fcast[,2:30] <- round(fcast[,2:30], digits = 2)
fcast <- fcast[,-c(31, 32, 33)]

# test stat calcs
summary(fcast[3,])
rowMeans(fcast[3,])
fcast_stats <- fcast[fcast$date == as.Date('2021-06-08'), ]
fcast_stats <- fcast_stats[,-1]
out_stat <- rowMeans(fcast_stats)
fcast_stats <- as.matrix(fcast_stats)
out_stat <- rowMins(fcast_stats)



# raw forecast output, figure, bar graph (histogram)
# visualizing just the last horizon of the forecast
fcast <- fcast[15,]
fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
  gather(key = ensemble, value = forecast, ens_1:ens_25)

info <- hist(fcast$forecast)

data <- data.frame(
  breaks = info$breaks[1:length(info$breaks)-1],
  counts = as.vector(info$counts)
)
data$breaks <- as.factor(data$breaks)

ggplot(data = data, aes(breaks, counts, fill = breaks)) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = 'Dark2', name = 'Frequency of Predicted Chl Concentration', 
                    label = c('0-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45', '45-50')) +
  ylab('Number of Ensembles') +
  xlab('Predicted Algal Concentration (ug/L)') +
  #labs(title = paste0("June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
  theme(
        panel.background = element_rect(fill = NA, color = 'black'),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 15, hjust = 0))

# raw forecast output, figure, pie chart
data$counts <- as.factor(data$counts)
data$breaks <- as.factor(data$breaks)
ggplot(data, aes(x="", y=counts, fill=breaks)) +
  scale_fill_brewer(palette = 'YlOrBr', name = 'Bins of Predicted Chla (ug/L)') +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
#  labs(title = input$figure_title, caption = input$figure_caption) +
  theme_void() # remove background, grid, numeric labels


# raw forecast output, number
fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)
fcast <- fcast[15,]

ggplot(data = fcast, aes(x = date, y = mean)) +
  geom_label(aes(label = paste0("The forecasted \n algal concentration is \n ", round(mean, 1), ' +/-', round(min, 1), ' ug/L'), x =date+ 0.5), size = 12) +
#  labs(title = input$figure_title, caption = input$figure_caption) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = NA, color = 'black'),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 15, hjust = 0))

# raw forecast output, figure, time series
data <- read.csv("data/wq_forecasts/mock_chl_obs.csv")
data$date <- as.Date(data$date)
fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)

# confidence intervals
ggplot()+
  geom_line(data = fcast, aes(date, mean)) +
  scale_y_continuous(breaks = seq(0, 100, 10))+
  xlim(min(fcast$date)-7, max(fcast$date)) +
  geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = cb_cols[2]), size = 4) +
  geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max), fill = cb_cols[2], alpha = 0.3) +
  geom_vline(xintercept = as.Date(min(fcast$date)), linetype = "dashed") +
  geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
  #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
#  labs(title = paste0("Time Series leading up to June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
  theme_classic(base_size = 24) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24),
        legend.position = 'none')

# ensemble lines


conditionalPanel("input.metric_raw=='raw forecast output' && input.raw_comm_type=='figure' && input.raw_plot_options=='time series",
                 radioButtons('ts_line_type', 'Select how you want to visualize the forecast ensembles',
                              choices = c('Line', 'Distribution')))
fcast <- read.csv("data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)
fcast <- fcast %>% select(-Past, - Future, -y)
fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
  gather(key = ensemble, value = forecast, ens_1:ens_25, -date)

raw_fig_ts_ens <- ggplot()+
  geom_line(data = fcast, aes(date, forecast, group = ensemble)) +
  scale_y_continuous(breaks = seq(0, 100, 10))+
  xlim(min(fcast$date)-7, max(fcast$date)) +
  geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = cb_cols[2]), size = 4) +
  geom_vline(xintercept = as.Date(min(fcast$date)), linetype = "dashed") +
  geom_vline(xintercept = as.Date(date_of_event), color = 'grey44', size = 2) +
  #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
  #  labs(title = paste0("Time Series leading up to June 18 Forecast \n", input$figure_title), caption = input$figure_caption) +
  theme_classic(base_size = 24) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24),
        legend.position = 'none')

# work on legend for decision figures
#fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)
fcast[,2:30] <- round(fcast[,2:30], digits = 2)
fcast <- fcast[,-c(31, 32, 33)]

#data <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/mock_chl_obs.csv")
data$date <- as.Date(data$date)

ggplot()+
  geom_line(data = fcast, aes(date, mean)) +
  scale_y_continuous(breaks = seq(0, 100, 10))+
  xlim(min(fcast$date)-7, max(fcast$date)) +
  geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = l.cols[6]), size = 4) +
  geom_vline(xintercept = as.numeric(min(fcast$date)), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 2) +
  #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
  theme_classic(base_size = 24) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24))


# final plot of all forecasts fo decision activity
setwd("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/")
date_of_event <- as.Date('2021-06-06')
data <- read.csv("./data/wq_forecasts/mock_chl_obs.csv")
data$date <- as.Date(data$date)
data <- data[data$date<date_of_event,]

fcast_14 <- read.csv("./data/wq_forecasts/forecast_day14.csv")
fcast_14$date <- as.Date(fcast_14$date)
fcast_10 <- read.csv("./data/wq_forecasts/forecast_day10.csv")
fcast_10$date <- as.Date(fcast_10$date)
fcast_10 <- fcast_10[fcast_10$date<=date_of_event+2,]
fcast_7 <- read.csv("./data/wq_forecasts/forecast_day7.csv")
fcast_7$date <- as.Date(fcast_7$date)
fcast_7 <- fcast_7[fcast_7$date<=date_of_event+2,]
fcast_2 <- read.csv("./data/wq_forecasts/forecast_day2.csv")
fcast_2$date <- as.Date(fcast_2$date)
fcast_2 <- fcast_2[fcast_2$date<=date_of_event+2,]
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]

ggplot() +
  xlim(min(data$date), date_of_event + 2) +
  geom_ribbon(data = fcast_14, aes(date, ymin = min, ymax = max, fill = "14-day"), alpha = 0.8) +
  geom_line(data = fcast_14, aes(date, mean, color = "14-day mean")) + #B2DF8A
  geom_ribbon(data = fcast_10, aes(date, ymin = min, ymax = max, fill = "10-day"), alpha = 0.7) +
  geom_line(data = fcast_10, aes(date, mean,  color = "10-day mean")) + #A6CEE3
  geom_ribbon(data = fcast_7, aes(date, ymin = min, ymax = max, fill = "7-day"), alpha = 0.7) +
  geom_line(data = fcast_7, aes(date, mean, color = "7-day mean")) + # 33A02C
  geom_ribbon(data = fcast_2, aes(date, ymin = min, ymax = max, fill = "2-day"), alpha = 0.6) +
  geom_line(data = fcast_2, aes(date, mean, color = "2-day mean")) + # FB9A99
  scale_y_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("14-day mean" = cols[1], "10-day mean" = cols[5], "7-day mean" = cols[3], "2-day mean" = cols[4], "Obs" = cols[6])) +
  scale_fill_manual(values = c("14-day" = cols[1], "10-day" = cols[5], "7-day" = cols[3], "2-day" = cols[4])) +
  geom_point(data = data, aes(date, obs_chl_ugl, color = "Obs"), size = 2.5) +
  geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 1.3) +
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
  theme_classic(base_size = 24) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16))

# decision plot
ggplot()+
  geom_line(data = fcast, aes(date, mean, color = "Forecast Mean")) +
  scale_y_continuous(breaks = seq(0, 100, 10))+
  xlim(min(fcast$date)-7, max(fcast$date)) +
  geom_point(data = data[data$date<=min(fcast$date),], aes(date, obs_chl_ugl, color = "Obs"), size = 4) +
  geom_vline(xintercept = as.numeric(min(fcast$date)), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(date_of_event), color = 'grey44', size = 1.2) +
  #geom_label(data = day14, aes(Past, y, label = 'Past'), size = 12) +
  scale_color_manual(name = "", values = c("Obs" = l.cols[2], 'Forecast Mean' = 'black'))+
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
  theme_classic(base_size = 24) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24))


# decision with and without uncertainty
data <- data.frame(
  day = c(as.Date('2021-05-23'), as.Date('2021-05-30'), as.Date('2021-06-04')),
  choice_noUC = NA,
  choice_withUC = NA,
  binary_noUC = NA,
  binary_withUC = NA)

data$choice_noUC <- c('Decision_Day14',
                      'Decision_Day7',
                      'Decision_Day2')
data$choice_withUC <- c('Decision_Day14_UC',
                        'Decision_Day7_UC',
                        'Decision_Day2_UC')

data$binary_noUC <- c(0, 1, 1)
data$binary_withUC <- c(0.9, 0.1, 0.9)

ggplot(data = data) +
  geom_point(aes(x = day, y = binary_noUC, color = "Without Uncertainty", position = 'jitter'), size = 4) +
  geom_point(aes(x = day, y = binary_withUC, color = "With Uncertainty", position = 'jitter'), size = 4) +
  scale_y_continuous(breaks = c(0, 1), labels = c('Cancel', 'Continue')) +
  ylab("Decision") +
  xlab("Date") +
  scale_x_date(breaks = c(as.Date('2021-05-23'), as.Date('2021-05-27'), as.Date('2021-05-30'), as.Date('2021-06-04')), date_labels = '%b %d') +
  scale_color_manual(name = "", values = c("Without Uncertainty" = cols[5], "With Uncertainty" = cols[3]))+
  theme_classic(base_size = 15) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text = element_text(size = 10),
        axis.text.y = element_text(angle = 90, hjust = 0.7))
