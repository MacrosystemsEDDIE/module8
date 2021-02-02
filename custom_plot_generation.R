library(tidyverse)

fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
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
fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
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
fcast <- fcast[15,]
fcast <- fcast %>% select(date, ens_1:ens_25) %>% 
  gather(key = ensemble, value = forecast, ens_1:ens_25)

info <- hist(fcast$forecast)

data <- data.frame(
  breaks = info$breaks[1:length(info$breaks)-1],
  counts = as.vector(info$counts)
)
ggplot(data = data, aes(breaks, counts, fill = breaks)) +
  geom_bar(stat = 'identity') +
  #labs(title = input$figure_title, caption = input$figure_caption) +
  scale_x_continuous(breaks = c(0,15, 20, 25, 30, 35, 40, 45, 50)) +
  ylab('Number of Ensembles') +
  xlab('Predicted Algal Concentration (ug/L)') +
  theme(legend.position = 'none',
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
fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
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
data <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/mock_chl_obs.csv")
data$date <- as.Date(data$date)
fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
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

,
conditionalPanel("input.metric_raw=='raw forecast output' && input.raw_comm_type=='figure' && input.raw_plot_options=='time series",
                 radioButtons('ts_line_type', 'Select how you want to visualize the forecast ensembles',
                              choices = c('Line', 'Distribution')))
fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
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

