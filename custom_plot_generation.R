

fcast <- read.csv("C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day2.csv")
fcast$date <- as.Date(fcast$date)

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
  labs(title = input$figure_title, caption = input$figure_caption) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = NA, color = 'black'),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 15, hjust = 0))
