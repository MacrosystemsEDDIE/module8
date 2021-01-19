# code for old forecast plot (time series using FLARE temp forecasts)

forecast_data <- eventReactive(input$forecast_day, {
  forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
  forecast_file <- file.path("data", "wq_forecasts", forecast_dates[forecast_id,3])
  nc <- nc_open(forecast_file)
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
  full_time_local <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
  full_time_day_local <- as_date(full_time_local)
  temp_mean <- ncvar_get(nc,'temp_mean') # rows are days, columns are depths
  temp_mean_1.6 <- temp_mean[,6]
  temp <- ncvar_get(nc,'temp')
  temp_upper <- ncvar_get(nc,'temp_upperCI')
  temp_lower  <- ncvar_get(nc,'temp_lowerCI')
  temp_upper_1.6 <- temp_upper[,6]
  temp_lower_1.6  <- temp_lower[,6]
  obs <- ncvar_get(nc, 'obs')
  obs_1.6 <- obs[,6]
  nc_close(nc)
  
  forecast <- data.frame('date' = full_time_day_local, 'temp_mean' = temp_mean_1.6, 'upper_CI' = temp_upper_1.6, 'lower_CI' = temp_lower_1.6, 
                         'obs' = obs_1.6, 'mean_post_sulfate' = temp_mean_1.6*0.1, 'upper_CI_post_sulfate' = temp_upper_1.6*0.1, 'lower_CI_post_sulfate' = temp_lower_1.6*0.1)
  forecast[forecast$date>=forecast_dates[forecast_id+1,2],5] <- 'NA'
  forecast$obs <- as.numeric(forecast$obs)
  return(forecast)
})

forecast_ens_data <- eventReactive(input$forecast_day,{
  forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
  forecast_file <- file.path("data", "wq_forecasts", forecast_dates[forecast_id,3])
  nc <- nc_open(forecast_file)
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
  full_time_local <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
  full_time_day_local <- as_date(full_time_local)
  temp <- ncvar_get(nc,'temp')
  temp_1.6 <- temp[,6]
  obs <- ncvar_get(nc, 'obs')
  obs_1.6 <- obs[,6]
  nc_close(nc)
  
  #finish up by making a dataframe with the forecast ensembles by day
})

#  forecast_plot <- eventReactive(input$forecast_day, {
#    forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
#    p <- ggplot(data = forecast_data(), aes(x = date, y = temp_mean)) + 
#      geom_line() +
#      geom_ribbon(aes(date, ymin = lower_CI, ymax = upper_CI, fill = '95th', alpha = 0.4)) +
#      geom_vline(xintercept = as.Date(forecast_dates[forecast_id,2])) +
#      geom_text(aes(as.Date(forecast_dates[forecast_id,2])-1, y = 27.5), label = 'past') +
#      geom_text(aes(as.Date(forecast_dates[forecast_id,2])+1, y = 27.5), label = 'future') +
#      geom_vline(xintercept = as.Date(date_of_event), color = 'red') +
#      geom_text(aes(as.Date(date_of_event)-1.1, y = 27.5), color = 'red', label = 'Day of Event') +
#      ylab('Chlorophyll-a (µg/L)') + 
#      xlab("Date") +
#      theme(panel.grid.major = element_blank(),
#            legend.position = 'none') 
#    if(input$show_obs){
#      p <- p + geom_point(aes(x = date, y = obs, color = 'red'), na.rm = TRUE) 
#
#    }
#    if(!is.na(input$add_threshold)){
#      p <- p + geom_hline(yintercept = input$add_threshold)
#    }
#    return(p)
#  })
#  

output$forecast_plot <- renderPlotly({ 
  forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
  print(str(as.Date(forecast_dates[forecast_id, 2])))
  print(str(forecast_data()))
  print(str(forecast_dates))
  p <- ggplot(data = forecast_data(), aes(x = as.Date(date), y = temp_mean)) + 
    geom_line() +
    geom_ribbon(aes(date, ymin = lower_CI, ymax = upper_CI, fill = '95th', alpha = 0.4)) +
    geom_vline(xintercept = as.Date(forecast_dates[forecast_id, 2])) +
    geom_text(aes(as.Date(forecast_dates[forecast_id,2])-1, y = 27.5), label = 'past') +
    geom_text(aes(as.Date(forecast_dates[forecast_id,2])+1, y = 27.5), label = 'future') +
    geom_vline(xintercept = as.Date(date_of_event), color = 'red') +
    geom_text(aes(as.Date(date_of_event)-1.1, y = 27.5), color = 'red', label = 'Day of Event') +
    ylab('Chlorophyll-a (µg/L)') + 
    xlab("Date") +
    theme_minimal(base_size = 16) +
    theme(panel.background = element_rect(fill = NA, color = 'black'),
          panel.border = element_rect(color = 'black', fill = NA),
          legend.position = 'none')
  #theme(panel.grid.major = element_blank(),
  #      legend.position = 'none') 
  if(input$show_obs){
    p <- p + geom_point(aes(x = date, y = obs, color = 'red'), na.rm = TRUE) 
    
  }
  if(!is.na(input$add_threshold)){
    p <- p + geom_hline(yintercept = input$add_threshold)
  }
  return(ggplotly(p))
})

output$forecast_plot_updated <- renderPlotly({
  input$update_forecast
  forecast_id <- which(forecast_dates$day_in_future == input$forecast_day)
  j <- ggplot(data = forecast_data(), aes(x = as.Date(date), y = temp_mean)) + 
    geom_line() +
    geom_ribbon(aes(date, ymin = lower_CI, ymax = upper_CI, fill = '95th', alpha = 0.4)) +
    geom_line(aes(x = as.Date(date), y = mean_post_sulfate)) +
    geom_ribbon(aes(x = date, ymin = lower_CI_post_sulfate, ymax = upper_CI_post_sulfate, alpha = 0.4)) +
    geom_vline(xintercept = as.Date(forecast_dates[forecast_id, 2])) +
    geom_text(aes(as.Date(forecast_dates[forecast_id,2])-1, y = 27.5), label = 'past') +
    geom_text(aes(as.Date(forecast_dates[forecast_id,2])+1, y = 27.5), label = 'future') +
    geom_vline(xintercept = as.Date(date_of_event), color = 'red') +
    geom_text(aes(as.Date(date_of_event)-1.1, y = 27.5), color = 'red', label = 'Day of Event') +
    ylab('Chlorophyll-a (µg/L)') + 
    xlab("Date") +
    theme_minimal(base_size = 16) +
    theme(panel.background = element_rect(fill = NA, color = 'black'),
          panel.border = element_rect(color = 'black', fill = NA),
          legend.position = 'none')
  return(ggplotly(j))
  
  
})


p <- ggplot(data = mock_data, aes(date_of_forecast[16], forecast_ugL[16])) +
  geom_point(size = 5) +
  ylim(0, 50) +
  ggtitle(paste0('Forecasted Microcystin for 2020-07-19 made on ', mock_data$date_forecast_made[16]))+
  xlim((mock_data$date_of_forecast[16]-1), (mock_data$date_of_forecast[16]+1)) +
  ylab('Forecasted Microsystin (ug/L)') +
  xlab('Forecast Date') +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = NA, color = 'black'),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15, hjust = 0.5))

