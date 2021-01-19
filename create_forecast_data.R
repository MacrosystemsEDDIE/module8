# create fake forecast data

library(matrixStats)
library(tidyverse)

set.seed(1234)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# define time frames
tpast <- as.Date(Sys.time() - 6*60*60*24)
t0 <- as.Date(Sys.time())
t1 <- as.Date(Sys.time() + 14*60*60*24)


obs <- data.frame(
  date = seq.Date(tpast, t0, by = "1 day"),
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
fcast$date <- seq.Date(t0, t1, by = "1 day")
fcast$mean <- mean
fcast$min <- min
fcast$max <- max
fcast[1,1:28] <- obs$chl_ugl[7]

fcast <- fcast %>% 
  select(date, mean, min, max, everything()) %>% 
  mutate(min = ifelse(min < 0, 0, min))

lab_df <- data.frame(
  date = c(as.Date(Sys.time() - 3*60*60*24) ,as.Date(Sys.time() + 4*60*60*24) ),
  y = 55,
  labs = c("Past", "Future")
)
date_of_event <- as.Date(Sys.time() + 14*60*60*24) 
cb_cols <- RColorBrewer::brewer.pal(12, "Paired")

ggplot()+
  geom_line(data = fcast, aes(date, mean)) +
  #scale_y_continuous(breaks = seq(0, 100, 10))+
  #ylim(0,60) +
  xlim(tpast, as.Date('2021-02-02'))+
  geom_point(data = obs, aes(date, chl_ugl, color = cb_cols[6]), size = 4) +
  geom_ribbon(data = fcast, aes(date, ymin = min, ymax = max), fill = cb_cols[6], alpha = 0.3) +
  geom_vline(xintercept = as.Date('2021-01-19'), linetype = "dashed") +
  geom_vline(xintercept = as.Date(date_of_event), color = 'red') +
  geom_label(data = lab_df, aes(date, y, label = labs), size = 12) +
  ylab("Chlorophyll-a (ug/L)") +
  xlab("Date") +
  theme_classic(base_size = 26) +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text.x = element_text(size = 24),
        legend.position = 'none')

write.csv(fcast, "C:/Users/wwoel/Desktop/Project-EDDIE-shiny/module8/data/wq_forecasts/forecast_day16.csv")



  
  
obs_new <- obs[2:7,]
  
  

