flights_data = readRDS("C:/Users/danan/Desktop/Bitamin/BITA4_Machine-Learning-with-R/flights.RDS")
flights_data
library(dplyr)
library(ggplot2)
flights_data %>% 
  na.omit() %>% 
  mutate(delay = as.factor(ifelse(dep_delay>30,1,0))) %>% 
  arrange(dep_dt) %>%
  filter(month==1) %>% 
  ggplot(aes(x=dep_dt,y=dep_delay,
             shape= delay, col=delay)) +
  geom_point()+
  theme_bw()

install.packages("plotly")
library(plotly)
flights_data %>% 
  arrange(dep_dt) %>%
  filter(month==1) %>% 
  plot_ly(
    x = ~dep_dt,
    y = ~dep_delay,
    color = ~carrier,
    type="scatter"
  )
