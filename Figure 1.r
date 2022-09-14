#load libraries
library(tidyverse)

fig1_data <- tibble(years = c(2011:2021), volumes=c(10.7, 13.3, 13.0, 15.3, 15.5, 15.0, 15.2, 15.5, 14.9, 20, 24.0))

fig1 <- fig1_data %>% 
  ggplot(mapping = aes(x = years, y = volumes)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  geom_line(size = 2, alpha = 0.8) +
  cowplot::theme_minimal_vgrid(font_size = 12) +
  geom_point() +
  labs(title="Retail Share of US Equities Trading Volume",
       subtitle="Annual percentage (except 2021, Q1 only)",
       caption="Source: WSJ collecting Bloomberg Intelligence data", 
       x="Year",
       y="Percentage of trading volume") 
