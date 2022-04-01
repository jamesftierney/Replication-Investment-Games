library(tidyverse)
library(ggstream)
library(cowplot)
library(paletteer)


# create a time series of monthly active users
users <- read_csv("~/Downloads/statistic_id1259920_monthly-active-users-of-the-leading-etrading-apps-in-the-us-2017-2021.csv")
users <- rename(users, "date" = "X1")
users$date <- as.Date(paste0("01-", users$date),format = "%d-%b-%y")

# sum up the monthly active users and calculate market share
users <- users %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(across(where(is.numeric)))) %>%
  gather(Brokerage, value, c(`Robinhood`, `Fidelity Investments`, `WeBull`, `Schwab Mobile`, `E*TRADE`, `TD Ameritrade`, `Merrill Edge`, `eToro`, `TradeStation`, `Interactive Brokers`)) %>%
  mutate(share = value/sum) 
  
# Create a time series chart of all the individual monthly active users
ggplot(users, aes(x=date)) + 
  geom_line(aes(y=value, col=Brokerage)) + 
  cowplot::theme_minimal_vgrid(font_size = 12) +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::lighten(.2)) +  
  labs(title="Monthly active users of online brokerage apps",
       caption="Source: Airnow (via Statista)", 
       y="Monthly users (in 1,000s)") +  # title and caption
  theme(panel.grid.minor = element_blank())

# market share plot of monthly active users
ggplot(users, aes(x=date, y=share, fill=variable)) +
geom_area()


# total plot of monthly active users
ggplot(users, aes(x=date, y=value, fill=Brokerage, label =Brokerage, color = Brokerage)) +
  geom_area()+
  #geom_stream_label(size = 4, type = "mirror", n_grid = 1000) +
  theme(legend.position = "none") +
  cowplot::theme_minimal_vgrid(font_size = 12) +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::darken(.8)) +
  scale_fill_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::lighten(.2)) +  
  labs(title="Monthly active users of online brokerage apps",
       caption="Source: Airnow (via Statista)", 
       y="Monthly users (in 1,000s)") +  # title and caption
  theme(panel.grid.minor = element_blank())


# total plot of monthly active users, using stream plot
ggplot(users, aes(x=date, y=value, fill=Brokerage, label =Brokerage, color = Brokerage)) +
geom_stream(type="ridge", bw="0.5")+
#geom_stream_label(size = 4, type = "mirror", n_grid = 1000) +
    theme(legend.position = "none") +
  cowplot::theme_minimal_vgrid(font_size = 12) +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::darken(.8)) +
  scale_fill_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::lighten(.2)) +  
  labs(title="Monthly active users of online brokerage apps",
       caption="Source: Airnow (via Statista)", 
       y="Monthly users (in 1,000s)") +  # title and caption
  theme(panel.grid.minor = element_blank())

# market share plot of monthly active users, using stream plot
ggplot(users, aes(x=date, y=value, fill=Brokerage, label =Brokerage, color = Brokerage)) +
  geom_stream(type="proportional", bw="0.5")+
  #geom_stream_label(size = 4, type = "mirror", n_grid = 1000) +
  theme(legend.position = "none") +
  cowplot::theme_minimal_vgrid(font_size = 12) +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::darken(.8)) +
  scale_fill_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::lighten(.2)) +  
  labs(title="Monthly active users of online brokerage apps (market share)",
       caption="Source: Airnow (via Statista)", 
       y="Share of total monthly active users") +  # title and caption
  theme(panel.grid.minor = element_blank())

# market share plot of monthly active users, using calculated share
ggplot(users, aes(x=date, y=share, fill=Brokerage, label=Brokerage, color=Brokerage)) + 
  geom_area() +
  theme(legend.position = "none") +
  cowplot::theme_minimal_vgrid(font_size = 12) +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::darken(.8)) +
  scale_fill_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::lighten(.2)) +  
  labs(title="Monthly active users of online brokerage apps (market share)",
       caption="Source: Airnow (via Statista)", 
       y="Share of total monthly active users") +  # title and caption
  theme(panel.grid.minor = element_blank())

