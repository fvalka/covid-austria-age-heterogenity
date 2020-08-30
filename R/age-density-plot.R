library(here)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(RcppRoll)

source(here("R/functions/fix-negative-incidence.R"))
source(here("R/functions/austrian-demography.R"))
source(here("R/pipeline/convert-age-stratified-time-series.R"))

convert_age_stratified_time_series()

population_stratified <- gather(
  as_tibble(
    as.list(c(
      "<5"=demo_age_group_total(0:4),
      "5-14"=demo_age_group_total(5:14),
      "15-24"=demo_age_group_total(15:24),
      "25-34"=demo_age_group_total(25:34),
      "35-44"=demo_age_group_total(35:44),
      "45-54"=demo_age_group_total(45:54),
      "55-64"=demo_age_group_total(55:64),
      "65-74"=demo_age_group_total(65:74),
      "75-84"=demo_age_group_total(75:84),
      ">84"=demo_age_group_total(85:100),
      ">24"=demo_age_group_total(25:100)))), "age_group", "pop")

plot_order <- c("<5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", ">84")

data <- read.csv(here("data/age-stratified-incidence-austria.csv"))

data <- data %>%
  group_by(age_group) %>%
  mutate(incidence=fix_negative_incidence(incidence)) %>% 
  mutate(incidence_7day = roll_meanr(incidence, 7)) %>% 
  left_join(population_stratified) %>%
  mutate(incidence_7day_per_capita = incidence_7day/pop*10e5)

data %>%
  ggplot() +
  geom_line(aes(x=ymd(date), y=incidence_7day, color=age_group)) +
  scale_color_viridis_d(limits=plot_order) +
  theme_pubr()

pal_grad3 <- c("#10547E", "#82BC24", "#FFCE47", "#FF0A12")

last_update <- max((data %>% filter(row_number() > 7 & row_number() < n()))$date)

data %>%
  filter(row_number() > 7 & row_number() < n()) %>%
  ggplot() +
  geom_raster(aes(x=ymd(date), y=age_group, fill=incidence_7day_per_capita)) +
  scale_fill_gradientn(colours = colorRampPalette(pal_grad3,space="rgb", bias = 1.7)(1000),
                       limits=c(0,200),
                       n.breaks=10,
                       guide = guide_colourbar(barwidth = 25, 
                                               title.position = "top",
                                               title.hjust = 1)
  ) +
  scale_y_discrete(limits = plot_order) +
  theme_pubclean() +
  labs(fill="New cases per 100k people in age group (7-day moving average)", x=NULL, y="Age Group",
       caption=sprintf("Source: BMSGPK, Österreichisches COVID-19 Open Data Informationsportal (https://www.data.gv.at/covid-19).
       Data lake with hourly snapshots: Attila Kerekes
       Population: Eurostat, The Statistical Office of the European Union, 2019
       Visualization: Fabian Valka
       Last Update: %s", last_update)) +
  ggtitle("Age Distribution of COVID-19 Cases in Austria") + 
  theme(legend.position=c(0.2, -0.15),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold", size=21, margin=margin(12,12,15,12))) 

ggsave("out/age-density.png", width = 13, height = 6, dpi=150 )
