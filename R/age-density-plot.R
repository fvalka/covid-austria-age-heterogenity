library(here)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(RcppRoll)
library(forecast)

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
  mutate(incidence_7day_per_capita = incidence_7day/pop*10e5) %>%
  mutate(incidence_per_capita = incidence/pop*10e5) %>%
  filter(row_number() > 7 & row_number() < n())

data %>%
  mutate(incidence_per_capita_rel = incidence_7day_per_capita/max(incidence_7day_per_capita)) %>%
  filter(n() - row_number() <= 30) %>%
  ggplot() +
  geom_line(aes(x=ymd(date), y=incidence_per_capita_rel, color=age_group)) +
  scale_color_viridis_d(limits=plot_order, option = "plasma", direction=-1) +
  theme_pubclean()

rel_incidence <- data %>%
  mutate(incidence_per_capita_rel = incidence_7day_per_capita/max(incidence_7day_per_capita)) %>%
  filter(n() - row_number() <= 45)

plot_age_group <- function(ages) {
  ggCcf((rel_incidence %>% filter(age_group==ages))$incidence_7day_per_capita,
        (rel_incidence %>% filter(age_group=="15-24"))$incidence_7day_per_capita, lag.max = 20) +
    ggtitle(sprintf("CCF of %s to 15-24", ages)) +
    cowplot::theme_minimal_vgrid() +
    ylim(c(0,1))
}

ggarrange(
  plot_age_group("<5"),
  plot_age_group("5-14"),
  plot_age_group("15-24"),
  plot_age_group("25-34"),
  plot_age_group("35-44"),
  plot_age_group("45-54"),
  plot_age_group("55-64"),
  plot_age_group("65-74"),
  plot_age_group("75-84"),
  plot_age_group(">84"),
  ncol = 3,
  nrow = 4
)


pal_grad3 <- c("#10547E", "#82BC24", "#FFCE47", "#FF0A12")

last_update <- max(data$date)

data  %>%
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
  scale_x_date(date_minor_breaks = "1 week") + 
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

ggsave(here("out/age-density.png"), width = 13, height = 6, dpi=150 )

total_incidence <- data %>% 
  group_by(date) %>% 
  summarise(total_incidence_7day_per_capita = sum(incidence_7day_per_capita))

data  %>%
  left_join(total_incidence) %>%
  mutate(relative_incidence = incidence_7day_per_capita/total_incidence_7day_per_capita) %>%
  ggplot() +
  geom_raster(aes(x=ymd(date), y=age_group, fill=relative_incidence)) +
  scale_fill_gradientn(colours = colorRampPalette(pal_grad3,space="rgb", bias = 2)(1000),
                       n.breaks=10,
                       guide = guide_colourbar(barwidth = 25, 
                                               title.position = "top",
                                               title.hjust = 1),
                       limits=c(0,1)
  ) +
  scale_fill_viridis_c(n.breaks=10,
                       guide = guide_colourbar(barwidth = 25, 
                                               title.position = "top",
                                               title.hjust = 1),
                       limits=c(0,0.5)
                       ) +
  scale_fill_gradientn(colours = colorRampPalette(pal_grad3,space="rgb", bias=1.7)(1000),
                       limits=c(0,0.8),
                       n.breaks=10,
                       guide = guide_colourbar(barwidth = 25, 
                                               title.position = "top",
                                               title.hjust = 1)
  ) +
  scale_y_discrete(limits = plot_order) +
  theme_pubclean() +
  labs(fill="Proportion of the incidence proportion (7-day moving average)", x=NULL, y="Age Group",
       caption=sprintf("
       Showing the relative proportion of the incidence proportion for each age group over a 7-day moving average. 
       The incidence proportion for each age group is calculated by dividing the 7-day moving average of new cases in each 
       age group by the number of people in the age group initially at risk, the total population in that age group. 
       Source: BMSGPK, Österreichisches COVID-19 Open Data Informationsportal (https://www.data.gv.at/covid-19).
       Data lake with hourly snapshots: Attila Kerekes
       Population: Eurostat, The Statistical Office of the European Union, 2019
       Visualization: Fabian Valka
       Last Update: %s", last_update)) +
  ggtitle("Relative Age Distribution of COVID-19 Cases in Austria") + 
  theme(legend.position=c(0.2, -0.2),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold", size=21, margin=margin(12,12,15,12))) 
ggsave(here("out/relative-age-density.png"), width = 13, height = 6, dpi=150 )
