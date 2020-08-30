library(here)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(incidence)
library(EpiEstim)
library(RcppRoll)

source(here("R/functions/fix-negative-incidence.R"))
source(here("R/functions/austrian-demography.R"))
source(here("R/pipeline/convert-age-stratified-time-series.R"))

convert_age_stratified_time_series()

estimate_covid_r <- function(dates, cases) {
  case_incidence <- as.incidence(cases, dates=dates)
  
  window_size <- 7
  
  T <- case_incidence$timespan
  window_size_offset <- window_size - 1
  t_start <- seq(2, T-window_size_offset) # starting at 2 as conditional on the past observations
  t_end <- t_start + window_size_offset
  
  # obtained from: .Richter, L. et al. Schätzung des seriellen Intervalles von COVID19, Österreich. 3.
  si_mean = 4.46
  si_mean_std = 0.153 # calculated from 95% ci from the paper assuming normality by centering and divison by 1.96
  si_std_mean = 2.63
  si_std_std = 0.133 # calculated from 95% ci from the paper assuming normality as above
  
  config_uncertain <- make_config(list(mean_si = si_mean, std_mean_si =si_mean_std,
                                       min_mean_si = si_mean - si_mean_std * 1.96, max_mean_si = si_mean + si_mean_std * 1.96,
                                       std_si = si_std_mean, std_std_si = si_std_std,
                                       min_std_si = si_std_mean - si_std_std * 1.96, max_std_si = si_std_mean + si_std_std * 1.96,
                                       t_start=t_start, t_end=t_end,
                                       mean_prior = 2.59 # obtained from: Liu, Q. et al. Assessing the Global Tendency of COVID-19 Outbreak.
  ))
  
  estimation_result <- estimate_R(case_incidence, 
                                  method="uncertain_si",
                                  config = config_uncertain)
  
  c(rep(NA, window_size), estimation_result$R$`Median(R)`)
}

plot_order <- c("<5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", ">84")

convert_age_stratified_time_series()

data <- read.csv(here("data/age-stratified-incidence-austria.csv"))

data <- data %>%
  group_by(age_group) %>%
  mutate(incidence=fix_negative_incidence(incidence)) %>%
  mutate(r=estimate_covid_r(date, incidence))

data <- data %>% mutate(incidence_7day = roll_sumr(incidence, 14))

data %>%
  ggplot() +
  geom_line(aes(x=ymd(date), y=incidence_7day, color=age_group)) +
  scale_color_viridis_d(limits=plot_order) +
  theme_pubr() +
  ggtitle("New COVID-19 Cases by Age Group, 7-day Moving Average") +
  labs(color="Age Group", x="Date", y="Incidence")
ggsave("out/age-group-incidence.png", dpi=150)

data %>%
  #filter(age_group == "15-24") %>%
  ggplot() +
  geom_line(aes(x=ymd(date), y=r, color=age_group)) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_pubr() +
  coord_cartesian(ylim = c(0,4)) +
  scale_color_viridis_d(limits=plot_order) +
  ggtitle("Time-Varying Reproduction Number by Age Group") +
  labs(color="Age Group", x="Date", y=expression(R[t]))

ggsave("out/age-group-rt.png", dpi=150)
