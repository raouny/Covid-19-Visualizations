
## loading necessary Packages 

library(readr)
library(tidyverse)
library(plotly)
library(ggthemes)
library(gganimate)
library(geosphere)
library(DT)
library(scales)
library(openair)
library(corrplot)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(ggdark)

# Importing related data

ts_confirmed <- read_csv(file = "./data/time_series_covid_19_confirmed.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))

ts_recovered <- read_csv(file = "./data/time_series_covid_19_recovered.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))

ts_deaths <- read_csv(file = "./data/time_series_covid_19_deaths.csv",
                      col_types = cols(
                        .default = col_double(),
                        `Province/State` = col_character(),
                        `Country/Region` = col_character()
                      ))

codes <- read_csv('./data/2014_world_gdp_with_codes.csv',
                  col_types = cols(
                    COUNTRY = col_character(),
                    `GDP (BILLIONS)` = col_double(),
                    CODE = col_character()
                  ))

# Transforming the original data from wide format to long format

ts_confirmed <- ts_confirmed %>%
  gather("Date", "Confirmed", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_recovered <- ts_recovered %>%
  gather("Date", "Recovered", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_deaths <- ts_deaths %>%
  gather("Date", "Deaths", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

## Incorporate datasets

ts_total <- ts_confirmed %>%
  left_join(ts_deaths) %>%
  left_join(ts_recovered) %>%
  mutate(Recovered = replace_na(Recovered, replace = 0))

## We all know "Diamond Princess" and "MS Zaandam" are cruises, So we have to remove them from the data

ts_total <- ts_total %>%
  filter(`Country/Region` != "Diamond Princess") %>%
  filter(`Country/Region` != "MS Zaandam")

## Created a dataset including latest news of COVID-19

cases_latest <- ts_total %>%
  group_by(`Country/Region`, Date) %>%
  summarise(Confirmed  = sum(Confirmed),
            Recovered = sum(Recovered),
            Deaths = sum(Deaths)) %>%
  mutate("New Cases" = Confirmed - lag(Confirmed, 1) ) %>%
  filter(Date == max(Date))

day_latest <- max(cases_latest$Date)

## Created a dataset including total news of COVID-19

cases_total_date <- ts_total %>%
  rename(Region = `Country/Region`) %>%
  group_by(Date) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>%
  mutate("New Cases" = Confirmed - lag(Confirmed, 1))

cases_total_latest <- cases_total_date %>%
  filter(Date == max(Date))

codes <- codes %>%
  select(COUNTRY, CODE) %>%
  rename(Region = COUNTRY ,
         Code = CODE) %>%
  rownames_to_column("id")

codes$id <- as.integer(codes$id)

## Making sure countries's and regions' names are in line with other datasets.

codes$Region <- codes$Region %>%
  str_replace(pattern = "United States", replacement = "US") %>%
  str_replace(pattern = "Macedonia", replacement = "North Macedonia") %>%
  str_replace(pattern = "Czech Republic", replacement = "Czechia") %>%
  str_replace(pattern = "Taiwan", replacement = "Taiwan*") %>%
  str_replace(pattern = "West Bank", replacement = "West Bank and Gaza") %>%
  str_replace(pattern = "Congo, Democratic Republic of the", replacement = "Congo (Kinshasa)") %>%
  str_replace(pattern = "Congo, Republic of the", replacement = "Congo (Brazzaville)") %>%
  str_replace(pattern = "Bahamas, The", replacement = "Bahamas") %>%
  str_replace(pattern = "Swaziland", replacement = "Eswatini") %>%
  str_replace(pattern = "Gambia, The", replacement = "Gambia")

cases_latest_codes <- cases_latest %>%
  left_join(codes, by = c("Country/Region" = "Region" )) %>%
  arrange(desc(Confirmed))

## Setting boundries' color as light grey

line <- list(color = toRGB("#d1d1d1"), width = 0.2)

## Specifing parameters of the 3D map
geo <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'orthographic'),
  resolution = '100',
  showcountries = TRUE,
  countrycolor = '#d1d1d1',
  showocean = TRUE,
  oceancolor = '#064273',
  showlakes = TRUE,
  lakecolor = '#99c0db',
  showrivers = TRUE,
  rivercolor = '#99c0db',
  bgcolor = '#e8f7fc')

plot_geo() %>%
  layout(geo = geo,
         paper_bgcolor = '#e8f7fc',
         title = paste0("World COVID-19 Confirmed by Region at", day_latest)) %>%
  add_trace(data = cases_latest_codes,
            z = ~Confirmed,
            colors = "Reds",
            text = ~'Country/Region',
            locations = ~Code,
            marker = list(line = line))

cases_latest_codes %>%
  select(`Country/Region`,Code, Date, Confirmed, `New Cases`, Recovered, Deaths) %>%
  arrange(desc(Confirmed)) %>%
  datatable(
    rownames = FALSE,
    fillContainer = TRUE,
    options = list(
      bPaginate = FALSE)
  )

cases_all <- cases_total_date %>%
  select(-Confirmed, -`New Cases`) %>%
  gather("Status", "Cases", -"Date")

barchart <- ggplot(data = cases_total_date, aes(x = Date)) +
  geom_bar(aes(y = Confirmed), position = "stack", stat = "identity", fill = "#ff5050") +
  geom_bar(data = cases_all, aes(y = Cases, fill = Status), position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#000000", "#009900")) +
  scale_y_continuous(breaks = seq(0, 4200000, by = 300000), labels = comma) +
  theme_solarized(base_size = 10, light = TRUE)+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"),
        panel.background = element_rect(fill = "White"),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("World COVID-19 Total Cases by Day")

ggplotly(barchart) %>%
  layout(legend = list(orientation = 'h'))

confirmed <- cases_total_date[,"Confirmed"]
date <- seq(from=as.Date('2020-01-22'),
            by=1,
            to = as.Date('2020-05-18'))
calendarPlot(data.frame(confirmed, date), pollutant = 'Confirmed', year = 2020, main = "Confirmed Cases")

recovered <- cases_total_date[,"Recovered"]
calendarPlot(data.frame(recovered, date), pollutant = 'Recovered', year = 2020, main = "Recovered Cases")

deaths <- cases_total_date[,"Deaths"]
calendarPlot(data.frame(deaths, date), pollutant = 'Deaths', year = 2020, main = "Deaths")

cases_total_date <- cases_total_date %>%
  group_by(Date, Confirmed) %>%
  mutate(Mortality_rate = Deaths / Confirmed,
         Recovery_rate = Recovered / Confirmed) %>%
  ungroup()


barchart_1 <-cases_total_date %>%
  select(Date, Mortality_rate, Recovery_rate) %>%
  gather(status.ratio, ratio, -Date ) %>%
  ggplot(aes(x = Date, y = ratio, fill = status.ratio)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"),
        panel.background = element_rect(fill = "White"),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("The Mortality_rate and the Recovery_rate")

ggplotly(barchart_1) %>%
  layout(legend = list(orientation = 'h'))

cases_total_date %>%
  select(-Date) %>%
  na.omit() %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot.mixed(tl.col = "black", tl.pos = "d", tl.cex = 0.7, cl.cex = 0.7,
                 number.cex = 0.7)

top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  ggplot(aes(x = Date, y = Mortality_Rate, fill = Names)) +
  geom_bar(stat = "identity",alpha = 0.8) +
  facet_wrap(~ Names) +
  theme_minimal() +
  labs(x = "", y = "Mortality Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.y = element_text(face = "bold", size = 10))
