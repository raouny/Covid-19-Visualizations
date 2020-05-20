---
title: "ReadME.MD"
author: "Radhouene Aouni"
date: "5/19/2020"
output: html_document
runtime: shiny
---

# Introduction

1. The current situation: 
As we can see right now, there are more than 4.4 million accumulative confirmed cases across the world, of which over 2.5 million have not been cured yet. The whole world is fighting aganist the highly contagious virus that is threatening us all and we can't assure when the turning point will be, but, please, **stay calm and stay safe**, we will definitely  beat it. 
2. Something about the visualization: For the interactive plots and quick search datatable, the orignal ideas are from jiaying-Wu: [https://jiaying-wu.github.io/COVID-19.github.io/](https://jiaying-wu.github.io/COVID-19.github.io/)
3. Framework of this notebook: I simply broke it into 4 parts, which are in order a brief introduction of the notebook, data wrangling process, analysis and visualization of COVID-19 globally in general, analysis and visualization of COVID-19 in some representative countries/regions. 
4. Suggestions: If you have any question or suggestion for me, please leave a comment and Btw, if you like my notebook, please upvote it.  
5. About update: I added some visualization of Chinese mainland on 05/17/2020 and the whole framework won't change a lot in the near furture, but I will continously update given the original data keeps updating.

# Preparations

Well, before diving into data analysis and visualization, making our raw data tidy is always necessary. 

## loading necessary Packages 

```{r, warning=FALSE, message = FALSE}
install.packages("ggdark")
```

```{r, message=FALSE, warning=FALSE}
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
```

## Importing related data 

The data about COVID-19 is from [https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset](https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset)

The data about country code is from [https://github.com/plotly/datasets/blob/master/2014_world_gdp_with_codes.csv](https://github.com/plotly/datasets/blob/master/2014_world_gdp_with_codes.csv)

```{r}
ts_confirmed <- read_csv(file = "../input/novel-corona-virus-2019-dataset/time_series_covid_19_confirmed.csv",
                           col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))

ts_recovered <- read_csv(file = "../input/novel-corona-virus-2019-dataset/time_series_covid_19_recovered.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))

ts_deaths <- read_csv(file = "../input/novel-corona-virus-2019-dataset/time_series_covid_19_deaths.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))

codes <- read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv',
                 col_types = cols(
                   COUNTRY = col_character(),
                   `GDP (BILLIONS)` = col_double(),
                   CODE = col_character()
                 ))
```

## Preprocessing data 

### Transforming the original data from wide format to long format

In the original data, a day stands for a variable(column), but they should be placed by row. So we have to get all the days together and create a variable "Date" to store them (per day per row format).   

```{r}
ts_confirmed <- ts_confirmed %>%
  gather("Date", "Confirmed", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_recovered <- ts_recovered %>%
  gather("Date", "Recovered", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_deaths <- ts_deaths %>%
  gather("Date", "Deaths", -c("Province/State", "Country/Region", "Lat", "Long")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))
```

### Creating new datasets for further analysis and visualization

We incorporated the three seperate datasets about the confirmed cases, the recovered cases and deaths. We also created a dataset including latest news of COVID-19 and a dataset including total news of COVID-19.

```{r, message=FALSE}
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
```


```{r}
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

```

# What is going on right now with the whole world 

## The latest news about COVID-19

Let's find where we are right now by following the latest news.

We should match codes for the map.

```{r}
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
```

The interactive 3D global map shows us vividly thoes who are suffering the most. You can rotate it to see the whole picture.  

```{r}
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

```

Datatable for quick search of the country/region you are interested in.

**You Can Search by Country/Region** 

```{r}
cases_latest_codes %>%
  select(`Country/Region`,Code, Date, Confirmed, `New Cases`, Recovered, Deaths) %>%
  arrange(desc(Confirmed)) %>%
  datatable(
    rownames = FALSE,
    fillContainer = TRUE,
    options = list(
      bPaginate = FALSE)
  )
```

## COVID-19's spread all the world

### Total cases by date 

This is a interactive barchart, you can just click the part you like.
```{r}
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
```

Here, I use the calendarPlot to keep track of the process of COVID-19's growth by day.

```{r}
confirmed <- cases_total_date[,"Confirmed"]
date <- seq(from=as.Date('2020-01-22'),
            by=1,
            to = as.Date('2020-05-18'))
calendarPlot(data.frame(confirmed, date), pollutant = 'Confirmed', year = 2020, main = "Confirmed Cases")
```

```{r}
recovered <- cases_total_date[,"Recovered"]
calendarPlot(data.frame(recovered, date), pollutant = 'Recovered', year = 2020, main = "Recovered Cases")
```

```{r}
deaths <- cases_total_date[,"Deaths"]
calendarPlot(data.frame(deaths, date), pollutant = 'Deaths', year = 2020, main = "Deaths")
```

### Motality rate and recovery rate 

This is also a interactive plot to show you specifically the mortality rate and recovery
rate in average. You can click to see all details.

We can see the mortality rate is incresing slowly but steadily, which can't be a good sign. The recovery rate, however is unstable by the time, but we can see it has been going up obviously from April.     

```{r}
cases_total_date <- cases_total_date %>%
  group_by(Date, Confirmed) %>%
  mutate(Mortality_rate = Deaths / Confirmed,
         Recovery_rate = Recovered / Confirmed) %>%
  ungroup()
```


```{r}
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
```

Now, let's find out the correlations between all these variables. what we are doing is to quantify and visualize them.

```{r}
cases_total_date %>%
  select(-Date) %>%
  na.omit() %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot.mixed(tl.col = "black", tl.pos = "d", tl.cex = 0.7, cl.cex = 0.7,
                  number.cex = 0.7)
```

# Visualization of some representative countries/regions 

Now, let us move to some countries/regions whose situations are deeply concerned by us.

## Top 10 countries/regions (Confirmed&Deaths&Recovered)

```{r}
top_10_confirmed <- cases_latest %>%
  select('Country/Region', Confirmed) %>%
  arrange(desc(Confirmed))

top_10_confirmed[1:10,] %>%
  ggplot(aes(x = reorder(`Country/Region`,Confirmed), y = Confirmed )) +
  geom_bar(stat = "identity", fill  = "red", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 1400000, by = 200000), labels = comma) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 (the Most Confirmed Cases)") +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"))
```

```{r}
top_10_Deaths <- cases_latest %>%
  select('Country/Region', Deaths) %>%
  arrange(desc(Deaths))

top_10_Deaths[1:10,] %>%
  ggplot(aes(x = reorder(`Country/Region`,Deaths), y = Deaths )) +
  geom_bar(stat = "identity", fill  = "blue", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 80000, by = 8000), labels = comma) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 (the Most Deaths)") +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"))
```

```{r}
top_10_Recovered <- cases_latest %>%
    select('Country/Region', Recovered) %>%
  arrange(desc(Recovered))

top_10_Recovered[1:10,] %>%
  ggplot(aes(x = reorder(`Country/Region`,Recovered), y = Recovered )) +
  geom_bar(stat = "identity", fill  = "green", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 240000, by = 30000), labels = comma) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 (the Most Recovered)") +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"))
```

## Further analysis with the  most affected countries/regions 

To make comparisons more obvious and focus on what have happened recently, most of the plots below used the data in the last month. 

### Comparison between the comfirmed, the recovered and deaths(Top 6).

```{r}
top_6_affected <- ts_total %>%
  select('Country/Region', Date, Confirmed, Deaths, Recovered) %>%
  filter(`Country/Region` %in% c("US", "Spain", "Italy", "United Kingdom",
                                 "Russia","France")) %>%
  group_by(`Country/Region`, Date) %>%
  summarise(Confirmed  = sum(Confirmed),
            Recovered = sum(Recovered),
            Deaths = sum(Deaths)) %>%
  mutate("New_Cases" = Confirmed - lag(Confirmed, 1),
         "Recovery_Rate" = Recovered / Confirmed ,
         "Mortality_Rate" = Deaths / Confirmed) %>%
  mutate("New_Cases" = round(New_Cases, 3),
         "Recovery_Rate" = round(Recovery_Rate, 3),
         "Mortality_Rate" = round(Mortality_Rate, 3))

top_6_affected$New_Cases[is.na(top_6_affected$New_Cases)] <- 0
top_6_affected$Recovery_Rate[is.nan(top_6_affected$Recovery_Rate)] <- 0
top_6_affected$Mortality_Rate[is.nan(top_6_affected$Mortality_Rate)] <- 0
```

```{r}
top_6_affected %>%
  filter(Date >= "2020-04-21") %>%
  select('Country/Region', Date, Confirmed, Recovered, Deaths) %>%
  rename(Names = 'Country/Region') %>%
  gather(Status, Cases, -c("Date", "Names")) %>%
  ggplot(aes(x = Date, y = Cases, fill = as.factor(Names) ) ) +
  geom_bar(stat = "identity", color = "black")+
  scale_fill_brewer(palette = "Pastel1") +
  facet_grid(.~Status) +
  labs(x = "", y= "", fill = "") +
  theme_economist_white() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.text =element_text(face = "bold", size = 8))
```

### Comparison between the recovery rate and mortality rate (Top 6)

It seems a little weird that all the others' recovery rate have gradually been rising since March apart from UK's recovery rate almost closing to zero from
March.

```{r}
top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  ggplot(aes(x = Date, y = Recovery_Rate, group = Names )) +
  theme_clean() +
  geom_line(aes(color = Names), size = 1.3) +
  labs(x = "", y= "", color = "", title = "Recovery Rate")

```

It clearly tells us all the six countries' mortality rates go up roughly over time, although Russia didn't show significant trend. 

```{r}
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
```

## New Cases in countries/regions (Top 6) {.tabset}

### New Cases in  the US

The news cases curve in the US shows high volatility over time. 

```{r, warning=FALSE}
New_Cases_US <-
top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  filter(Names == "US") %>%
  filter(Date >= "2020-04-21")

ggplot(New_Cases_US, aes(x = Date, y = New_Cases, label = New_Cases)) +
  geom_line(size = 3, colour = "white") +
  geom_point(size = 10, colour = "white", shape = 19) +
  geom_text(size = 3, fontface = 'bold', family = "MOntserrat") +
  theme_dark(base_family = "MOntserrat", base_size = 10) +
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#0c244a"),
        plot.background = element_rect(fill = "#0c244a"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(colour = "grey50")) +
  scale_x_date(breaks = New_Cases_US$Date, labels = format(New_Cases_US$Date, "%B\n%d")) +
  labs(x = "", y = "",
       title = "New Cases Per Day(US)")
```


### New Cases in France

France's volatility is not so high compared with US.

```{r, warning=FALSE}
New_Cases_France <-
top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  filter(Names == "France") %>%
  filter(Date >= "2020-04-21")

ggplot(New_Cases_France, aes(x = Date, y = New_Cases, label = New_Cases)) +
  geom_line(size = 3, colour = "white") +
  geom_point(size = 10, colour = "white", shape = 19) +
  geom_text(size = 3, fontface = 'bold', family = "MOntserrat") +
  theme_dark(base_family = "MOntserrat", base_size = 10) +
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#0c244a"),
        plot.background = element_rect(fill = "#0c244a"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(colour = "grey50")) +
  scale_x_date(breaks = New_Cases_France$Date, labels = format(New_Cases_France$Date, "%B\n%d")) +
  labs(x = "", y = "",
       title = "New Cases Per Day(France)")
```


### New Cases in Italy

Italy's new cases has been going down stably over time.

```{r, warning=FALSE}
New_Cases_Italy <-
top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  filter(Names == "Italy") %>%
  filter(Date >= "2020-04-21")

ggplot(New_Cases_Italy, aes(x = Date, y = New_Cases, label = New_Cases)) +
  geom_line(size = 3, colour = "white") +
  geom_point(size = 10, colour = "white", shape = 19) +
  geom_text(size = 3, fontface = 'bold', family = "MOntserrat") +
  theme_dark(base_family = "MOntserrat", base_size = 10) +
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#0c244a"),
        plot.background = element_rect(fill = "#0c244a"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(colour = "grey50")) +
  scale_x_date(breaks = New_Cases_Italy$Date, labels = format(New_Cases_Italy$Date, "%B\n%d")) +
  labs(x = "", y = "",
       title = "New Cases Per Day(Italy)")
```


### New Cases in Russia

Russia's new cases has been going up stably over time, just opposite to those of Italy .

```{r, warning=FALSE}
New_Cases_Russia <-
top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  filter(Names == "Russia") %>%
  filter(Date >= "2020-04-21")

ggplot(New_Cases_Russia, aes(x = Date, y = New_Cases, label = New_Cases)) +
  geom_line(size = 3, colour = "white") +
  geom_point(size = 10, colour = "white", shape = 19) +
  geom_text(size = 3, fontface = 'bold', family = "MOntserrat") +
  theme_dark(base_family = "MOntserrat", base_size = 10) +
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#0c244a"),
        plot.background = element_rect(fill = "#0c244a"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(colour = "grey50")) +
  scale_x_date(breaks = New_Cases_Russia$Date, labels = format(New_Cases_Russia$Date, "%B\n%d")) +
  labs(x = "", y = "",
       title = "New Cases Per Day(Russia)")
```


### New Cases in Spain

Spain's new cases growth is relatively stable, except for a sharp drop in April 24 when something might have happened.

```{r, warning=FALSE}
New_Cases_Spain <-
top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  filter(Names == "Spain") %>%
  filter(Date >= "2020-04-21")

ggplot(New_Cases_Spain, aes(x = Date, y = New_Cases, label = New_Cases)) +
  geom_line(size = 3, colour = "white") +
  geom_point(size = 10, colour = "white", shape = 19) +
  geom_text(size = 3, fontface = 'bold', family = "MOntserrat") +
  theme_dark(base_family = "MOntserrat", base_size = 10) +
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#0c244a"),
        plot.background = element_rect(fill = "#0c244a"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(colour = "grey50")) +
  scale_x_date(breaks = New_Cases_Spain$Date, labels = format(New_Cases_Spain$Date, "%B\n%d")) +
  labs(x = "", y = "",
       title = "New Cases Per Day(Spain)")
```


### New Cases in United Kingdom

UK's new cases trend is similar to the US, both having high volatility.

```{r, warning=FALSE}
New_Cases_United_Kingdom <-
top_6_affected %>%
  rename(Names = 'Country/Region') %>%
  filter(Names == "United Kingdom") %>%
  filter(Date >= "2020-04-21")

ggplot(New_Cases_United_Kingdom, aes(x = Date, y = New_Cases, label = New_Cases)) +
  geom_line(size = 3, colour = "white") +
  geom_point(size = 10, colour = "white", shape = 19) +
  geom_text(size = 3, fontface = 'bold', family = "MOntserrat") +
  theme_dark(base_family = "MOntserrat", base_size = 10) +
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#0c244a"),
        plot.background = element_rect(fill = "#0c244a"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(colour = "grey50")) +
  scale_x_date(breaks = New_Cases_United_Kingdom$Date, labels = format(
    New_Cases_United_Kingdom$Date, "%B\n%d")) +
  labs(x = "", y = "",
       title = "New Cases Per Day(United Kingdom)")
``` 

# Take a look at my country(China)

China is one of the earliest countries fighting against the COVID-19 and  the spread of virus has already been stabilized in the sacrifice of many fighters. Let's take a look at the history.

Here, we only analyse chinese mainland's situation.

## Processing our Data

```{r,message=FALSE, warning=FALSE}
China_mainland <- ts_confirmed %>%
  left_join(ts_deaths) %>%
  left_join(ts_recovered) %>%
  mutate(Recovered = replace_na(Recovered, replace = 0)) %>%
  rename(Province = "Province/State",
         Country.region = "Country/Region") %>%
  filter(Country.region == "China") %>%
  filter(Province != "Hong Kong")

China_mainland_all <-
  China_mainland %>%
  select(Province, Country.region, Confirmed, Deaths, Recovered, Date) %>%
  group_by(Date) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>%
  mutate(New_cases = Confirmed - lag(Confirmed, 1),
            New_deaths = Deaths - lag(Deaths, 1),
            Mortality_rate = Deaths / Confirmed,
            Recovery_rate = Recovered / Confirmed)

China_mainland_all$New_cases[is.na(China_mainland_all$New_cases)] <- 0
China_mainland_all$New_deaths[is.na(China_mainland_all$New_deaths)] <- 0

China_mainland_province <-
  China_mainland %>%
  select(Province, Country.region, Confirmed, Deaths, Recovered, Date) %>%
  group_by(Province, Date) %>%
  summarise(Confirmed  = sum(Confirmed),
            Recovered = sum(Recovered),
            Deaths = sum(Deaths)) %>%
  mutate(New_cases = Confirmed - lag(Confirmed, 1),
         New_deaths = Deaths - lag(Deaths, 1),
         Mortality_rate = Deaths / Confirmed,
         Recovery_rate = Recovered / Confirmed)

China_mainland_province$New_cases[is.na(China_mainland_province$New_cases)] <- 0
China_mainland_province$New_deaths[is.na(China_mainland_province$New_deaths)] <- 0
China_mainland_province$Mortality_rate[is.na(China_mainland_province$Mortality_rate)] <- 0
China_mainland_province$Recovery_rate[is.na(China_mainland_province$Recovery_rate)] <- 0
```

## Where are we now 

Datatable for quick search of the provice you are interested in.

**You Can Search by Province** 

```{r}
datatable <- 
China_mainland_province %>%
  filter(Date == max(Date))
  
datatable %>%
  select(Province, Date, Confirmed, Confirmed, Recovered, Deaths, New_cases) %>%
  arrange(desc(Confirmed)) %>%
  datatable(
    rownames = FALSE,
    fillContainer = TRUE,
    options = list(
      bPaginate = FALSE),
    style = "bootstrap") %>%
  formatStyle("Confirmed", background = styleColorBar(datatable$Confirmed, "red")) %>%
  formatStyle("Recovered", background = styleColorBar(datatable$Recovered, "green")) %>%
  formatStyle("Deaths", background = styleColorBar(datatable$Deaths, "yellow")) %>%
  formatStyle("New_cases", background = styleColorBar(datatable$New_cases, "steelblue"))
```

Interactive barchart of overall Confirmed cases / Recovered cases / Deaths over time


```{r}
p1 <-
China_mainland_all %>% 
  ggplot(aes(x = Date, y= Confirmed, fill = Confirmed)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "D") +
  labs(fill = "Confirmed", x = "", y = "") +
  theme_dark() +
  theme(legend.title = element_text(face = "bold")) 
  
ggplotly(p1)
```

```{r}
P2 <- 
China_mainland_all %>% 
  ggplot(aes(x = Date, y= Deaths, fill = Deaths)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "C") +
  labs(fill = "Deaths", x = "", y = "") +
  theme_dark() +
  theme(legend.title = element_text(face = "bold")) 

ggplotly(P2)
```


```{r}

P3 <- 
  China_mainland_all %>% 
  ggplot(aes(x = Date, y= Recovered, fill = Recovered)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "A") +
  labs(fill = "Recovered", x = "", y = "") +
  theme_dark() +
  theme(legend.title = element_text(face = "bold")) 
  

ggplotly(P3)
```

Overall New cases / New deaths over time

```{r}
China_mainland_all %>% 
  select(Date, New_cases, New_deaths) %>%
  gather(status, cases, -Date) %>%
  ggplot(aes(x = Date, y= cases, group = status)) +
  geom_line(aes(color = status), size = 1.3) +
  labs(color = "") +
  theme_dark() +
  theme(legend.position = "top")+
  labs(x = "", y = "", fill = "")
```

Overall Mortality rate / Recovery Rate over time

```{r}
China_mainland_all %>% 
  select(Date, Mortality_rate, Recovery_rate) %>%
  gather(status, cases, -Date) %>%
  ggplot(aes(x = Date, y= cases, group = status)) +
  geom_line(aes(color = status), size = 1.3) +
  labs(color = "") +
  theme_dark() +
  theme(legend.position = "top")+
  labs(x = "", y = "", fill = "")
```

## Cases by Province 

Comparison of the confirmed/ recovered and deaths between procinces 

```{r}
China_mainland_province %>%
  filter(Date == max(Date)) %>%
  select(Province, Confirmed, Recovered, Deaths) %>%
  gather(status, cases, -Province) %>%
  ggplot(aes(x = reorder(Province, cases), y = cases, fill = status )) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 150000, by = 30000), labels = comma) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  theme_economist() +
  labs(x = "", y ="", fill = "")
```

Details about the most affected provinces (Top 6)

Ratio of confirmed cases 

```{r}
Top_6 <- 
  China_mainland_province %>%
  filter(Date == max(Date)) %>%
  arrange(desc(Confirmed)) 
Top_6 <- Top_6[1:6,]
```

```{r,message=FALSE, warning=FALSE}
percent <- paste(Top_6$Province ,round(100*Top_6$Confirmed / sum(Top_6$Confirmed), 2), "%")

Top_6 %>%
  ggplot(aes(x = "", y = Confirmed, fill = percent) ) +
  geom_bar(stat = "identity",  width = 1) +
  annotate("text", x=1, y =40000, label = percent[1], size = 5) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
       plot.title = element_text(hjust = 0.5),
       axis.text.x = element_blank()) +
  theme_economist() +
  labs(x="", y = "", title = "Confirmed", fill = "") 
  
```

Confirmed cases / Recovered Rate over time by province (Top 6)

```{r,message=FALSE, warning=FALSE}
Top_6_all <- China_mainland_province %>%
  filter(Province == Top_6$Province)

Top_6_all %>%
  select(Province, Date, Confirmed, Recovered, Deaths) %>%
  gather(status, cases, -c("Province", "Date")) %>%
  ggplot()+
  geom_line(aes(Date, cases, group = status, color = status), lwd = 1) +
  labs(color = "") +
  facet_wrap(~ Province, scales = "free") +
  theme_fivethirtyeight() +
  dark_mode(theme_fivethirtyeight())
```

New cases / New Deaths Rate over time by province (Top 6)

```{r,message=FALSE, warning=FALSE}
Top_6_all %>%
  select(Province, Date, New_cases, New_deaths) %>%
  gather(status, cases, -c("Province", "Date")) %>%
  ggplot(aes(Date, cases, fill = status))+
  geom_bar(stat = "identity", position = "stack") +
  labs(fill = "") +
  facet_wrap(~ Province, scales = "free") +
  theme_fivethirtyeight() +
  dark_mode(theme_fivethirtyeight())
```

Mortality rate / recovery rate over time by province (Top 6)

```{r}
Top_6_all %>%
  select(Province, Date, Mortality_rate, Recovery_rate) %>%
  gather(status, cases, -c("Province", "Date")) %>%
  ggplot()+
  geom_line(aes(Date, cases, group = status, color = status), lwd = 1) +
  labs(color = "") +
  facet_wrap(~ Province, scales = "free") +
  theme_fivethirtyeight() +
  dark_mode(theme_fivethirtyeight())
```

**This is the end. Thanks for reading**