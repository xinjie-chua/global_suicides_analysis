library(dplyr)
library(countrycode)
library(highcharter)

data <- read_csv("master.csv",show_col_types = FALSE)

# rename columns
data <- data %>%
  select(-c('HDI for year','country-year')) %>%
  rename(gdp_for_year = 'gdp_for_year ($)', 
         gdp_per_capita = 'gdp_per_capita ($)',
         suicide_per_100k = 'suicides/100k pop') %>%
  as.data.frame()

# excluding countries with <= 3 years of data:

minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(), 
           years = rows / 12) %>%
  arrange(years)

data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7)))


# add continent
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

# move continent to the first column
data <- data %>%
  select(continent, everything())

data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))

data <- as_tibble(data)


df3 = data %>%
  filter(country %in% c("United States", "Canada", "Australia", "Mexico", "South Korea")) %>% 
  select(-age, -continent, -gdp_for_year, -sex) %>% 
  select(-generation, -year)


custom_theme <- hc_theme(
  colors = c('#5CACEE', 'green', 'red'),
  chart = list(
    backgroundColor = '#FAFAFA', 
    plotBorderColor = "black"),
  xAxis = list(
    gridLineColor = "C9C9C9", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#C9C9C9", 
    minorGridLineColor = "#C9C9C9", 
    tickColor = "#C9C9C9", 
    title = list(style = list(color = "#333333"))), 
  yAxis = list(
    gridLineColor = "#C9C9C9", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#C9C9C9", 
    minorGridLineColor = "#C9C9C9", 
    tickColor = "#C9C9C9", 
    tickWidth = 1, 
    title = list(style = list(color = "#333333"))),   
  title = list(style = list(color = '#333333', fontFamily = "Lato")),
  subtitle = list(style = list(color = '#666666', fontFamily = "Lato")),
  legend = list(
    itemStyle = list(color = "#333333"), 
    itemHoverStyle = list(color = "#FFF"), 
    itemHiddenStyle = list(color = "#606063")), 
  credits = list(style = list(color = "#666")),
  itemHoverStyle = list(color = 'gray'))

sex_color <- c("#83c99d", "#95a2f0") # baby blue & pink
