library(dplyr)
library(countrycode)

data <- read_csv("master.csv")

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

# Nominal factors
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], function(x){factor(x)})

# Making age ordinal
data$age <- gsub(" years", "", data$age) 
data$age <- factor(data$age, 
                   ordered = T, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))
# Making generation ordinal
data$generation <- factor(data$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

data <- as_tibble(data)
