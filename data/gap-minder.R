library(pacman)
p_load(tidyverse, readxl)

# lecture

# Free data from Gapminder.org
emissions <- read_excel("data/indicator CDIAC carbon_dioxide_emissions_per_capita.xlsx", sheet="Data") %>%
  rename(country = `CO2 per capita`) %>%
  gather(key = year, value = emissions, -country) %>%
  mutate(year = as.integer(year))

# Free data from Gapminder.org
population <- read_excel("data/indicator gapminder population.xlsx", sheet="Data") %>%
  rename(country = `Total population`) %>%
  gather(key = year, value = population, -country) %>%
  mutate(year = as.integer(year))

# Free data from Gapminder.org
life_expectancy <- read_excel("data/indicator life_expectancy_at_birth.xlsx", sheet="Data") %>%
  rename(country = `Life expectancy`) %>%
  gather(key = year, value = life_expectancy, -country) %>%
  mutate(year = as.integer(year))

# Free data from Gapminder.org
income <- read_excel("data/indicator gapminder gdp_per_capita_ppp.xlsx", sheet="Data") %>%
  rename(country = `GDP per capita`) %>%
  gather(key = year, value = income, -country) %>%
  mutate(year = as.integer(year))

# Free data from Gapminder.org
countries <- read_excel("data/Data Geographies - v1 - by Gapminder.xlsx", sheet="list-of-countries-etc")

gapminder <- list(emissions, population, life_expectancy, income) %>% Reduce(function(a,b) full_join(a, b, by=c("country", "year")), x=.) %>%
  semi_join(countries, by=c("country"="name"))

gapminder %>%
  group_by(country) %>%
  summarise_all(function(var) list(var)) %>%
  jsonlite::toJSON(na = "null") %>%
  write(file='data/gap-minder.json')