library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")

summarise(gapminder_data, averageLifeExp=mean(lifeExp), medianLifeExp=median(lifeExp))

# Learning to pipe

gapminder_summary<-gapminder_data%>%
  summarize(averageLifeExp=mean(lifeExp))

gapminder_summary



# Filtering
gapminder_summary_2007<-gapminder_data%>%
  filter(year == 2007)%>%
  summarize(average = mean(lifeExp))

# Finding average gdpPercap for the first year in the dataset

gapminder_data%>%
  summarize( Firstyear = min(year))

gapminder_data%>%
  filter(year == 1952)

gapminder_data%>%
  filter(year == min(year))%>%
  summarize(Average_GDP = mean(gdpPercap))

# Using group_by()

gapminder_data%>%
  group_by(year, continent)%>%
  summarize(average = mean(lifeExp),
            error = sd(lifeExp))

# Mutate function

gapminder_data%>%
  mutate(gdp = pop * gdpPercap)

# Mutate a new column which is population in millions

gapminder_data%>%
  mutate(popInMillions = pop / 1000000)

# Select 

gapminder_data%>%
  select(pop, year)

gapminder_data%>%
  select(-continent, -pop) 


# Pivot_wider
gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)

# Working with messy data

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region","country","year","series","value","footnotes","source"))

co2_emissions<-co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year == 2005)%>%
  select(-year)


# Bringing in 2007 Population Data
gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%
  filter(year == 2007)%>%
  select(country, pop, lifeExp, gdpPercap)

joined_co2_pop<-inner_join(co2_emissions, gapminder_data_2007)

anti_join( gapminder_data_2007, co2_emissions)

full_join(co2_emissions, gapminder_data_2007)

# Writing a CSV
write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")













