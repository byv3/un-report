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

# Read back in the CSV file we just wrote

joined_pop_co2<-read_csv("data/joined_co2_pop.csv")
# If you have time, consider how to ggplot your data
# Create a histogram for both gdpPercap and LifeExp, separately, to explore those variables distributions

joined_pop_co2%>%
  ggplot(aes(x = gdpPercap))+
  geom_histogram()
# Find help pages on theme() function to change axis titles font and size

gdp_co2_plot <- joined_pop_co2%>%
  ggplot(aes(x = gdpPercap, y = per_capita_emissions))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "GDP Per Capita", y = "CO2 Emissions Per Capita (metric tons)",
       title = "Comparing Per Capita CO2 emissions and GDP")+
  theme_classic()+
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label)))

ggsave(gdp_co2_plot, filename = "figures/gdp_vs_co2_plot.png", 
       height = 4, width = 6, units = "in",
       dpi = 300)









