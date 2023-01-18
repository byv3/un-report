library(tidyverse)
gapminder_data <- read_csv("gapminder_data.csv")

#Summarizing data
summarize(gapminder_data, averageLifeExp=mean(lifeExp))

#Working with the pipe operator
gapminder_summary <- gapminder_data %>% 
  summarize(averageLifeExp=mean(lifeExp))

Sys.Date()
getwd()
sum(5,6)

ggplot(data = gapminder_data) +
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1")


ggplot(gapminder_data) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))

ggsave("figures/awesome_plot.jpg", width=6, height=4)


violin_plot <- ggplot(data = gapminder_data) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(fill=continent))
violin_plot <- violin_plot + theme_bw()
ggsave("figures/awesome_violin_plot.jpg", plot = violin_plot, width=6, height=4)



install.packages(c("gganimate", "gifski"))
library(gganimate)
library(gifski)


ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop, color = continent)+
  geom_point()


staticHansPlot <- ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5) + # we made our points slightly transparent, because it makes it easier to see overlapping points
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color= "Continent", size="Population (in millions)")+
  theme_classic()

staticHansPlot

staticHansPlot <- ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5) + # we made our points slightly transparent, because it makes it easier to see overlapping points
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color= "Continent", size="Population (in millions)")+
  theme_classic()

staticHansPlot


animatedHansPlot <- staticHansPlot +
  transition_states(year,  transition_length = 1, state_length = 1)+
  ggtitle("{closest_state}")

animatedHansPlot

anim_save("hansAnimatedPlot.gif", 
          plot = animatedHansPlot,
          renderer = gifski_renderer())


# make sure names of countries match between the map info and the data
# NOTE: we haven't learned how to modify the data in this way yet, but we'll learn about that in the next lesson. Just take for granted that it works for now :)
mapdata <- map_data("world") %>%
  mutate(region = recode(region,
                         USA="United States",
                         UK="United Kingdom"))

#install.packages("mapproj")
gapminder_data %>%
  ggplot() +
  geom_map(aes(map_id=country, fill=lifeExp), map=mapdata) +
  expand_limits(x = mapdata$long, y = mapdata$lat) +
  coord_map(projection = "mollweide", xlim = c(-180, 180)) +
  ggthemes::theme_map()
