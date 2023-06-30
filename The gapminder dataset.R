library(gapminder)
library(tidyverse)


# Sort in ascending order of lifeExp
gapminder %>% arrange(lifeExp)

# Sort in descending order of lifeExp
gapminder %>% arrange(desc(lifeExp))

# Filter for the year 1957, then arrange in descending order of population
gapminder %>% filter(year == 1957) %>% arrange(desc(pop))

# Use mutate to change lifeExp to be in months
gapminder %>% mutate(lifeExp = lifeExp * 12) 
gapminder %>% mutate(lifeExpMonths = 12 * lifeExp)


# Filter, mutate, and arrange the gapminder dataset
gapminder %>% filter(year == 2007) %>% mutate(lifeExpMonths = 12 * lifeExp) %>% arrange(desc(lifeExpMonths))


# Visualizing with ggplot2

gapminder %>% filter(year == 1952) -> gapminder_1952

ggplot(gapminder_1952, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

gapminder_1952 %>% ggplot2::ggplot(aes(x = pop, y = lifeExp)) + 
  ggplot2::geom_point()

gapminder_1952 %>% ggplot2::ggplot(aes(x = pop, y = lifeExp)) + 
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() 


gapminder_1952 <- gapminder %>%
  filter(year == 1952)

gapminder_1952 %>% 
  ggplot2::ggplot(aes(x = pop, y = lifeExp, color = continent)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() 


ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) +
  geom_point() +
  scale_x_log10()
