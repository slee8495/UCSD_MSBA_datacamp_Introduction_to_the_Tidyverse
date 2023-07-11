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

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

gapminder_1952 %>% 
  ggplot2::ggplot(aes(x = pop, y = lifeExp)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~continent)


gapminder %>% 
  ggplot2::ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~year)


gapminder %>% 
  dplyr::summarise(medianLifeExp = median(lifeExp))


gapminder %>% 
  dplyr::filter(year == 1957) %>% 
  dplyr::summarise(medianLifeExp = median(lifeExp),
                   maxGdpPercap = max(gdpPercap))

gapminder %>% 
  dplyr::filter(year == 1957) %>% 
  dplyr::group_by(continent) %>% 
  dplyr::summarise(medianLifeExp = median(lifeExp),
                   maxGdpPercap = max(gdpPercap))


gapminder %>% 
  dplyr::group_by(continent, year) %>% 
  dplyr::summarise(medianLifeExp = median(lifeExp),
                   maxGdpPercap = max(gdpPercap))


by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

by_year %>% 
  ggplot2::ggplot(aes(x = year, y = medianLifeExp)) +
  ggplot2::geom_point() +
  ggplot2::expand_limits(y = 0)

gapminder %>% 
  dplyr::group_by(continent, year) %>% 
  dplyr::summarise(medianGdpPercap = median(gdpPercap)) -> by_year_continent

by_year_continent %>% 
  ggplot2::ggplot(aes(x = year, y = medianGdpPercap, color = continent)) +
  ggplot2::geom_point() +
  ggplot2::expand_limits(y = 0)


gapminder %>% 
  dplyr::filter(year == 2007) %>% 
  dplyr::group_by(continent) %>% 
  dplyr::summarise(medianLifeExp = median(lifeExp),
                   medianGdpPercap = median(gdpPercap)) -> by_continent_2007

by_continent_2007 %>% 
  ggplot2::ggplot(aes(x = medianGdpPercap, y = medianLifeExp, color = continent)) +
  ggplot2::geom_point()


by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))


by_year %>% 
  ggplot2::ggplot(aes(x = year, y = medianGdpPercap)) +
  ggplot2::geom_line() +
  ggplot2::expand_limits(y = 0)



by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

by_year_continent %>% 
  ggplot2::ggplot(aes(x = year, y = medianGdpPercap, color = continent)) +
  ggplot2::geom_line() +
  ggplot2::expand_limits(y = 0)


gapminder %>% 
  dplyr::filter(year == 1952) %>% 
  dplyr::group_by(continent) %>% 
  dplyr::summarise(medianGdpPercap = median(gdpPercap)) -> by_continent

by_continent %>% 
  ggplot2::ggplot(aes(x = continent, y = medianGdpPercap)) +
  ggplot2::geom_col()

gapminder %>% 
  dplyr::filter(year == 1952, continent == "Oceania") -> oceania_1952

oceania_1952 %>% 
  ggplot2::ggplot(aes(x = country, y = gdpPercap)) +
  ggplot2::geom_col()

gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

gapminder_1952 %>% 
  ggplot2::ggplot(aes(x = pop_by_mil)) +
  ggplot2::geom_histogram(bins = 50)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

gapminder_1952 %>% 
  ggplot2::ggplot(aes(x = pop)) +
  ggplot2::geom_histogram() +
  ggplot2::scale_x_log10()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

gapminder_1952 %>% 
  ggplot2::ggplot(aes(x = continent, y = gdpPercap)) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_y_log10()


ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +
  ggplot2::labs(title = "Comparing GDP per capita across continents")
