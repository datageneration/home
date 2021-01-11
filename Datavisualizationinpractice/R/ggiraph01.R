# Creating Interactive charts using ggplot2 and ggiraph
# Boxplot, Scatterplot

## Install packages
install.packages(c("vroom", "janitor", "tidyverse", "gapminder","ggiraph"))
lapply(c("vroom", "janitor", "tidyverse", "gapminder", "ggiraph"), require, character.only = TRUE)

## Import data
hpi2016 = vroom::vroom("https://raw.githubusercontent.com/datageneration/datavisualizationinpractice/main/data/happyplanetindex.csv", .name_repair = janitor::make_clean_names)
gm = gapminder
p <- ggplot(hpi2016,
            aes(x = region, y = happy_planet_index, tooltip = region)) +
  geom_boxplot_interactive()
x <- girafe(ggobj = p)
if( interactive() ) print(x)

p <- ggplot(hpi2016, aes(x = region, y = happy_planet_index, tooltip = region, fill = region, data_id=region)) +
  geom_boxplot_interactive(outlier.colour = "red") +
  guides(fill = "none") + theme_bw()

xx <- girafe(ggobj = p)
if( interactive() ) print(xx)

## Use Gapminder data

p <- ggplot(gm,
            aes(x = continent, y = lifeExp, tooltip = year)) +
  geom_boxplot_interactive()
x <- girafe(ggobj = p)
if( interactive() ) print(x)

p <- ggplot(gm,
            aes(x = continent, y = lifeExp, tooltip = continent, fill = continent,)) +
  geom_boxplot_interactive(outlier.color = "green") + theme_bw()
x <- girafe(ggobj = p)
if( interactive() ) print(x)

## Interactive scatterplot

## ggiraph cannot allow single quote in tooltip variable
gm$country=as.character(gm$country)
gm$country[gm$country == "Cote d'Ivoire"] <- "Cote dIvoire"


life <- ggplot(gm, aes(x = gdpPercap, y = lifeExp, color = continent, tooltip = country, data_id = country)) + 
  scale_x_log10(labels = scales::comma) +
  geom_point_interactive(size = .2)  +
  labs(title="Life Expectancy and GDP by continent", 
       subtitle="1952 - 2007",
       caption="Source: Gapminder",
       x="GDP per capita",
       y="Life Expectancy") +
  facet_wrap(~ year) +
  theme_bw() + 
  theme(text=element_text(family="Palatino"), axis.text = element_text(size=8), plot.title = element_text(size=16,hjust = 0.5), plot.subtitle = element_text(size=12,hjust = 0.5), legend.position = "bottom")
x <- girafe(ggobj = life)
if( interactive() ) print(x)  

## Select one year data (1972)    
life <- ggplot(subset(gm, year %in% c("1972")), aes(x = gdpPercap, y = lifeExp, color = continent, tooltip = country, data_id = country)) + 
  scale_x_log10(labels = scales::comma) +
  geom_point_interactive(size = .5)  +
  labs(title="Life Expectancy and GDP by continent", 
       subtitle="",
       caption="Source: Gapminder",
       x="GDP per capita",
       y="Life Expectancy") +
  facet_wrap(~ year) +
  theme_bw() + 
  theme(text=element_text(family="Palatino"), axis.text = element_text(size=8), plot.title = element_text(size=16,hjust = 0.5), plot.subtitle = element_text(size=12,hjust = 0.5), legend.position = "bottom")
x <- girafe(ggobj = life)
if( interactive() ) print(x)  
