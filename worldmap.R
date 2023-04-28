library(readr)
library(gganimate)
library(ggplot2)
library(countrycode)
library(scales)
library(gifski)
library(ggdark)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(ggrepel)  
library(gapminder)
library(shiny)
library(maps)

world <- map_data("world")
# Load Data
glo <- read.csv("glob.csv")
name <- names(glo)
# Change entity column name to region
names(glo)[names(glo) == "Entity"] <- "region"
# Missing country: USA
glo$region[glo$region == 'United States'] <- 'USA'
# Missing country: Congo
glo$region[glo$region == 'Congo'] <- 'Republic of Congo'

glo$region[glo$region == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'

worldSubset <- inner_join(world, glo, by = "region")

# Style theme
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

# Schizophrenia
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Schizophrenia)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(title = "Schizophrenia Rates") +
  plain
# Depression
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Depression)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(title = "Depressioin Rates") +
  plain
# Anxiety
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Anxiety)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(title = "Anxiety Disorder Rates") +
  plain
# Bipolar
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Bipolar)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(title = "Bipolar Disorder Rates") +
  plain
# Eating disorders
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Eating_disorder)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(title = "Eating Disorder Rates") +
  plain
# Drug Use
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Drug_use_disorder)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(title = "Drug Use Disorder Rates") +
  plain

# Alcohol Use
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Alcohol_use_disorder)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  labs(title = "Alcohol Use Disorder Rates") +
  plain

