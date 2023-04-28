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
library(ggcorrplot)

glob <- read.csv("Mental health Depression disorder Data.csv")
glob$continent <- countrycode(sourcevar = glob[, "Entity"],
                            origin = "country.name",
                            destination = "continent")
glob <- na.omit(glob)
glob$Year <- as.integer(glob$Year )


names(glob) <- c("index", "Entity", "Code", "Year", "Schizophrenia", "Bipolar", "Eating_disorder", "Anxiety", "Drug_use_disorder", "Depression", "Alcohol_use_disorder", "continent")

glob$Schizophrenia <- as.numeric(glob$Schizophrenia)
glob$Bipolar <- as.numeric(glob$Bipolar)
glob$Eating_disorder <- as.numeric(glob$Eating_disorder)

globcorr <- glob[,c("Schizophrenia", "Bipolar", "Eating_disorder", "Anxiety", "Drug_use_disorder", "Depression", "Alcohol_use_disorder")]


corr <- round(cor(globcorr), 1)

ggcorrplot(corr, hc.order = TRUE,
           lab = TRUE)

# Animated graph
p <- ggplot(glob, aes(x = Anxiety, y = Depression, size = Depression, color = Entity, label = Entity))  +
  guides(color = "none", label = "none") +
  scale_x_continuous(labels = unit_format(unit = "%")) +
  scale_y_continuous(labels = unit_format(unit = "%")) +
  facet_wrap(~ continent) +
  scale_color_viridis_d() +
  dark_theme_light() +
  scale_size(range = c(1, 9)) +
  geom_text(alpha = 0.85, vjust="inward", hjust="inward") +
  # Animation
  labs(title = "Year: {frame_time}",x = "\nAxiety Rates", y = "Depression Rates\n", size = "Depression", color = "Country") +
  transition_time(Year) + ease_aes("linear")

cut_d <- top_n(glob, n = 400, Depression)
cut_a <- top_n(glob, n = 300, Anxiety)
topcount <- c(unique(cut_d$Entity),
              unique(cut_a$Entity))
topcount_cut <- glob %>% filter(Entity %in% topcount)
  
p_c <- ggplot(glob, aes(x = Anxiety, y = Depression, color = continent))  +
  scale_x_continuous(labels = unit_format(unit = "%")) +
  scale_y_continuous(labels = unit_format(unit = "%")) +
  theme_ipsum() +
  scale_size(range = c(1, 30)) +
  geom_point(alpha=0.5, aes(size = Depression, color = continent)) +
  geom_text(data = topcount_cut, alpha=0.7, color="black", aes(label = topcount_cut$Entity)) +
  scale_color_viridis_d() +
  # Animation
  labs(title = "Year: {frame_time}",x = "\nAxiety Rates", y = "Depression Rates\n", size = "Depression", color = "Continet") +
  transition_time(Year) + ease_aes("linear")



animate(p, duration = 15, fps = 20, width = 900, height = 700, renderer = gifski_renderer())
anim_save('globalmentalhealth3.gif')

animate(p_c, duration = 15, fps = 20, width = 800, height = 650, renderer = gifski_renderer())
anim_save('globalmentalhealth5.gif')

# Shiny App

glo <- read.csv("glob.csv")

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    selectInput(inputId = "Region", label = "Please Select Region", choices=c("Asia",  "Africa", "Americas", "Europe", "Oceania", "ALL")),
    selectInput(inputId = "Size", label = "Please Select Size", choices=c("Schizophrenia", "Bipolar", "Eating_disorder", "Drug_use_disorder", "Alcohol_use_disorder")),
    selectInput(inputId = "Color", label = "Please Select Color", choices=c("Schizophrenia", "Bipolar", "Eating_disorder", "Drug_use_disorder", "Alcohol_use_disorder"))
  ),
  mainPanel(imageOutput("plot1"))))

server <- function(input, output) {
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    # now make the animation
    if (input$Region == "ALL") {
      cut_d <- top_n(glo, n = 300, Depression)
      cut_a <- top_n(glo, n = 300, Anxiety)
      topcount <- c(unique(cut_d$Entity),
                    unique(cut_a$Entity))
      topcount_cut <- glo %>% filter(Entity %in% topcount)
      p <- ggplot(glo, aes(x = Anxiety, y = Depression)) +
        geom_point(alpha=0.5, aes_string(size = input$Size, color = input$Color)) + 
        scale_color_viridis() +
        scale_size(range = c(1, 30)) +
        geom_text(data = topcount_cut, alpha=0.7, color="black", aes(label = Entity)) + theme_minimal() +
        # Animation
        labs(title = "Year: {frame_time}", x = "\nAxiety Rates", y = "Depression Rates\n", size = paste0(input$Size), color = paste0(input$Color)) +
        transition_time(Year) + ease_aes("linear")}
    else {
      d <- subset(glo, continent == input$Region)
      cut_d <- top_n(d, n = 100, Depression)
      cut_a <- top_n(d, n = 100, Anxiety)
      topcount <- c(unique(cut_d$Entity),
                    unique(cut_a$Entity))
      topcount_cut <- glo %>% filter(Entity %in% topcount)
      p <- ggplot(data = d, aes(x = Anxiety, y = Depression)) +
        geom_point(alpha=0.5, aes_string(size = input$Size, color = input$Color)) +
        scale_color_viridis() +
        scale_size(range = c(1, 30)) +
        geom_text(data = topcount_cut, alpha=0.7, color="black", aes(label = Entity)) + theme_minimal() +
        # Animation
        labs(title = "Year: {frame_time}", subtitle = paste0(input$Region), x = "\nAxiety Rates", y = "Depression Rates\n", size = paste0(input$Size), color = paste0(input$Color)) +
        transition_time(Year) + ease_aes("linear")
    }
    
    # New
    anim_save("outfile.gif", animate(p, width = 700, height = 500)) # New
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)}
shinyApp(ui, server)

