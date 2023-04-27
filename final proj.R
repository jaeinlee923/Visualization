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
library(ggrepel)  
library(gapminder)
library(shiny)

glob <- read.csv("Mental health Depression disorder Data.csv")
glob$continent <- countrycode(sourcevar = glob[, "Entity"],
                            origin = "country.name",
                            destination = "continent")
glob <- na.omit(glob)
glob$Year <- as.integer(glob$Year )
# Animated graph
p <- ggplot(glob, aes(x = Anxiety.disorders...., y = Depression...., size = Depression...., color = Entity, label = Entity))  +
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

cut_d <- top_n(glob, n = 400, Depression....)
cut_a <- top_n(glob, n = 300, Anxiety.disorders....)
topcount <- c(unique(cut_d$Entity),
              unique(cut_a$Entity))
topcount_cut <- glob %>% filter(Entity %in% topcount)
  
p_c <- ggplot(glob, aes(x = Anxiety.disorders...., y = Depression...., color = continent))  +
  scale_x_continuous(labels = unit_format(unit = "%")) +
  scale_y_continuous(labels = unit_format(unit = "%")) +
  theme_ipsum() +
  scale_size(range = c(1, 30)) +
  geom_point(alpha=0.5, aes(size = Depression...., color = continent)) +
  geom_text(data = topcount_cut, alpha=0.7, color="black", aes(label = topcount_cut$Entity)) +
  scale_color_viridis_d() +
  # Animation
  labs(title = "Year: {frame_time}",x = "\nAxiety Rates", y = "Depression Rates\n", size = "Depression", color = "Continet") +
  transition_time(Year) + ease_aes("linear")



animate(p, duration = 15, fps = 20, width = 900, height = 700, renderer = gifski_renderer())
anim_save('globalmentalhealth3.gif')

animate(p_c, duration = 15, fps = 20, width = 800, height = 650, renderer = gifski_renderer())
anim_save('globalmentalhealth5.gif')


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


# Load Data
glob <- read.csv("Mental health Depression disorder Data.csv")
# Create continent variable
glob$continent <- countrycode(sourcevar = glob[, "Entity"],
                              origin = "country.name",
                              destination = "continent")
# Omit those that contain NA values
glob <- na.omit(glob)
# Turn year into integer
glob$Year <- as.integer(glob$Year)


# Shiny App

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    selectInput("Region", "Please Select Region", choices=c("Asia", "Africa","Americas","Europe","Oceania","ALL"))
  ),
  mainPanel(imageOutput("plot1"))))
server <- function(input, output) {
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    # now make the animation
    if (input$Region=="ALL") {
      cut_d <- top_n(glob, n = 300, Depression....)
      cut_a <- top_n(glob, n = 300, Anxiety.disorders....)
      topcount <- c(unique(cut_d$Entity),
                    unique(cut_a$Entity))
      topcount_cut <- glob %>% filter(Entity %in% topcount)
      p <- ggplot(glob, aes(x = Anxiety.disorders...., y = Depression...., color = Alcohol.use.disorders....))  +
        scale_x_continuous(labels = unit_format(unit = "%")) +
        scale_y_continuous(labels = unit_format(unit = "%")) +
        scale_size(range = c(1, 30)) +
        geom_point(alpha=0.5, aes(size = Drug.use.disorders...., color = Alcohol.use.disorders....)) +
        geom_text(data = topcount_cut, alpha=0.7, color="black", aes(label = topcount_cut$Entity)) +
        scale_color_viridis() + theme_minimal() +
        # Animation
        labs(title = "Year: {frame_time}", x = "\nAxiety Rates", y = "Depression Rates\n", size = "Drug Use\nDisorders", color = "Alchohol Related\nDisorders") +
        transition_time(Year) + ease_aes("linear")}
    else {
      d <- subset(glob, continent == input$Region)
      cut_d <- top_n(d, n = 100, Depression....)
      cut_a <- top_n(d, n = 100, Anxiety.disorders....)
      topcount <- c(unique(cut_d$Entity),
                    unique(cut_a$Entity))
      topcount_cut <- glob %>% filter(Entity %in% topcount)
      p <- ggplot(data = d, aes(x = Anxiety.disorders...., y = Depression...., color = Alcohol.use.disorders....))  +
        scale_x_continuous(labels = unit_format(unit = "%")) +
        scale_y_continuous(labels = unit_format(unit = "%")) +
        scale_size(range = c(1, 30)) +
        geom_point(alpha=0.5, aes(size = Drug.use.disorders...., color = Alcohol.use.disorders....)) +
        geom_text(data = topcount_cut, alpha=0.7, color="black", aes(label = Entity)) +
        scale_color_viridis() + theme_minimal() +
        # Animation
        labs(title = "Year: {frame_time}", subtitle = paste0(input$Region), x = "\nAxiety Rates", y = "Depression Rates\n", size = "Drug Use\nDisorders", color = "Alchohol Related\nDisorders") +
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



