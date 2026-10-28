
#reading in libraries
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)


#loading in our data
player_matches <- read.csv("data/player_matches.csv")



# User Interface Settings
ui <- fluidPage(

    # Application title
    titlePanel("2025-26 UEFA Champions League Explorer"),
    
    #Sidebar
    sidebar = sidebar(
      h4("Filter Data"),
      p("Select filters to explore Champions League Statistics.")
    ),

    #Navigation Set Up
    navset_card_tab(
      
      nav_panel(
        "About",
        h3("Welcome to the UEFA Champions League Explorer"),
        p("Explore player and team performance statistics.")
      ),
      
      nav_panel(
        "Data Download",
        h3("Data Download")
      ),
      
      nav_panel(
        "Data Exploration",
        h3("Statistical Summaries and Visualizations")
      )
   )
)

# Server Settings
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
