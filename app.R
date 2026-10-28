
#reading in libraries
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)


#loading in our data
player_matches <- read.csv("data/player_matches.csv")


# Create a cleaner categorical variable
player_matches <- player_matches |>
  mutate(
    starter = case_when(
      tolower(as.character(starter)) == "true" ~ "Starter",
      tolower(as.character(starter)) == "false" ~ "Substitute",
      TRUE ~ NA_character_
    )
  )

# Numeric variables available for filtering
numeric_vars <- c(
  "Match Rating" = "rating_title__value",
  "Goals" = "goals__value",
  "Assists" = "assists__value",
  "Distance Covered (km)" = "physical_metrics_distance_covered__value",
  "Chances Created" = "chances_created__value"
)


# User Interface Settings
ui <- fluidPage(

    # Application title
    titlePanel("2025-26 UEFA Champions League Explorer"),
    
    #Sidebar
    sidebar = sidebar(
      h4("Filter Data"),
      
      selectInput(
        inputId = "team_filter",
        label = "Select Team(s)",
        choices = sort(unique(na.omit(player_matches$team_name))),
        selected = sort(unique(na.omit(player_matches$team_name))),
        multiple = TRUE
        ),
      
      checkboxGroupInput(
        inputId = "starter_filter",
        label = "Appearance Type:",
        choices = c("Starter", "Substitute"),
        selected = c("Starter", "Substitute")
      ),
      
      hr(),
      
      selectInput(
        inputId = "numeric_var1",
        label = "First Numeric Variable:",
        choices = numeric_vars,
        selected = "rating_title__value"
      ),
      
      uiOutput("numeric_slider1"),
      
      selectInput(
        inputId = "numeric_var2",
        label = "Second Numeric Variable:",
        choices = numeric_vars,
        selected = "physical_metrics_distance_covered__value"
      ),
      
      uiOutput("numeric_slider2"),
      
      hr(),
      
      actionButton(
        inputId = "apply_filters",
        label = "Apply Filters",
        class = 'btn-primary'
      )
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
        h3("Explore and Download Data"),
        textOutput("row_count")
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
