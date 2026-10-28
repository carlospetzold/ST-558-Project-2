
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
ui <- page_sidebar(

    # Application title
    title = "2025-26 UEFA Champions League Explorer",
    
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
  
  #Create first numeric slider
  output$numeric_slider1 <- renderUI({
    
    req(input$numeric_var1)
    
    values <- player_matches[[input$numeric_var1]]
    values <- values[is.finite(values)]
    
    req(length(values) > 0)
    
    sliderInput(
      inputId = "numeric_range1",
      label = "Select Range:",
      min = min(values),
      max = max(values),
      value = range(values),
      step = if (input$numeric_var1 %in% c("goals__value", "assists__value", "chances_created__value")) 1 else NULL
    )
  })
  
  #Create second numeric slider
  output$numeric_slider2 <- renderUI({
    
    req(input$numeric_var2)
    
    values <- player_matches[[input$numeric_var2]]
    values <- values[is.finite(values)]
    
    req(length(values) > 0)
    
    sliderInput(
      inputId = "numeric_range2",
      label = "Select Range:",
      min = min(values),
      max = max(values),
      value = range(values),
      step = if (input$numeric_var2 %in% c("goals__value", "assists__value", "chances_created__value")) 1 else NULL
    )
  })
  
  # Data updates only when Apply Filters is pressed
  filtered_data <- eventReactive(input$apply_filters, {
    
    req(input$team_filter)
    req(input$starter_filter)
    req(input$numeric_range1, input$numeric_range2)
    
    data <- player_matches |>
      filter(
        team_name %in% input$team_filter,
        starter %in% input$starter_filter
      )
    
    var1 <- input$numeric_var1
    var2 <- input$numeric_var2
    
    range1 <- input$numeric_range1
    range2 <- input$numeric_range2
    
    data |>
      filter(
        !is.na(.data[[var1]]),
        .data[[var1]] >= range1[1],
        .data[[var1]] <= range1[2],
        !is.na(.data[[var2]]),
        .data[[var2]] >= range2[1],
        .data[[var2]] <= range2[2]
      )
    
  }, ignoreNULL = FALSE)
  
  # Display number of observations after filtering
  output$row_count <- renderText({
    paste(
      "Number of player-match observations:",
      nrow(filtered_data())
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
