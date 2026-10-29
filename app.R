
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
        selected = "Borussia Dortmund",
        multiple = TRUE
        ),
      
      checkboxInput(
        inputId = "all_teams",
        label = "Select All Teams",
        value = FALSE
      ),
      
      checkboxGroupInput(
        inputId = "starter_filter",
        label = "Appearance Type:",
        choices = c("Starter", "Substitute"),
        selected = c("Starter", "Substitute")
      ),
      
      hr(),
      
      checkboxInput(
        inputId = "enable_num1",
        label = "Apply First Numeric Filter",
        value = FALSE
      ),
      
      selectInput(
        inputId = "numeric_var1",
        label = "First Numeric Variable:",
        choices = numeric_vars,
        selected = "rating_title__value"
      ),
      
      uiOutput("numeric_slider1"),
      
      checkboxInput(
        inputId = "enable_num2",
        label = "Apply Second Numeric Filter",
        value = FALSE
      ),
      
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
        
        h2("Welcome to the UEFA Champions League Explorer"),
        
        p(
          "This interactive application allows users to explore
    player and team performance statistics from the
    2025-26 UEFA Champions League season."
        ),
        
        h4("About the Dataset"),
        
        p(
          "The dataset contains player-level match observations,
    including match ratings, goals, assists, distance covered,
    chances created, and whether each player appeared as a
    starter or substitute."
        ),
        
        h4("How to Use the App"),
        
        tags$ul(
          tags$li(
            "Sidebar: Select teams, appearance types, and optional
      numerical ranges. Click Apply Filters to update the data."
          ),
          tags$li(
            "Data Download: View and download the filtered dataset."
          ),
          tags$li(
            "Data Exploration: Generate numerical summaries,
      contingency tables, and statistical visualizations."
          )
        ),
        
        h4("Data Source"),
        
        p(
          "The data were obtained from fotmob.com."
        ),
        
        h4("UEFA Champions League"),
        
        tags$a(
          "View the original data source",
          href = "https://www.fotmob.com/leagues/42/overview/champions-league",
          target = "_blank"
        )
      ),
      
      nav_panel(
        "Data Download",
        h3("Explore and Download Data"),
        textOutput("row_count"),
        downloadButton(
          outputId = "download_data",
          label = "Download Filtered Data"
        ),
        br(),
        br(),
        DT::dataTableOutput("filtered_table")
      ),
      
      nav_panel(
        "Data Exploration",
        h3("Statistical Summaries and Visualizations")
      )
   )
)

# Server Settings
server <- function(input, output, session) {
  
  team_choices <- sort(unique(na.omit(player_matches$team_name)))
  
  observeEvent(input$all_teams, {
    
    if (isTRUE(input$all_teams)) {
      updateSelectInput(
        session,
        "team_filter",
        selected = team_choices
      )
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$team_filter, {
    
    updateCheckboxInput(
      session,
      "all_teams",
      value = setequal(input$team_filter, team_choices)
    )
    
  })
  
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
    
    req(input$numeric_range1, input$numeric_range2)
    
    data <- player_matches
    
    # Filter by selected teams
    data <- data |>
      filter(team_name %in% input$team_filter)
    
    # Filter by appearance type
    data <- data |>
      filter(starter %in% input$starter_filter)
    
    # Apply first numeric filter
    var1 <- input$numeric_var1
    range1 <- input$numeric_range1
    
    if (isTRUE(input$enable_num1)) {
      data <- data |>
        filter(
          between(.data[[var1]], range1[1], range1[2])
        )
    }
    
    # Apply second numeric filter
    var2 <- input$numeric_var2
    range2 <- input$numeric_range2
    
    if (isTRUE(input$enable_num2)) {
      data <- data |>
        filter(
          between(.data[[var1]], range1[1], range1[2])
        )
    }
    
    # Debugging messages
    print(paste("Apply Filters clicked:", input$apply_filters))
    print(paste("Selected teams:", paste(input$team_filter, collapse = ", ")))
    print(paste("Rows remaining:", nrow(data)))
    
    data
    
  }, ignoreInit = FALSE)
  
  # Download the currently filtered data as a CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("UCL_Filtered_Data_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
  
  output$filtered_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
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
