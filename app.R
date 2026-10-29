
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

#categorical variables
categorical_vars <- c(
  "Team" = "team_name",
  "Player" = "player_name",
  "Appearance Type" = "starter"
)


# User Interface Settings
ui <- page_sidebar(
    fillable = FALSE,

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
        
        h3("Statistical Summaries and Visualizations"),
        
        h4("Categorical Summaries"),
        
        selectInput(
          inputId = "contingency_type",
          label = "Select Contingency Table Type:",
          choices = c(
            "One-way" = "one",
            "Two-way" = "two"
          )
        ),
        
        selectInput(
          inputId = "cat_var1",
          label = "First Categorical Variable:",
          choices = categorical_vars,
          selected = "team_name"
        ),
        
        conditionalPanel(
          condition = "input.contingency_type == 'two'",
          
          selectInput(
            inputId = "cat_var2",
            label = "Second Categorical Variable:",
            choices = categorical_vars,
            selected = "starter"
          )
        ),
        
        DT::dataTableOutput("categorical_table"),
        
        hr(),
        
        h4("Numerical Summaries"),
        
        selectInput(
          inputId = "summary_numeric",
          label = "Select Numeric Variable:",
          choices = numeric_vars,
          selected = "rating_title__value"
        ),
        
        selectInput(
          inputId = "summary_group",
          label = "Summarize Across:",
          choices = categorical_vars,
          selected = "team_name"
        ),
        
        DT::dataTableOutput("numerical_table"),
        
        hr(),
        
        h4("Graphical Summaries"),
        
        selectInput(
          inputId = "plot_type",
          label = "Select Plot Type:",
          choices = c(
            "Bar Chart" = "bar",
            "Faceted Bar Chart" = "faceted",
            "Boxplot" = "box",
            "Dumbbell Plot" = "dumbbell",
            "Scatterplot" = "scatter",
            "Heatmap" = "heatmap"
          )
        ),
        
        selectInput(
          inputId = "plot_group",
          label = "Group By:",
          choices = c(
            "Player" = "player_name",
            "Team" = "team_name"
          ),
          selected = "player_name"
        ),
        
        selectInput(
          inputId = "plot_metric",
          label = "Select Statistic:",
          choices = c(
            "Appearances" = "Appearances",
            "Goals" = "goals__value",
            "Assists" = "assists__value",
            "Match Rating" = "rating_title__value",
            "Distance Covered (km)" = "physical_metrics_distance_covered__value",
            "Chances Created" = "chances_created__value"
          ),
          selected = "Appearances"
        ),
        
        conditionalPanel(
          condition = "input.plot_type == 'scatter'",
          
          selectInput(
            inputId = "plot_metric2",
            label = "Select Y-Axis Variable:",
            choices = numeric_vars,
            selected = "rating_title__value"
          )
        ),
        
        plotOutput(
          outputId = "exploration_plot",
          height = "800px",
          width = "900px",
          fill = FALSE
        )
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
  
  # Categorical summaries
  output$categorical_table <- DT::renderDataTable({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0,
           "No observations meet the selected filters.")
    )
    
    var1 <- input$cat_var1
    
    if (input$contingency_type == "one") {
      
      # One-way frequency table
      result <- data |>
        count(.data[[var1]], name = "Frequency") |>
        mutate(
          Percentage = round(
            100 * Frequency / sum(Frequency), 2
          )
        ) |>
        arrange(desc(Frequency))
      
    } else {
      
      # Two-way contingency table
      var2 <- input$cat_var2
      
      validate(
        need(var1 != var2,
             "Please select two different categorical variables.")
      )
      
      result <- data |>
        count(.data[[var1]], .data[[var2]],
              name = "Frequency") |>
        pivot_wider(
          names_from = all_of(var2),
          values_from = Frequency,
          values_fill = 0
        )
    }
    
    DT::datatable(
      result,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Numerical summaries
  output$numerical_table <- DT::renderDataTable({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0,
           "No observations meet the selected filters.")
    )
    
    variable <- input$summary_numeric
    grouping <- input$summary_group
    
    result <- data |>
      group_by(.data[[grouping]]) |>
      summarize(
        N = sum(!is.na(.data[[variable]])),
        
        Mean = round(
          mean(.data[[variable]], na.rm = TRUE), 2
        ),
        
        Median = round(
          median(.data[[variable]], na.rm = TRUE), 2
        ),
        
        SD = round(
          sd(.data[[variable]], na.rm = TRUE), 2
        ),
        
        Minimum = if (all(is.na(.data[[variable]]))) {
          NA_real_
        } else {
          min(.data[[variable]], na.rm = TRUE)
        },
        
        Maximum = if (all(is.na(.data[[variable]]))) {
          NA_real_
        } else {
          max(.data[[variable]], na.rm = TRUE)
        },
        
        .groups = "drop"
      ) |>
      arrange(desc(Mean))
    
    DT::datatable(
      result,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Interactive graphical summaries
  output$exploration_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0,
           "No observations meet the selected filters.")
    )
    
    grouping <- input$plot_group
    metric <- input$plot_metric
    
    # Calculate the requested statistic
    if (input$plot_type == "bar") {
      
      # Regular bar chart
      summary_data <- data |>
        group_by(group_name = .data[[grouping]]) |>
        summarize(
          Value = if (metric == "Appearances") {
            n()
          } else {
            sum(.data[[metric]], na.rm = TRUE)
          },
          .groups = "drop"
        )
      
      ggplot(summary_data,
             aes(x = reorder(group_name, Value),
                 y = Value)) +
        geom_col(fill = "#FDE100", color = "black") +
        coord_flip() +
        scale_y_continuous(breaks = scales::breaks_width(1)) +
        labs(
          title = "Player and Team Performance",
          subtitle = "2025-26 UEFA Champions League",
          x = NULL,
          y = "Total"
        ) +
        theme_minimal()
      
    } else if (input$plot_type == "faceted") {
      
      # Calculate statistics by appearance type
      summary_data <- data |>
        filter(!is.na(starter)) |>
        group_by(group_name = .data[[grouping]], starter) |>
        summarize(
          Value = if (metric == "Appearances") {
            n()
          } else {
            sum(.data[[metric]], na.rm = TRUE)
          },
          .groups = "drop"
        )
      
      # Create faceted bar chart
      ggplot(summary_data,
             aes(x = reorder(group_name, Value),
                 y = Value)) +
        geom_col(fill = "#FDE100", color = "black") +
        facet_wrap(~starter, ncol = 2) +
        coord_flip() +
        scale_y_continuous(breaks = scales::breaks_width(1)) +
        labs(
          title = "Performance by Appearance Type",
          subtitle = "2025-26 UEFA Champions League",
          x = NULL,
          y = "Total"
        ) +
        theme_minimal() +
        theme(
          strip.text = element_text(face = "bold")
        )
    } else if (input$plot_type == "box") {
      
      # Boxplots require individual observations
      # rather than summarized totals
      
      validate(
        need(
          metric != "Appearances",
          "Please select a numeric match-level variable."
        )
      )
      
      plot_data <- data |>
        filter(!is.na(.data[[metric]]))
      
      validate(
        need(
          nrow(plot_data) > 0,
          "No observations available for this variable."
        )
      )
      
      ggplot(
        plot_data,
        aes(
          x = reorder(.data[[grouping]],
                      .data[[metric]], FUN = median),
          y = .data[[metric]]
        )
      ) +
        geom_boxplot(
          fill = "#FDE100",
          color = "black"
        ) +
        geom_jitter(
          width = 0.15,
          alpha = 0.3
        ) +
        coord_flip() +
        labs(
          title = "Distribution of Player and Team Performance",
          subtitle = "2025-26 UEFA Champions League",
          x = NULL,
          y = metric
        ) +
        theme_minimal()
    } else if (input$plot_type == "dumbbell") {
      
      # Plot 4: Dumbbell plot
      
      validate(
        need(metric != "Appearances",
             "Select a numeric match-level variable.")
      )
      
      summary_data <- data |>
        filter(
          !is.na(starter),
          !is.na(.data[[metric]])
        ) |>
        group_by(
          group_name = .data[[grouping]],
          starter
        ) |>
        summarize(
          Average = mean(.data[[metric]]),
          .groups = "drop"
        ) |>
        mutate(
          starter = factor(
            starter,
            levels = c("Starter", "Substitute")
          )
        ) |>
        pivot_wider(
          names_from = starter,
          values_from = Average,
          names_expand = TRUE
        ) |>
        filter(
          !is.na(Starter),
          !is.na(Substitute)
        )
      
      validate(
        need(nrow(summary_data) > 0,
             "No groups have observations in both appearance types.")
      )
      
      ggplot(
        summary_data,
        aes(y = reorder(group_name, Starter))
      ) +
        geom_segment(
          aes(
            x = Starter,
            xend = Substitute,
            yend = reorder(group_name, Starter)
          ),
          color = "gray60",
          linewidth = 1
        ) +
        geom_point(
          aes(x = Starter, color = "Starter"),
          size = 3
        ) +
        geom_point(
          aes(x = Substitute, color = "Substitute"),
          size = 3
        ) +
        scale_color_manual(
          values = c(
            "Starter" = "#E0B900",
            "Substitute" = "#555555"
          )
        ) +
        labs(
          title = "Average Performance: Starter vs. Substitute",
          subtitle = "2025-26 UEFA Champions League",
          x = "Average Value",
          y = NULL,
          color = "Appearance Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          legend.position = "bottom"
        )
      
    } else if (input$plot_type == "scatter") {
      
      # Plot 5: Scatterplot
      
      validate(
        need(metric != "Appearances",
             "Select a numeric match-level variable for the X-axis.")
      )
      
      metric2 <- input$plot_metric2
      
      validate(
        need(metric != metric2,
             "Select two different numeric variables.")
      )
      
      # Calculate average values by selected group
      summary_data <- data |>
        group_by(group_name = .data[[grouping]]) |>
        summarize(
          X = mean(.data[[metric]], na.rm = TRUE),
          Y = mean(.data[[metric2]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        filter(is.finite(X), is.finite(Y))
      
      validate(
        need(nrow(summary_data) >= 2,
             "At least two groups with available data are required.")
      )
      
      ggplot(summary_data, aes(x = X, y = Y)) +
        geom_point(
          color = "#E0B900",
          size = 3,
          alpha = 0.8
        ) +
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = "black",
          linetype = "dashed"
        ) +
        labs(
          title = "Relationship Between Performance Metrics",
          subtitle = "2025-26 UEFA Champions League",
          x = paste("Average", metric),
          y = paste("Average", metric2)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold")
        )
      
    } else if (input$plot_type == "heatmap") {
      
      # Plot 6: Heatmap
      
      summary_data <- data |>
        group_by(group_name = .data[[grouping]]) |>
        summarize(
          Match_Rating = mean(
            rating_title__value, na.rm = TRUE
          ),
          Distance = mean(
            physical_metrics_distance_covered__value,
            na.rm = TRUE
          ),
          Chances = mean(
            chances_created__value, na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        pivot_longer(
          cols = c(Match_Rating, Distance, Chances),
          names_to = "Metric",
          values_to = "Value"
        ) |>
        mutate(
          Value = if_else(is.finite(Value),
                          Value, NA_real_)
        ) |>
        group_by(Metric) |>
        mutate(
          Standardized_Value = {
            s <- sd(Value, na.rm = TRUE)
            
            if (!is.finite(s) || s == 0) {
              rep(NA_real_, n())
            } else {
              (Value - mean(Value, na.rm = TRUE)) / s
            }
          }
        ) |>
        ungroup()
      
      validate(
        need(
          any(is.finite(summary_data$Standardized_Value)),
          "Not enough variation to standardize the selected data."
        )
      )
      
      ggplot(
        summary_data,
        aes(
          x = Metric,
          y = group_name,
          fill = Standardized_Value
        )
      ) +
        geom_tile(
          color = "white",
          height = 0.95
        ) +
        scale_fill_gradient2(
          low = "#555555",
          mid = "white",
          high = "#FDE100",
          midpoint = 0,
          na.value = "gray90"
        ) +
        labs(
          title = "Performance Metrics Heatmap",
          subtitle = "2025-26 UEFA Champions League",
          x = "Performance Metric",
          y = NULL,
          fill = "Standardized\nValue"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(
            angle = 30,
            hjust = 1
          ),
          panel.grid = element_blank()
        )
    }
    
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
