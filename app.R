library(shiny)
library(tidyverse)
library(purrr)
library(knitr)

# Import the `get_players_picked_year` function from the .qmd file
source(
  purl("NBA Draft Shiny Write Up.qmd", output = tempfile(), quiet = TRUE)
)

shinyApp(
  ui = fluidPage(
    # Adding custom CSS styling for a modern look
    tags$head(
      tags$style(HTML("
        body {
          font-family: 'Arial', sans-serif;
          background-color: #f7f9fc;
          color: #2c3e50;
        }
        .title {
          font-size: 28px;
          font-weight: bold;
          color: #2c3e50;
          margin-bottom: 15px;
        }
        .sidebar-title {
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 15px;
        }
        .action-link {
          text-decoration: none;
          color: #3498db;
          font-weight: bold;
        }
        .action-link:hover {
          color: #1abc9c;
          text-decoration: underline;
        }
        .btn-primary {
          background-color: #3498db;
          border-color: #3498db;
          color: white;
        }
        .btn-primary:hover {
          background-color: #1abc9c;
          border-color: #1abc9c;
        }
        .grid-layout {
          display: grid;
          grid-template-columns: repeat(2, 1fr);
          gap: 20px;
          margin-top: 20px;
        }
        .single-column-layout {
          display: grid;
          grid-template-columns: 1fr;
          margin-top: 20px;
        }
        .tables-layout {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 20px;
          margin-top: 20px;
        }
        .grid-item {
          background: white;
          padding: 15px;
          border-radius: 8px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        .table-container {
          background: white;
          padding: 15px;
          border-radius: 8px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        .plot-title {
          text-align: center;
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        .table-title {
          font-size: 20px;
          font-weight: bold;
          margin-bottom: 10px;
          text-align: center;
        }
        .modal-header {
          background-color: #2c3e50;
          color: white;
          padding: 15px;
          border-radius: 5px 5px 0 0;
        }
        .modal-body {
          padding: 20px;
          font-size: 16px;
          line-height: 1.5;
        }
        .modal-close-btn {
          background-color: #e74c3c;
          color: white;
          border: none;
          padding: 5px 10px;
          font-size: 14px;
          font-weight: bold;
          cursor: pointer;
        }
        .modal-close-btn:hover {
          background-color: #c0392b;
        }
      "))
    ),
    # App Title
    titlePanel("NBA Draft Players and Outcomes (1989-2021)"),
    sidebarLayout(
      sidebarPanel(
        # Sidebar for user inputs
        div(class = "sidebar-title", "Draft Season Selection"),
        selectInput(
          "season", "Select a draft season:", 
          choices = 1989:2021, # Dropdown menu with years 1989-2021
          selected = 2003
        ), # User input for season
        actionButton("find_players", "Find Players", class = "btn btn-primary"), # Action button
        uiOutput("names") # Dynamically render list of players drafted in the selected year
      ),
      mainPanel(
        # Main panel layout for plots and tables
        div(
          class = "grid-layout",
          div(class = "grid-item", plotOutput("plot1", height = "300px")), # Career VORP plot
          div(class = "grid-item", plotOutput("plot2", height = "300px"))  # Years active plot
        ),
        div(
          class = "single-column-layout",
          div(class = "grid-item", plotOutput("plot3", height = "500px")) # Team success plot
        ),
        div(
          class = "tables-layout",
          div(
            class = "table-container",
            div(class = "table-title", "Top 10 Players by Career VORP"),
            tableOutput("table_top10")
          ),
          div(
            class = "table-container",
            div(class = "table-title", "Bottom 10 Players by Career VORP"),
            tableOutput("table_bottom10")
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    # Store and manage observers dynamically
    observers <- reactiveVal(list())
    
    # Reactive function to fetch players drafted in a specific season
    players_from_draft_reactive <- eventReactive(input$find_players, {
      # Clear existing observers to avoid conflicts when switching seasons
      lapply(observers(), function(obs) obs$destroy())
      observers(list())
      players_from_draft <- get_players_picked_year(draft_players, input$season) # Get players for selected draft year
      return(players_from_draft) # Return the reactive data frame
    })
    
    # Generate UI for the list of players drafted in the selected season
    output$names <- renderUI({
      players <- players_from_draft_reactive()$player # Extract player names from the reactive data
      season <- input$season # Store the current season for dynamic IDs
      
      # Use `map` to generate action links for all players in the selected draft year
      map(
        seq_len(length(players)),
        function(i) {
          fluidRow(
            actionLink(
              paste0("link_", season, "_", i), # Dynamic ID for each player's link
              players[i], # Player name as link text
              class = "action-link" # Add custom CSS styling to the links
            )
          )
        }
      )
    })
    
    # Observe clicks on player links and show corresponding modal dialog boxes
    observe({
      player_data <- players_from_draft_reactive() # Get reactive player data
      season <- input$season # Current season used to identify links
      
      # Create an observer for each player dynamically
      new_observers <- lapply(1:nrow(player_data), function(i) {
        observeEvent(input[[paste0("link_", season, "_", i)]], {
          showModal(modalDialog(
            title = div(class = "modal-header", player_data$player[i], paste0("(", player_data$college[i], ")")),
            easyClose = TRUE,
            footer = tags$button("Close", class = "modal-close-btn", `data-dismiss` = "modal"),
            div(class = "modal-body",
                tags$p(HTML(paste0(
                  "Picked <b>", player_data$overall_pick[i], "</b> overall in <b>", player_data$year[i], "</b>.<br>",
                  "Drafted by <b>", player_data$team[i], "</b>.<br>",
                  "Played <b>", player_data$years_active[i], "</b> years in the NBA.<br>",
                  "Overall career value by VORP: <b>", player_data$value_over_replacement[i], "</b>."
                )))
            )
          ))
        })
      })
      observers(new_observers) # Update observer list with new observers
    })
    
    # Generate the career VORP distribution plot
    observeEvent(input$find_players, {
      output$plot1 <- renderPlot({
        ggplot(players_from_draft_reactive(), aes(x = value_over_replacement)) +
          geom_histogram(binwidth = 1, fill = "green4", color = "white") +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(
            title = paste0("Distribution of Career VORP - ", input$season),
            x = "Career VORP", y = "Number of Players"
          )
      })
    })
    
    # Generate the years active distribution plot
    observeEvent(input$find_players, {
      output$plot2 <- renderPlot({
        ggplot(players_from_draft_reactive(), aes(x = years_active)) +
          geom_histogram(binwidth = 1, fill = "darkred", color = "white") +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(
            title = paste0("Distribution of Years Active in NBA - ", input$season),
            x = "Years in NBA", y = "Number of Players"
          )
      })
    })
    
    # Generate the team draft success bar chart
    observeEvent(input$find_players, {
      output$plot3 <- renderPlot({
        players_from_draft_reactive() |>
          group_by(team) |>
          summarize(total_vorp = sum(value_over_replacement)) |>
          ggplot(aes(x = total_vorp, y = fct_reorder(team, total_vorp))) +
          geom_col(fill = "blue", color = "white") +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(
            title = paste0("Total VORP Added by Teams in ", input$season, " Draft"),
            subtitle = "As measured by VORP added in draft",
            x = "Total Team VORP", y = "Team"
          )
      })
    })
    
    # Generate the table for the top 10 players by VORP
    observeEvent(input$find_players, {
      output$table_top10 <- renderTable({
        players_from_draft_reactive() |>
          arrange(desc(value_over_replacement)) |>
          select(player, overall_pick, team, value_over_replacement) |>
          head(10) |>
          rename(
            "Player" = player,
            "Overall Pick" = overall_pick,
            "Team" = team,
            "Career VORP" = value_over_replacement
          )
      }, striped = TRUE, hover = TRUE) # Enable table styling (striped rows, hover effect)
    })
    
    # Generate the table for the bottom 10 players by VORP
    observeEvent(input$find_players, {
      output$table_bottom10 <- renderTable({
        players_from_draft_reactive() |>
          arrange(value_over_replacement) |>
          select(player, overall_pick, team, value_over_replacement) |>
          head(10) |>
          rename(
            "Player" = player,
            "Overall Pick" = overall_pick,
            "Team" = team,
            "Career VORP" = value_over_replacement
          )
      }, striped = TRUE, hover = TRUE) # Enable table styling (striped rows, hover effect)
    })
  }
)
