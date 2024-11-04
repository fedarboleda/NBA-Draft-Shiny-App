library(shiny)
library(tidyverse)
library(purrr)
library(knitr)

source(
  purl("project.qmd", output = tempfile(), quiet = TRUE)
) # import get_players_picked_year() function from qmd

shinyApp(
  ui = fluidPage(
    titlePanel("NBA Draft Players and Outcomes (1989-2021)"), # title for Shiny App
    sidebarLayout(
      sidebarPanel( # aspects to put in side panel of Shiny App
        textInput("season", "Select a draft season:", value = 2003), # user input for season
        actionButton("find_players", "Find Players"), # action button to find players drafted in "season"
        uiOutput("names") # put UI output list of players drafted in sidebar
      ),
      mainPanel(
        fluidRow( # output VORP, seasons active, team draft success, and top-5 table in a column in main panel
          column(11, plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"), tableOutput("table"))
        )
      )
    )
  ),
  server = function(input, output, session) {
    players_from_draft_reactive <- eventReactive(input$find_players, {
      players_from_draft <- get_players_picked_year(draft_players, input$season)
      return(players_from_draft) # create and return reactive dataframe with players drafted from
      # the user's season input upon pressing action button
    })

    output$names <- renderUI(fluidPage({ # creating a UI with the players drafted in that season
      players <- players_from_draft_reactive()$player # extract player name
      names <- map( # using map() to apply function to all players in "players"
        seq_len(length(players)),
        function(i) {
          fluidRow( # function to create action links for all the players in "players"
            actionLink(paste0("link", i), paste0(players[i]))
          )
        }
      )
    }))

    observe({
      lapply(1:nrow(players_from_draft_reactive()), function(i) { # lapply() to apply dialog box to players drafted list
        observeEvent(input[[paste0("link", i)]], { # showing modal dialog box when pressing action link
          showModal(modalDialog( # creating modal dialog box
            # making title of dialog box the player and the college they went to
            title = paste0(players_from_draft_reactive()$player[i], " (", players_from_draft_reactive()$college[i], ")"),
            p(HTML(gsub( # using HTML() and gsub() to create html breaks that facilitate information on different lines
              "\n", "<br/>",
              paste0(
                # number of player picked
                "Picked ", players_from_draft_reactive()$overall_pick[i], " overall in ", players_from_draft_reactive()$year[i], "\n",
                # team the player was drafted by
                " Drafted by ", players_from_draft_reactive()$team[i], "\n",
                # how many years they played in the NBA
                " Played ", players_from_draft_reactive()$years_active[i], " years in the NBA\n",
                # total career value for player by VORP
                " Overall career value by VORP: ", players_from_draft_reactive()$value_over_replacement[i], "\n"
              )
            ))),
            easyClose = TRUE # closes dialog box when clicked anywhere
          ))
        })
      })
    })

    observeEvent(input$find_players, { # create plot when action button to find players is pressed
      output$plot1 <- renderPlot({
        vorp_plot <- ggplot(players_from_draft_reactive(), aes(x = value_over_replacement, fill = factor(1))) +
          geom_histogram() + # histogram for career VORP for players from selected draft year
          scale_fill_manual(values = "green4") + # using fill above and this line to make histogram green
          theme_minimal() + # minimal theme
          theme(legend.position = "none") + # no legend
          labs( # title and axes for plot using season input
            title = paste0("Distribution of career value over replacement player (VORP) for players drafted in ", input$season),
            x = "Career value over replacement player (VORP)", y = "Number of players"
          )
        vorp_plot
      })
    })

    observeEvent(input$find_players, { # create plot when action button to find players is pressed
      output$plot2 <- renderPlot({
        years_plot <- ggplot(players_from_draft_reactive(), aes(x = years_active, fill = factor(1))) +
          geom_histogram() + # histogram for years active for players from selected draft year
          scale_fill_manual(values = "darkred") + # using fill above and this line to make histogram red
          theme_minimal() + # minimal theme
          theme(legend.position = "none") + # no legend
          labs( # title and axes for plot using season input
            title = paste0("Distribution of years in NBA for players drafted in ", input$season),
            x = "Years in NBA", y = "Number of players"
          )
        years_plot
      })
    })

    observeEvent(input$find_players, { # create plot when action button to find players is pressed
      output$plot3 <- renderPlot({
        team_plot <- players_from_draft_reactive() |>
          group_by(team) |> # grouping by team to make calculations per team
          summarize(total_vorp = sum(value_over_replacement)) |> # finding cumulative VORP per draft in a season
          ggplot(aes(x = total_vorp, y = fct_reorder(team, total_vorp), fill = factor(1))) + # using fct_reorder() to put bars in order
          geom_col() + # creating bar chart with team on y-axis and cumulative VORP added in draft on x-axis
          scale_fill_manual(values = "blue") + # making plot blue along with fill above
          theme_minimal() + # minimal theme
          theme(legend.position = "none") + # no legend
          labs( # title and axes for plot using season input
            title = paste0("Value added by teams in the ", input$season, " draft"),
            subtitle = "As measured by VORP added in draft",
            x = "Total team VORP added from draft", y = "Team"
          )
        team_plot
      })
    })

    observeEvent(input$find_players, { # create table when action button to find players is pressed
      output$table <- renderTable({ # creating table for top 5 players by VORP
        players_from_draft_reactive() |>
          arrange(desc(value_over_replacement)) |> # arrange data frame from most career VORP to least
          select(player, overall_pick, team, value_over_replacement) |> # selecting player name, pick, team, and VORP for table
          head(5) |> # selecting top 5 players by VORP
          rename( # formatting column names in table
            "Top 5 players from draft by VORP" = player, "Overall Pick" = overall_pick,
            "Team" = team, "Career VORP" = value_over_replacement
          )
      })
    })
  }
)
