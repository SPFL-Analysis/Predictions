
library(shiny)
library(tidyverse)
library(RODBC)
library(ggiraph)
library(scales)

# Define UI for application that draws a histogram
ui <-
  fluidRow(
    #titlePanel("SPFL xG Game Maps"),
    column(
      width = 3,
      offset = 0,
      selectInput(
        inputId = "season",
        label = "Season",
        choices = c("2018-2019")
      ),
      selectInput(
        inputId = "league",
        label = "League",
        choices = c("Premiership", "Championship", "League 1", "League 2")
      ),
      uiOutput("homeList"),
      uiOutput("awayList"),
      uiOutput("matchDates"),
      hr(),
      actionButton(inputId = "runPlot", label = "Create plot")
    ),
    # Show a plot of the generated distribution
    column(
      width = 9,
      offset = 0,
      align = 'left',
      ggiraph::ggiraphOutput("xGMap", width = "80%", height = "600px")
      )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

  # get the input game data
  channel <- RODBC::odbcConnect("SPFL")
  qry <- "select * from spfl.dbo.getShotMapShinyData"
  mapData <- sqlQuery(channel, qry)
  odbcClose(channel)

  output$homeList <- renderUI({
    homeTeams <- dplyr::filter(
      mapData,
      season == input$season,
      league == input$league
    ) %>%
      dplyr::select(home) %>%
      pull() %>%
      as.character()
    selectInput(
      "home", 
      "Home team", 
      sort(unique(homeTeams)),
      selected = sort(unique(homeTeams))[1])
  })

  output$awayList <- renderUI({
    awayTeams <- dplyr::filter(
      mapData,
      season == input$season,
      league == input$league,
      home == input$home,
      away != input$home
    ) %>%
      dplyr::select(away) %>%
      pull() %>%
      as.character()
    selectInput(
      "away", 
      "Away team", 
      sort(unique(awayTeams)), 
      selected = sort(unique(awayTeams))[1]
      )
  })

  output$matchDates <- renderUI({
    dates <- dplyr::filter(
      mapData,
      season == input$season,
      league == input$league,
      home == input$home,
      away == input$away
    ) %>%
      dplyr::select(matchDate) %>%
      pull()

    if (length(dates) == 0) {
      hr("No matches played between these teams.")
    } else {
      checkboxGroupInput("date", "Macth date", sort(unique(dates)))
    }
  })

  # plot function
  create_shotmap <- function(df) {
    df$home <- as.character(df$home)
    df$away <- as.character(df$away)
    df$goal_value <- as.numeric(df$goal_value)
    home <- unique(df$home)
    away <- unique(df$away)
    homedata <- subset(df, team == home)
    awaydata <- subset(df, team == away)

    annotate_home <- 
      paste(
        home, "xG =", 
        sprintf("%.2f", round(sum(homedata$xG), 2)), 
        "Goals =", sum(homedata$goal_value)
        )
    annotate_away <- 
      paste(
        away, 
        "xG =", 
        sprintf("%.2f", round(sum(awaydata$xG), 2)), 
        "Goals =", sum(awaydata$goal_value)
        )
    
    df <- dplyr::mutate(df, tooltip = scales::percent(xG, accracy = 1))

    len <- 100
    height <- 100

    ggplot(data = df) +
      geom_rect(
        aes(xmin = 0, xmax = len, ymin = 0, ymax = height), 
        fill = "green4", colour = "white", size = 1, alpha = 0.05
        ) +
      geom_rect(
        aes(xmin = 0, xmax = len / 2, ymin = 0, ymax = height), 
        fill = NA, colour = "white", size = 1
        ) +
      geom_rect(
        aes(xmin = 0, xmax = 18, ymin = 23, ymax = height - 23), 
        fill = NA, colour = "white", size = 1
        ) +
      geom_rect(
        aes(xmin = len - 18, xmax = len, ymin = 23, ymax = height - 23), 
        fill = NA, colour = "white", size = 1
        ) +
      geom_rect(
        aes(xmin = 0, xmax = 6, ymin = 35, ymax = height - 35), 
        fill = NA, colour = "white", size = 1
        ) +
      geom_rect(
        aes(xmin = len - 6, xmax = len, ymin = 35, ymax = height - 35), 
        fill = NA, colour = "white", size = 1
        ) +
      geom_point(
        aes(x = len / 2, y = height / 2), 
        fill = "white", colour = "white", shape = 21, size = 1.5, stroke = 1
        ) +
      geom_point(
        aes(x = 13, y = height / 2), 
        fill = "white", colour = "white", shape = 21, size = 1.5, stroke = 1
        ) +
      geom_point(
        aes(x = len - 13, y = height / 2), 
        fill = "white", colour = "white", shape = 21, size = 1.5, stroke = 1
        ) +
      geom_curve(
        aes(x = 18, y = 35, xend = 18, yend = height - 35), 
        colour = "white", size = 1, curvature = 0.5
        ) +
      geom_curve(
        aes(x = len - 18, y = 35, xend = len - 18, yend = height - 35), 
        colour = "white", size = 1, curvature = -0.5
        ) +
      geom_curve(
        aes(x = len / 2, y = height / 2 - 10, xend = len / 2, yend = height / 2 + 10), 
        colour = "white", size = 1, curvature = 1
        ) +
      geom_curve(
        aes(x = len / 2, y = height / 2 - 10, xend = len / 2, yend = height / 2 + 10), 
        colour = "white", size = 1, curvature = -1
        ) +
      ggiraph::geom_point_interactive(
        aes(x = x, y = y, size = xG, 
            shape = chance_type, colour = Goal,
            tooltip = tooltip, data_id = paste0(xG, x, y, chance_type, Goal)
            ),
        position = 
          position_jitterdodge(jitter.height = 3, jitter.width = 4), 
        alpha = 0.6
      ) +
      scale_colour_manual(values = c("blue", "red")) +
      annotate("label", x = len * 0.25, y = height * 0.9, label = annotate_home, size = 3) +
      annotate("label", x = len * 0.75, y = height * 0.9, label = annotate_away, size = 3) +
      labs(shape = "Chance Type", colour = NULL) +
      theme(
        rect = element_blank(),
        line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.5, 0.2),
        legend.justification = "centre",
        legend.background = element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.key.size = unit(0.8, "lines"),
        legend.text.align = 0,
        legend.title.align = 0
      )
  }

  plotData <- reactive({
    dplyr::filter(
      mapData,
      season == input$season,
      league == input$league,
      home == input$home,
      away == input$away,
      matchDate == input$date
    )
  })

  output$xGMap <- ggiraph::renderggiraph({

    # take a dependency on the acion button
    # use isolte to avoid dependency on gamedata
    if (input$runPlot == 0) return()
    gameData <- isolate(plotData())
    ggiraph(code = print(create_shotmap(gameData)))
  })
}

# Run the application
shinyApp(ui = ui, server = server)