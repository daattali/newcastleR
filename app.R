library(shiny)
library(ggplot2)
library(magrittr)

# first parameter on new line so it doesnt go very far
# encoding ("r readcsv letters with accents")
# read csv outside of server function
# use shinybrowser to warn if on IE
# add a min and max to min_matches
# anything we do on client side (on the browser) is only for user friendliness - we always have to check in shiny on the server to make sure the input is fine. For example if i type a letter or a larger number, I'll get NA or a larger number
# FYI - I could even use javascript to send a value to shiny. Just be aware of that and remember that you can't trust any value coming from the brwoser - every check has to be made on the server side
# even better in my opinion - but this is completely subjective - use a slider. sliders and numeric inputs essentially provide the same functionality but using a different user experience. I use sliders when there aren't a lot of values
# defensive programming - add an `else` when you reach a place you think you should never reach and throw an error
# add a fixed column
# remove the row number
# dont use magic numbers for autowidth
# dont use magic strings for positions list
# make the plot button btn-lg and btn-primary

ui <- fluidPage(

  shinyjs::useShinyjs(),

  titlePanel("EPL 2019-2020 (data from fbref.com)"),

  tabsetPanel(

    tabPanel(
      "Explore players",
      fluidRow(
        column(
          4,
          numericInput("num_matches", "Minimum matches", 5)
        ),
        column(4, selectInput("position",
                              "Position",
                              c("All", "Goalkeepers", "Defenders", "Midfielders", "Forwards")))
      ),
      DT::DTOutput("main_data")
    ),

    tabPanel(
      "Plot",
      fluidRow(
        column(
          4,
          uiOutput("teams_selector_ui")
        ),
        column(
          4,
          selectInput("xvar", "X axis", c("Goals", "Assists", "Minutes", "Age"), selected = "Goals")
        ),
        column(
          4,
          selectInput("yvar", "X axis", c("Goals", "Assists", "Minutes", "Age"), selected = "Minutes")
        )
      ),
      actionLink("plot_show_options", "Show plot options"),
      fluidRow(
        id = "plot_options",
        column(4, sliderInput("plot_marker_size", "Marker size", 1, 5, 1)),
        column(4, colourpicker::colourInput("plot_marker_col", "Marker colour", "#471D85")),
        column(4, colourpicker::colourInput("plot_col", "Plot colour", "#EDEEFF"))
      ) %>% shinyjs::hidden(), br(),
      actionButton("do_plot", "Plot!"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {

  players_data <- read.csv("data/playersdata.csv")

  filtered_data <- reactive({
    data <- players_data

    data <- data %>%
      dplyr::filter(matches_played >= input$num_matches)

    if (input$position == "All") {
      # No filter
    } else if (input$position == "Goalkeepers") {
      data <- data %>%
        dplyr::filter(grepl("GK", position))
    } else if (input$position == "Defenders") {
      data <- data %>%
        dplyr::filter(grepl("DF", position))
    } else if (input$position == "Midfielders") {
      data <- data %>%
        dplyr::filter(grepl("MF", position))
    } else if (input$position == "Forwards") {
      data <- data %>%
        dplyr::filter(grepl("FW", position))
    }

    data
  })

  output$main_data <- DT::renderDT({
    data <- filtered_data()

    data$HIGHLIGHT <- data$age <= 32 & data$goals >= 10 & data$minutes < 2000

    names(data) <- lapply(names(data), function(x) {
      gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", gsub("_", " ", x), perl = TRUE)
    })

    DT::datatable(
      data,
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(width = 150, targets = c(1, 4)),
          list(visible = FALSE, targets = 13)
        )
      )
    ) %>%
      DT::formatStyle(
        "HIGHLIGHT",
        target = "row",
        backgroundColor = DT::styleEqual(TRUE, "#bcffc4")
      )
  })

  output$teams_selector_ui <- renderUI({
    selectInput("plot_team", "Team", choices = c("All" = "", unique(players_data$team)), multiple = TRUE)
  })

  observeEvent(input$do_plot, {
    output$plot <- renderPlot({
      data <- players_data
      if (length(input$plot_team) > 0) {
        data <- dplyr::filter(data, team %in% input$plot_team)
      }

      ggplot(data, aes_string(tolower(input$xvar), tolower(input$yvar))) +
        geom_point(size = input$plot_marker_size, col = input$plot_marker_col, alpha = 0.4) +
        theme(panel.background = element_rect(fill = input$plot_col))
    })
  })

  observeEvent(input$plot_show_options, {
    shinyjs::toggle("plot_options")
  })
}

shinyApp(ui, server)
