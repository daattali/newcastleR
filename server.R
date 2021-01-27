function(input, output, session) {

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

    data <- dplyr::mutate(
      data,
      ".highlight" = age <= 32 & data$goals >= 10 & data$minutes < 2000
    )

    names(data) <- lapply(names(data), function(x) {
      gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", gsub("_", " ", x), perl = TRUE)
    })

    DT::datatable(
      data,
      selection = "single",
      options = list(
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(2, 13))
        )
      )
    ) %>%
      DT::formatStyle(
        ".highlight",
        target = "row",
        backgroundColor = DT::styleEqual(TRUE, "#bcffc4")
      )
  })

  output$prediction <- renderText({
    req(input$predict)

    isolate({
      player <- filtered_data()[input$main_data_rows_selected, ]
      name <- player$player
      goals <- player$goals
      if (grepl("FW", player$position)) {
        if (player$age <= 25) {
          goals <- goals + 4
        } else if (player$age <= 32) {
          goals <- goals + 1
        } else {
          goals <- goals - 2
        }
      } else if (player$position == "GK") {
        stop("Our prediction algorithm breaks for goalkeepers")
      } else {
        goals <- max(0, goals + sample(-2:2, 1))
      }

      paste("Expected goals next season:", goals)
    })
  })

  output$teams_selector_ui <- renderUI({
    selectInput("plot_team", "Team", choices = c("All", unique(players_data$team)), "All", multiple = TRUE)
  })

  observeEvent(input$do_plot, {
    output$plot <- renderPlot({
      data <- players_data
      if (!"All" %in% input$plot_team) {
        data <- dplyr::filter(data, team %in% input$plot_team)
      }

      ggplot(data, aes_string(tolower(input$xvar), tolower(input$yvar))) +
        geom_point(size = input$plot_marker_size, col = input$plot_marker_col, alpha = 0.4) +
        theme_bw(input$plot_theme_size)
    })
  })

  observeEvent(input$plot_show_options, {
    shinyjs::toggle("plot_options")
  })
}
