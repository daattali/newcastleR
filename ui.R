fluidPage(

  shinyjs::useShinyjs(),

  titlePanel("EPL 2019-2020 (data from fbref.com)"),

  tabsetPanel(

    tabPanel("Explore players",
             fluidRow(
               column(
                 4,
                 numericInput("num_matches", "Minimum matches", 5)
               ),
               column(4, selectInput("position",
                                     "Position",
                                     c("All", "Goalkeepers", "Defenders", "Midfielders", "Forwards")))
             ),
             DT::DTOutput("main_data"),
             br(),
             actionButton("predict", "Predict player goals next season"),
             textOutput("prediction", inline = TRUE)
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
        column(4, sliderInput("plot_theme_size", "Text size", 10, 30, 12)),
        column(4, colourpicker::colourInput("plot_marker_col", "Marker colour", "#471D85"))
      ) %>% shinyjs::hidden(), br(),
      actionButton("do_plot", "Plot!"),
      plotOutput("plot")
    )
  ),

  br(), br(),
  "For best experience, please don't use Internet Explorer"
)
