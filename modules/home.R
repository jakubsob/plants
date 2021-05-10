box::use(
  shiny[...],
  shiny.react[...],
  shiny.fluent[...],
  leaflet[...],
  maps[...],
  dplyr[...],
  purrr[...],
  glue[...],
  ui_utils = ./ui_utils,
  timeline = ./timeline
)
box::reload(timeline)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    ui_utils$card(h1("Plants"), home_text()),
    reactOutput(ns("cards")),
  )
}

#' @export
server <- function(home_id) {
  moduleServer(home_id, function(input, output, session) {
    ns <- session$ns
    data_manager <- session$userData$data_manager
    selected <- session$userData$selected
  
    output$cards <- renderReact({
      req(!data_manager()$empty())
      tagList(
        ui_utils$card(strong("Timeline"), plotOutput(ns("timeline_plot"))),
        ui_utils$card(strong("Synonyms"), "")
      )
    })
    
    output$timeline_plot <- renderPlot({
      req(!data_manager()$empty())
      timeline$make_timeline(data_manager()$get())
    }, bg = "transparent")
    
  })
}

home_text <- function() {
  tagList(
    "Discover your plants and check weather in their places of origin.",
    br(),
    "Search for your plant using Add button in pane on the left.",
    br(),
    "Navigate through tabs using menu in the header."
  )
}