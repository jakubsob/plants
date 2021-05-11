box::use(
  shiny[...],
  shiny.react[...],
  shiny.fluent[...],
  leaflet[...],
  maps[...],
  dplyr[...],
  purrr[...],
  glue[...],
  ui_utils = ./ui_utils
)

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