box::use(
  shiny[...],
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
    ui_utils$card(h1("Plants"), "Discover your plants and check weather in their places of origin")
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
