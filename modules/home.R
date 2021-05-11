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

home_text <- function() {
    
  div(
    class = "flex-container",
    div(
      class = "flex-child",
      tagList(
        "Discover your plants and check weather in their places of origin.",
        br(),
        "Search for your plant using Add button in pane on the left.",
        br(),
        "Navigate through tabs using menu in the header."
      )
    ),
    div(
      class = "flex-child",
      img(src = "www/plant.png", class = "home-img")
    )
  )
}