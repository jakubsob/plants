box::use(
  shiny[...],
  shiny.fluent[...],
  shiny.react[...],
  dplyr[...],
  purrr[...],
  glue[...],
  ui_utils = ./ui_utils[card]
)
box::reload(ui_utils)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactOutput(ns("info"))
  )
}

#' @export
server <- function(info_id) {
  moduleServer(info_id, function(input, output, session) {
    ns <- session$ns
    data_manager <- session$userData$data_manager
    selected <- session$userData$selected
    
    output$info <- renderReact({
      # req(selected(), data_manager()$get_plants())
      # if (length(data_manager()$get_plants()) == 0) {
      #   card <- ui_utils$card("Empty", "Add plants using sidebar menu first.")
      #   return(card)
      # } 
      if (length(selected()) != 1) {
        card <- ui_utils$card("Empty", "Select a plant from sidebar menu.")
        return(card)
      }
      plant <- data_manager()$get_plants()[[selected()]]
      # plant <- data_manager$get_plants()[[1]]
      
      tagList(
        ui_utils$card(
          "",
          make_title(plant)
        ),
        ui_utils$card(
          "Info",
          make_info_list(plant)
        ),
        ui_utils$card(
          "Synonyms",
          glue_collapse(plant$synonyms, sep = ", ")
        ),
        ui_utils$card(
          "Common names",
          glue_collapse(plant$common_names, sep = ", ")
        )
      )
    })
  })
}

make_title <- function(plant) {
  div(
    class = "info-title",
    div(
      style = "info-image-container",
      img(class = "info-img", src = plant$image_url)
    ),
    div(
      class = "info-details-container",
      div(class = "info-details-title", plant$scientific_name),
      div(class = "info-details-item", glue("Common name: {plant$common_name}")),
      div(class = "info-details-item", glue("Genus: {plant$genus}")),
      div(class = "info-details-item", glue("Family: {plant$family}")),
      div(class = "info-details-item", glue("Family common name: {plant$common_name}")),
      div(class = "info-details-item", glue("Publication year: {plant$year}"))
    )
  )
}

make_info_list <- function(plant) {
  # plant <- data_manager$get_plants()[[1]]
  # groups <- c("links")
  groups <- c("foliage", "flower", "fruit", "growth", "edible", "planting", "links")
  items <- plant[groups]
  counts <- unname(map_int(items, length))
  starts <- cumsum(counts) - counts
  items <- purrr::flatten(items) %>% make_items() %>% unname()
  GroupedList(
    items = items,
    groups = pmap(
      list(name = groups, start = starts, count = counts),
      function(name, start, length, count) {
        list(
          key = name,
          name = stringr::str_to_title(name),
          startIndex = start,
          count = count,
          isCollapsed = TRUE
        )
      }
    ) %>% unname(),
    selectionMode = 0,
    onRenderCell = JS("(depth, item) => 
      React.createElement('span', { style: { paddingLeft: 49 } }, item)")
  )
}

make_items <- function(items) {
  imap(items, function(item, key) {
    if (!is.null(item) && grepl("^http", item)) item <- Link(item)
    span(strong(key_to_word(key)), ": ", item)
  })
}

key_to_word <- function(key) {
  key <- strsplit(key, "_")[[1]] %>% 
    stringr::str_to_title() %>% 
    glue::glue_collapse(sep = " ")
}


