box::use(
  shiny[...],
  shiny.fluent[...],
  shiny.react[...],
  dplyr[...],
  purrr[...],
  glue[...],
  stringr[str_to_title],
  ui_utils = ./ui_utils[card],
  timeline = ./timeline
)

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
      req(!data_manager()$empty())
      if (length(selected()) != 1) return(empty_info_card())
      plant <- data_manager()$get(selected())
      
      tagList(
        ui_utils$card(
          "",
          make_title(plant)
        ),
        ui_utils$card(
          strong("Info"),
          make_info_list(plant)
        ),
        ui_utils$card(
          strong("Synonyms"),
          glue_collapse(plant$synonyms, sep = ", ")
        ),
        ui_utils$card(
          strong("Common names"),
          glue_collapse(plant$common_names, sep = ", ")
        ),
        ui_utils$card(
          strong("Publication year timeline"), 
          plotOutput(ns("timeline_plot"))
        )
      )
    })
    
    output$timeline_plot <- renderPlot({
      req(!data_manager()$empty())
      timeline$make_timeline(data_manager()$get(), selected())
    }, bg = "transparent")
  })
}

empty_info_card <- function() {
  ui_utils$card("Empty", "Select a plant from sidebar menu.")
}

make_title <- function(plant) {
  div(
    class = "info-title",
    div(
      style = "info-image-container",
      img(class = "info-img", src = plant$image_url, alt = "No image available")
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

make_items <- function(items) {
  imap(items, function(item, key) {
    if (!is.null(item) && grepl("^http", item)) item <- Link(item)
    span(strong(key_to_word(key)), ": ", item)
  })
}

key_to_word <- function(key) {
  key <- strsplit(key, "_")[[1]] %>% 
    str_to_title() %>% 
    glue_collapse(sep = " ")
}

make_info_list <- function(plant, groups = c("foliage", "flower", "fruit", "growth", "edible", "planting", "links")) {
  items <- plant[groups] %>% 
    map(function(x) keep(x, ~ !is.na(.x))) %>% 
    compact()
  groups <- names(items)
  counts <- unname(map_int(items, length))
  starts <- cumsum(counts) - counts
  items <- flatten(items) %>% make_items() %>% unname()
  GroupedList(
    items = items,
    groups = pmap(
      list(name = groups, start = starts, count = counts),
      function(name, start, length, count) {
        list(
          key = name,
          name = str_to_title(name),
          startIndex = start,
          count = count,
          isCollapsed = TRUE
        )
      }
    ) %>% 
      unname(),
    selectionMode = 0,
    onRenderCell = JS("(depth, item) => 
      React.createElement('span', { style: { paddingLeft: 49 } }, item)")
  )
}


