box::use(
  shiny[...],
  shiny.fluent[...],
  leaflet[...],
  maps[...],
  dplyr[...],
  purrr[...],
  glue[...],
  owmr[...],
  promises[...],
  future[...],
  ui_utils = ./ui_utils[card]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactOutput(ns("info"))
}

#' @export
server <- function(map_id) {
  moduleServer(map_id, function(input, output, session) {
    ns <- session$ns
    data_manager <- session$userData$data_manager
    selected <- session$userData$selected
    icon <- makeIcon("www/plant.png", iconWidth = 32, iconHeight = 32)
    
    output$info <- renderReact({
      req(!data_manager()$empty())
      if (length(selected()) != 1) return(empty_info_card())
      plant <- data_manager()$get(selected())
      tagList(
        leafletOutput(ns("map"), height = "600px"),
        ui_utils$card(
          "Native",
          glue_collapse(plant$distributions, sep = ", ")
        )
      )
    })
    
    output$map <- renderLeaflet({
      req(!data_manager()$empty())
      
      plant <- data_manager()$get(selected())
      native <- region_to_country(plant$distributions)
      countries <- maps::world.cities %>%
        filter(capital == 1, country.etc %in% native)
      
      countries %>%
        select(country = country.etc, lat, lng = long) %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMarkers(
          lng = ~ lng,
          lat = ~ lat,
          label = ~ country,
          icon = icon
        )
    })
    
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (is.null(click)) return()
      proxy <- leafletProxy("map", session)
      proxy %>% clearPopups()
      
      future_promise({
        resp <- get_current(lon = click$lng, lat = click$lat, units = "metric")
        
        addPopups(
          map = proxy,
          lng = click$lng,
          lat = click$lat, 
          popup = if (resp$cod == 200) make_whether_prompt(resp) else resp$message
        ) 
      })
    })
  })
}

empty_info_card <- function() {
  ui_utils$card("Empty", "Select a plant from sidebar menu.", style = "height: 600px;")
}

region_to_country <- function(country) {
  countries <- unique(maps::world.cities$country.etc)
  map_chr(country, function(cn) {
    res <- countries[map_lgl(countries, ~ grepl(.x, cn))][1]
    if (length(res) == 0) return("")
    res
  })
}

make_whether_prompt <- function(owm_data) {
  glue("
    {icon(leaflet::icons(owmr::get_icon_url(owm_data$weather$icon)))}
    <b>{stringr::str_to_sentence(owm_data$weather$description)}</b></br>
    <b>Temp:</b> {owm_data$main$temp} °C</br>
    <b>Humidity:</b> {owm_data$main$humidity}</br>
    <b>Wind speed:</b> {owm_data$wind$speed}</br>
  ")
}
