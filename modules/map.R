box::use(
  shiny[...],
  shiny.fluent[...],
  leaflet[...],
  maps[...],
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
    leaflet::leafletOutput(ns("map"), height = "600px"),
    reactOutput(ns("info"))
  )
}

#' @export
server <- function(map_id) {
  moduleServer(map_id, function(input, output, session) {
    ns <- session$ns
    data_manager <- session$userData$data_manager
    selected <- session$userData$selected
    
    output$map <- leaflet::renderLeaflet({
      req(
        length(selected()) == 1,
        data_manager()$get_plants()[[selected()]]
      )
      
      plant <- data_manager()$get_plants()[[selected()]]
      # plant <- data_manager$get_plants()[[1]]
      native <- region_to_country(plant$distributions)
      countries <- maps::world.cities %>%
        filter(capital == 1, country.etc %in% native)

      # plant_icon <- makeIcon(
      #   iconUrl = plant$image_url,
      #   iconWidth = 38,
      #   iconHeight = 95,
      #   iconAnchorX = 22,
      #   iconAnchorY = 94,
      #   className = "info-img"
      # )
      #
      icon <- makeIcon("www/plant.png", "www/plant-24@2x.png", 24, 24, className = "leaflet-icon")

      countries %>%
        select(country = country.etc, lat, lng = long) %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE) %>%
        addMarkers(
          lng = ~ lng,
          lat = ~ lat,
          label = ~ country,
          icon = icon
        )
    })
    
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (is.null(click))
        return()
      text <- paste("Lattitude ", click$lat, "Longtitude ", click$lng, "\nWheather:")
      # click <- list(lat = -15.78, lng = -47.91)
      owm_data <- owmr::get_current(lon = click$lng, lat = click$lat, units = "metric")
      proxy <- leaflet::leafletProxy("map", session)
      proxy %>% leaflet::clearPopups()
      
      proxy %>% addPopups(
        lng = click$lng,
        lat = click$lat, 
        make_whether_prompt(owm_data)
      )
      # proxy %>% owmr::add_weather(
      #   data = owm_data,
      #   lng = click$lng,
      #   lat = click$lat,
      #   template = "<b>{{name}}</b>, {{temp}}°C",
      #   icon = owm_data$weather$icon
      # )
    })
    
    output$info <- renderReact({
      req(
        length(selected()) == 1
        # data_manager()$get_plants()[[selected()]]
      )
      
      plant <- data_manager()$get_plants()[[selected()]]
      
      ui_utils$card(
        "Native",
        glue_collapse(plant$distributions, sep = ", ")
      )
    })
  })
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
