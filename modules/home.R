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
ui <- function() {
  div(
    class = "card ms-depth-16 ms-sm12 ms-xl12 flex-container",
    div(
      class = "flex-child",
      Stack(
        horizontal = FALSE,
        tokens = list(childrenGap = 10),      
        Text(variant = "large", h1("Plants"), block = TRUE),
        span("Have you ever wanted to have a better relationship with your plants?"),
        span("Learn about your plants so you can provide them better conditions"),
        br(),
        h3("How to use"),
        span("Use sidebar on the left to search for your plants in the database. Once added they will be listed in the menu"),
        span("Learn about your plants in ", strong(Icon(iconName = "ContactInfo")), " Info tab"),
        span("To see where your plant originates from see ", strong(Icon(iconName = "Nav2DMapView")), " Map tab.",
             "This is where you can look up weather from places of origin to provide conditions and make your plant feel like at home"
        ),
        span("Once plants are added you can download the list and upload it next time in order not to search the plants once again")
      )
    ),
    div(
      class = "flex-push",
      img(src = "www/plant.png", class = "home-img")
    )
  )
}
