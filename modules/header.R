box::use(
  shiny[...],
  shiny.fluent[...],
  shiny.router[route_link]
)

#' @export
ui <- function() {
  
  logo <- img(src = "www/plant.png", class = "logo")
  title <- div(
    Text(variant = "xLarge", "Plants"),
    class = "title"
  )
  
  tagList(
    logo,
    title,
    Stack(
      horizontal = TRUE,
      div(
        class = "header-buttons",
        CommandBarButton.shinyInput(
          "home",
          key = "home",
          text = "Home",
          iconProps = list(iconName = "Home"),
          href = route_link("home")
        ),
        CommandBarButton.shinyInput(
          "info",
          key = "info",
          text = "Info",
          iconProps = list(iconName = "ContactInfo"),
          href = route_link("info")
        ),
        CommandBarButton.shinyInput(
          "map",
          key = "map",
          text = "Map",
          iconProps = list(iconName = "Nav2DMapView"),
          href = route_link("map")
        ),
        CommandBarButton.shinyInput(
          "upload",
          text = "Upload",
          iconProps = list(iconName = "Upload")
        ),
        CommandBarButton.shinyInput(
          "download",
          text = "Download",
          iconProps = list(iconName = "Download")
        )
      )
    )
  )
}