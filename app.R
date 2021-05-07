library(shiny)
library(shiny.fluent)
library(shiny.react)
library(shiny.router)
library(shinyjs)
library(sass)
library(purrr)
library(R6)
library(owmr)
options(shiny.launch.browser = TRUE)

box::use(
  modules/header,
  modules/sidebar,
  modules/search_modal,
  modules/home,
  modules/map,
  modules/info,
  modules/error_modal
)
box::reload(header)
box::reload(sidebar)
box::reload(map)
box::reload(info)
box::reload(search_modal)
box::reload(home)

source("modules/data_manager.R")

router <- make_router(
  route("home", home$ui("home")),
  route("info", info$ui("info"), info$server),
  route("map", map$ui("map"), map$server)
)

addResourcePath("www", "./www")
sass(sass_file("styles/main.scss"), output = "www/style.css", cache = FALSE)

# data_manager <- DataManager$new("data/plants.sqlite")
# res <- data_manager$search("rhipsalis")
# data_manager$add(c("101119", "174297", "295307"))
# data_manager$get_plants()

ui <- fluentPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    tags$script(src = "www/script.js")
  ),
  suppressDependencies("bootstrap"),
  useShinyjs(),
  tags$body(
    search_modal$ui("search_modal"),
    div(
      class = "grid-container",
      div(class = "header", header$ui()),
      div(class = "sidebar", 
        sidebar$ui("sidebar"),
        div(
          class = "sidebar-buttons",
          ActionButton.shinyInput(
            "add",
            text = "Add",
            iconProps = list(iconName = "Add"),
            styles = list("width: 100px")
          ),
          ActionButton.shinyInput(
            "remove",
            text = "Remove",
            iconProps = list(iconName = "Remove"),
            styles = list("width: 100px")
          )
        )
        # Separator("dev"),
        # ActionButton.shinyInput("add_plant", text = "add")
      ),
      div(class = "main", router$ui)
    )
  )
)

server <- function(input, output, session) {

  session$userData$search_is_open <- reactiveVal(FALSE)
  session$userData$data_manager <- DataManager$new("data/plants.sqlite")$reactive()
  
  session$userData$selected <- sidebar$server("sidebar")
  ids <- search_modal$server("search_modal", plants)
  router$server(input, output, session, info_id = "info", map_id = "map")
  
  observeEvent(ids(), {
    session$userData$data_manager()$add(ids())
  })
  
  observeEvent(input$add, {
    session$userData$search_is_open(TRUE)
  })
  
  observeEvent(input$remove, {
    session$userData$data_manager()$remove(session$userData$selected())
  })
  
  observeEvent(input$add_plant, {
    session$userData$data_manager()$add(c("101119", "174297", "295307"))
  })
}

shinyApp(ui = ui, server = server)

