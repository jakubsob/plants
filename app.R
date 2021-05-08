library(shiny)
library(shiny.fluent)
library(shiny.react)
library(shiny.router)
library(shinyjs)
library(sass)
library(R6)
library(owmr)
library(purrr)
library(DT)
library(glue)
library(dplyr)
library(dbplyr)
library(leaflet)

options(box.path = getwd())

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

router <- make_router(
  route("home", home$ui("home")),
  route("info", info$ui("info"), info$server),
  route("map", map$ui("map"), map$server)
)

addResourcePath("www", "./www")
sass(sass_file("styles/main.scss"), output = "www/style.css", cache = FALSE)
source("modules/data_manager.R")

# data_manager <- DataManager$new("data/plants.sqlite")
# res <- data_manager$search("syngonium")
# data_manager$add("295307")
# data_manager$add("101119")
# data_manager$add("186087")
# data_manager$get()

ui <- fluentPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    tags$script(src = "www/script.js"),
    tags$link(rel="shortcut icon", href="www/favicon.ico")
  ),
  suppressDependencies("bootstrap"),
  useShinyjs(),
  tags$body(
    search_modal$ui("search_modal"),
    div(
      class = "grid-container",
      div(class = "header", header$ui()),
      div(class = "sidebar", 
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
        ),
        sidebar$ui("sidebar"),
        Separator("dev"),
        ActionButton.shinyInput("add_plant", text = "add")
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
    if (session$userData$data_manager()$empty()) return()
    session$userData$data_manager()$remove(session$userData$selected())
  })
  
  observeEvent(input$add_plant, {
    ids <- session$userData$data_manager()$search("syngonium")$id[1:15]
    session$userData$data_manager()$add(ids)
  })
}

shinyApp(ui = ui, server = server)
