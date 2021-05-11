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
library(pool)
library(dplyr)
library(dbplyr)
library(leaflet)
library(promises)
library(future)
library(showtext)
library(ggplot2)
library(ggrepel)
library(DBI)
library(RSQLite)
library(stringr)
plan(multisession)

font_add_google("Montserrat", "Montserrat")
showtext_auto()
options(box.path = getwd())

box::use(
  modules/data_manager,
  modules/header,
  modules/sidebar,
  modules/search_modal,
  modules/home,
  modules/map,
  modules/info,
  modules/download,
  modules/error_modal
)
box::reload(data_manager)
box::reload(header)
box::reload(sidebar)
box::reload(map)
box::reload(info)
box::reload(search_modal)
box::reload(home)
box::reload(download)

router <- make_router(
  route("home", home$ui("home")),
  route("info", info$ui("info"), info$server),
  route("map", map$ui("map"), map$server)
)

addResourcePath("www", "./www")
sass(sass_file("styles/main.scss"), output = "www/style.css", cache = FALSE)
pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = config::get("db_loc")
)
onStop(function() {
  poolClose(pool)
})

ui <- fluentPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    tags$script(src = "www/script.js"),
    tags$link(rel = "shortcut icon", href = "www/favicon.ico")
  ),
  suppressDependencies("bootstrap"),
  useShinyjs(),
  tags$body(
    search_modal$ui("search_modal"),
    div(
      class = "grid-container",
      div(class = "header", header$ui()),
      div(class = "sidebar",  sidebar$ui("sidebar")),
      div(class = "main", router$ui)
    )
  )
)

server <- function(input, output, session) {

  session$userData$data_manager <- data_manager$DataManager$new(pool)$reactive()
  session$userData$search_is_open <- reactiveVal(FALSE)
  session$userData$selected <- sidebar$server("sidebar")
  ids <- search_modal$server("search_modal")
  router$server(input, output, session, info_id = "info", map_id = "map")

  observeEvent(ids(), {
    session$userData$data_manager()$add(ids())
  })
}

shinyApp(ui = ui, server = server)
