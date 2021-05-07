box::use(
  promises[...],
  future[...],
  shiny[...],
  shiny.fluent[...],
  shiny.react[...],
  dplyr[...],
  purrr[...],
  DT[...],
  glue[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactOutput(ns("modal"))
}

#' @export
server <- function(id, open) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$modal <- renderReact({
      browser()
      Modal(
        isOpen = open(), 
        isBlocking = FALSE,
        h1("Error"),
        DefaultButton.shinyInput(ns("close"), "Close")
      )
    })
    
    observeEvent(input$close, {
      open(FALSE)
    })
  })
}
