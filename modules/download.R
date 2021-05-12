box::use(
  shiny[...],
  shinyjs[click],
  glue[glue],
  utils[...]
)

#' @export
server <- function(input, output, session) {
  data_manager <- session$userData$data_manager
  
  observeEvent(input$download, {
    click("download_data")
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      glue("plants-{Sys.Date()}.csv")
    },
    content = function(file) {
      df <- data_manager()$get_df()
      write.csv2(df, file, row.names = FALSE)
    }
  )
}