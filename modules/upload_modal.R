box::use(
  shiny[...],
  shiny.fluent[...],
  shiny.react[...],
  shinymaterial[...],
  dplyr[...],
  purrr[...],
  DT[...],
  glue[...],
  utils[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactOutput(ns("modal"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_manager <- session$userData$data_manager
    upload_is_open <- session$userData$upload_is_open
    loaded_data <- reactiveVal()
    
    output$modal <- renderReact(
      Modal(
        isOpen = upload_is_open(), 
        isBlocking = FALSE,
        div(
          style = "margin: 10px;",
          h1("Upload list of your plants"),
          fileInput(ns("file_input"), label = NULL, accept = ".csv", width = "100%"),
          Separator(),
          Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 20),
            ActionButton.shinyInput(ns("close"), text = "Close", className = "btn-primary"),
            ActionButton.shinyInput(ns("add"), text = "Add", className = "btn-primary")
          )
        )
      )
    )
    
    observeEvent(input$upload, {
      upload_is_open(TRUE)
    })
    
    observeEvent(input$close, {
      upload_is_open(FALSE)
    })
    
    observeEvent(input$add, {
      upload_is_open(FALSE)
      tryCatch(
        session$userData$data_manager()$add(loaded_data()$id),
        error = function(e) {
          dm_add_error_notification()
        }
      )
    })
    
    observeEvent(input$file_input, {
      tryCatch(
        loaded_data(read.csv2(input$file_input$datapath)),
        error = function(e) {
          error_notification()
        }
      )
    })
  })
}

load_error_notification <- function() {
  ui <- tagList(
    strong("Error reading the data"),
    br(),
    "Make sure the .csv file contains 'id' column and is semi-colon separated"
  )
  showNotification(ui)
}

dm_add_error_notification <- function() {
  ui <- tagList(
    strong("Error adding the data"),
    br(),
    "Check whether plants id's are not corrupted"
  )
  showNotification(ui)
}
