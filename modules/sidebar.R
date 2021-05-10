box::use(
  shiny[...],
  shiny.fluent[...],
  shiny.react[...],
  shinyjs[...],
  purrr[...],
  magrittr[...],
  dplyr[...],
  DT[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id) 
  tagList(
    DTOutput(ns("scrollable"), height = "100%")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ids <- reactiveVal(NULL)
    data_manager <- session$userData$data_manager

    output$scrollable <- renderDT({
      items <- data_manager()$get() %>% 
        map(`[[`, "scientific_name") %>%
        unlist() %>% 
        as.data.frame()
      
      datatable(
        items,
        selection = "single",
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          scrollY = "80vmin",
          searching = FALSE,
          headerCallback = JS(
            "function(thead, data, start, end, display){",
            "  $(thead).remove();",
            "}"),
          initComplete = JS(
            "function(settings, json) {",
            "  $('body').find('.dataTables_scrollBody').addClass('scrollbar');",
            "}")
        )
      )
    })
    
    observeEvent(input$scrollable_rows_selected, {
      items <- data_manager()$get()
      ids(names(items[input$scrollable_rows_selected]))
    })
    
    return(ids)
  })
}