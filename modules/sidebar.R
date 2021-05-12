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
    div(
      class = "sidebar-buttons",
      ActionButton.shinyInput(
        ns("add"),
        text = "Add",
        iconProps = list(iconName = "Add"),
        styles = list("width: 100px")
      ),
      ActionButton.shinyInput(
        ns("remove"),
        text = "Remove",
        iconProps = list(iconName = "Remove"),
        styles = list("width: 100px")
      )
    ),
    DTOutput(ns("scrollable"), height = "100%"),
    { 
      if (config::is_active("test")) {
        ActionButton.shinyInput(ns("test_add"), text = "add")
      } else {
        NULL
      }
    }
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected <- reactiveVal(NULL)
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
            "}")
        )
      )
    })
    
    observeEvent(input$scrollable_rows_selected, {
      items <- data_manager()$get()
      selected(names(items[input$scrollable_rows_selected]))
    })
    
    observeEvent(input$add, {
      session$userData$search_is_open(TRUE)
    })
    
    observeEvent(input$remove, {
      if (session$userData$data_manager()$empty()) return()
      session$userData$data_manager()$remove(session$userData$selected())
    })
    
    observeEvent(input$test_add, {
      ids <- session$userData$data_manager()$search("syngonium")$id[1:15]
      session$userData$data_manager()$add(ids)
    })
    
    return(selected)
  })
}