box::use(
  shiny[...],
  shiny.fluent[...],
  shiny.react[...],
  dplyr[...],
  purrr[...],
  DT[...],
  glue[...]
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
    
    searchVal <- debounce(reactive(input$search), 1000)
    searchResult <- reactiveVal()
    ids <- reactiveVal()
    data_manager <- session$userData$data_manager
    search_is_open <- session$userData$search_is_open
    
    observeEvent(searchVal(), {
      req(searchVal())
      tryCatch({
        result <- data_manager()$search(searchVal())
        searchResult(result)
      },
      error = function(e) {
        searchResult(NULL)
      })
    })
    
    output$modal <- renderReact(
      Modal(
        isOpen = search_is_open(), 
        isBlocking = FALSE,
        div(
          style = "margin: 10px;",
          h1("Search for your plant"),
          SearchBox.shinyInput(ns("search"), placeholder = "Search plant by name"),
          Separator(),
          DTOutput(ns("table"), height = "100%"),
          Separator(),
          textOutput(ns("selected")),
          Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 20),
            ActionButton.shinyInput(ns("close"), text = "Close", className = "btn-primary"),
            ActionButton.shinyInput(ns("add"), text = "Add", className = "btn-primary")
          )
        )
      )
    )
    
    observeEvent(input$add, {
      search_is_open(FALSE)
      df <- searchResult()
      ids(df[input$table_rows_selected, ]$id)
    })
    
    observeEvent(input$close, {
      search_is_open(FALSE)
    })
    
    output$table <- renderDT({
      req(searchVal(), searchResult())
      
      df <- get_search_row(searchResult())
      
      datatable(
        df,
        selection = "multiple",
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = "t",
          scrollY = "50vmin",
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
    
    return(ids)
  })
  
}

get_search_row <- function(data) {
  data %>% 
    select(image_url, scientific_name, common_name) %>% 
    mutate(image_url = glue("<img src='{image_url}' height='200', alt='No image available'></img>"))
}
