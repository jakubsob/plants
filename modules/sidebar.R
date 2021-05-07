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

scrollable_personas <- function(personas, ns) {
  purrr::map(personas, function(p) {
    tagList(
      div(
        id = p$id,
        class = "clickable-persona",
        style = "cursor:pointer",
        Persona(
          imageUrl = p$imageUrl,
          primaryText = p$primaryText,
          secondaryText = p$secondaryText
        )
      ),
      Separator()
    )
  })
}

# ScrollablePersonas <- function(id, personas) {
#   div(
#     class = "scrollable-pane",
#     
#   )
# }

#' @export
ui <- function(id) {
  ns <- NS(id)
  # reactOutput(ns("scrollable"))
  tagList(
    DTOutput(ns("scrollable"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ids <- reactiveVal(NULL)
    data_manager <- session$userData$data_manager
    
    # output$scrollable <- renderReact({
    #   # scrollable <- items() %>% 
    #   #   scrollable_personas(ns)
    #   
    #   tagList(
    #     # div(
    #     #   class = "scrollable-pane",
    #     #   scrollable
    #     # ),
    #     Stack(
    #       horizontal = TRUE,
    #       tokens = list(childrenGap = 20),
    #       ActionButton.shinyInput(ns("select_all"), text = "Select all", class = "btn-primary"),
    #       ActionButton.shinyInput(ns("clear_all"), text = "Clear selection", className = "btn-primary")
    #     )
    #   )
    # })

    output$scrollable <- renderDT({
      items <- data_manager()$get_plants() %>% 
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
    
    observeEvent(input$scrollable_personas_active, {
      print(input$scrollable_personas_active)
    })
    
    observeEvent(input$scrollable_rows_selected, {
      
      items <- data_manager()$get_plants()
      ids(names(items[input$scrollable_rows_selected]))
    })
    
    return(ids)
  })
}