box::use(
  shiny[...],
  shiny.react[...],
  shiny.fluent[...],
  dm = ./data_manager
)

#' @export
server <- function(input, output, session) {
  # output$download <- downloadHandler(
  #   filename = function() {
  #     "x"
  #   },
  #   content = function() {
  #     write.csv(mtcars, file)
  #   }
  # )
}