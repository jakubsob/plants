box::use(
  shiny[...],
  shiny.react[...],
  shiny.fluent[...],
  glue[...]
)

#' @export
card <- function(title = NULL, content = NULL, size = 12, style = "") {
  div(
    class = glue("card ms-depth-16 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      if (!is.null(title)) Text(variant = "large", title, block = TRUE),
      content
    )
  )
}