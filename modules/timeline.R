box::use(
  dplyr[...],
  purrr[...],
  tibble[...],
  ggplot2[...],
  ggrepel[...]
)

#' @export
make_timeline <- function(plants,
                          selected = NULL,
                          bg_color = "#e5e6e0",
                          color = "#182c2b",
                          bg_color_selected = "#577f67",
                          color_selected = "#fcf7f4") {
  d <- plants %>% 
    map(`[`, c("id", "year", "scientific_name")) %>% 
    unname() %>% 
    transpose() %>% 
    as_tibble() %>% 
    mutate_all(unlist) %>% 
    {
      if (is.null(selected)) {
        mutate(., color = "ns", bg_color = "ns")
      } else {
        mutate(
          .,
          color = ifelse(id == selected, "s", "ns"),
          bg_color = ifelse(id == selected, "s", "ns")
        )
      }
    }
  
  d %>% 
    ggplot(aes(x = year, y = 0, colour = color)) +
    geom_hline(yintercept = 0, colour = color) +
    geom_point(size = 3, color = color) +
    geom_label_repel(
      aes(y = 0, label = scientific_name, fill = bg_color, color = color, segment.color = "black"),
      size = 5,
      direction = "y",
      min.segment.length = 0,
      nudge_y = 0.05
    ) +
    coord_fixed() +
    scale_y_continuous(limit = c(-0.001, .1)) +
    scale_colour_manual(values = c(s = color_selected, ns = color)) +
    scale_fill_manual(values = c(s = bg_color_selected, ns = bg_color)) +
    scale_x_continuous(expand = c(.3, .3)) +
    theme_classic() +
    theme(
      aspect.ratio = 0.3,
      text = element_text(family = "Montserrat"),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(colour = color),
      axis.ticks.x = element_line(colour = color),
      axis.line.x = element_blank(),
      panel.background = element_rect(fill = bg_color, colour = bg_color),
      plot.background = element_rect(fill = bg_color, colour = bg_color),
      legend.position = "none"
    )
}
