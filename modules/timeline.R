box::use(
  dplyr[...],
  purrr[...],
  tibble[...],
  ggplot2[...],
  ggimage[...],
  ggrepel[...]
)

#' @export
make_timeline <- function(plants, bg_color = "#e5e6e0", color = "#182c2b") {
  d <- plants %>% 
    map(`[`, c("year", "scientific_name", "image_url")) %>% 
    unname() %>% 
    transpose() %>% 
    as_tibble() %>% 
    mutate_all(unlist) %>% 
    mutate(
      color = color,
      text_pos = rep_len(c(1, -1), nrow(.))
    )
  
  d %>% 
    ggplot(aes(x = year, y = 0, colour = color)) +
    geom_hline(yintercept = 0, colour = color) +
    geom_point(size = 3) +
    geom_label_repel(
      aes(y = 0, label = scientific_name),
      color = color,
      fill = bg_color,
      size = 5,
      direction = "y",
      min.segment.length = 0,
      nudge_y = 0.05
    ) +
    coord_fixed() +
    scale_y_continuous(limit = c(-0.001, .1)) +
    scale_colour_manual(values = color) +
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
