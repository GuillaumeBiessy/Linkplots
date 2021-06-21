#' LinkPact ggplot theme
#'
#' @inheritParams ggplot2::theme_bw
#'
#' @return A complete ggplot2 theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' theme_LinkPact() |> theme_set()
#'
#' mtcars2 <- within(mtcars, {
#'   vs <- factor(vs, labels = c("V-shaped", "Straight"))
#'   am <- factor(am, labels = c("Automatic", "Manual"))
#'   cyl  <- factor(cyl)
#'   gear <- factor(gear)
#' })
#'
#' (p1 <- ggplot(mtcars2) +
#'     geom_point(aes(x = wt, y = mpg, colour = gear)) +
#'     labs(title = "Fuel economy declines as weight increases",
#'          subtitle = "(1973-74)",
#'          caption = "Data from the 1974 Motor Trend US magazine.",
#'          x = "Weight (1000 lbs)",
#'          y = "Fuel economy (mpg)",
#'          colour = "Gears"))
#'
#' p1 + facet_grid(vs ~ am)
theme_LinkPact <- function (base_size = 12,
                            base_family = "serif",
                            base_line_size = base_size / 24,
                            base_rect_size = base_size / 24)
{
  half_line <- base_size / 2

  lkp_blue  <- rgb(0, 34, 93, maxColorValue = 255)  # Bleu LinkPact
  lkp_green <- rgb(0, 136, 81, maxColorValue = 255) # Vert LinkPact

  ggplot2::theme_bw(base_size, base_family, base_line_size, base_rect_size) +
    theme(text = element_text(family = base_family, face = "plain",
                              colour = lkp_blue, size = base_size, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                              debug = FALSE),
          title = element_text(face = "bold"),
          axis.text = element_text(size = rel(0.8), colour = "grey30", face = "bold"),
          axis.ticks = element_line(colour = "grey20"),
          axis.title = element_text(size = rel(1.2)),
          legend.background = element_rect(colour = "grey85"),
          legend.spacing = unit(half_line, "pt"),
          legend.key.size = unit(1.2, "lines"),
          legend.text = element_text(size = rel(0.8)),
          legend.position = "right",
          legend.justification = "center",
          legend.box.spacing = unit(half_line, "pt"),
          panel.grid = element_line(colour = "grey92", size = rel(1.5)),
          panel.grid.minor = element_line(size = rel(0.5)),
          panel.spacing = unit(half_line, "pt"),
          panel.ontop = FALSE,
          strip.background = element_rect(fill = lkp_green, colour = "black"),
          strip.text = element_text(colour = "white", face = "bold", size = rel(0.8),
                                    margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
          strip.switch.pad.grid = unit(half_line / 2, "pt"),
          strip.switch.pad.wrap = unit(half_line / 2, "pt"),
          plot.background = element_rect(colour = "white"),
          plot.title = element_text(size = rel(1.4), hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.title.position = "panel",
          plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(b = half_line), color = lkp_green),
          plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line), color = lkp_green),
          plot.caption.position = "panel",
          plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
          plot.tag.position = "topleft",
          plot.margin = margin(half_line, 2 * half_line, half_line, half_line))
}
