
#' Tema con la paleta de colores institucionales en una versión obscura
#'
#' @param base_size tamaño de letra base
#' @param base_family familia tipográfica
#' @param fondo color del fondo del plot
#'
#' @return
#' @export
#'
#' @examples
theme_morant <- function(base_size = 12, base_family = "Futura", fondo="#F7F9EC") {
  colores <- tibble::deframe(temario::colores[["morant"]])
  (theme_foundation(base_size = base_size,
                    base_family = base_family)
    + theme(
      line = element_line(colour = colores["gris_claro"]),
      rect = element_rect(fill = fondo,
                          linetype = 0,
                          colour = NA),
      text = element_text(colour = colores["gris_medio"]),
      axis.title = element_blank(),
      axis.text = element_text(),
      axis.ticks = element_blank(),
      axis.line = element_line(colour = colores["principal"]),
      legend.background = element_rect(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      panel.grid = element_line(colour = NULL),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x =element_line(colour = colores["gris_claro"],
                                       linetype = "dotted"),
      panel.grid.minor = element_blank(),
      # unfortunately, can't mimic subtitles TODO!
      plot.title = element_text(hjust = 0,
                                size = rel(1.5),
                                face = "bold",
                                colour = colores["gris_obscuro"]),
      plot.subtitle = element_text(hjust = 0,
                                size = rel(1),
                                face = "bold",
                                colour = colores["gris_claro"]),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.text=element_text(colour =colores["terciario"], face = "bold"),
      strip.background = element_rect(fill=colores["secundario"])))
}

#' morant color palette
#'
#' The standard three-color morant palette for line plots comprises
#' blue, red, and green.
#'
#' @family colour morant
#' @export
#' @example inst/examples/ex-morant_pal.R
morant_pal <- function() {
  colors <- tibble::deframe(temario::colores[["morant"]] %>%
                              filter(str_detect(nombre, pattern = "^paleta_")))
  values <- unname(colors)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

#' morant color scales
#'
#' Color scales using the colors in the morant graphics.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour morant
#' @rdname scale_morant
#' @seealso \code{\link{theme_morant}()} for examples.
#' @export
scale_colour_morant <- function(...) {
  discrete_scale("colour", "morant", morant_pal(), ...)
}

#' @rdname scale_morant
#' @export
scale_color_morant <- scale_colour_morant

#' @rdname scale_morant
#' @export
scale_fill_morant <- function(...) {
  discrete_scale("fill", "morant", morant_pal(), ...)
}

