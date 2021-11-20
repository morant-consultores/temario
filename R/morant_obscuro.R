# library(ggplot2)
# library(tidyverse)
#
#
#
# data <- data.frame(
#   name=c("A","B","C","D","E") ,
#   value=c(3,12,5,18,45)
# )
#
# colores <- tibble::deframe(temario::colores[["morant"]])
#
# library(showtext)
# showtext_auto()
#
# ggplot(data, aes(x=name, y=value)) +
#   geom_bar(stat = "identity", fill = "#FFFFFF", width =.6)+
#   theme_foundation(base_size = base_size,
#                     base_family = base_family)+
#   theme_morant_obscuro()

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
theme_morant_obscuro <- function(base_size = 12, base_family = "Poppins",
                                 fondo="#4F0212") {

  colores <- tibble::deframe(temario::colores[["morant"]])

  theme_foundation(base_size = base_size,
                   base_family = base_family)+
  theme(
    rect = element_rect(fill = fondo,
                        linetype = 0,
                        colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x =element_line(colour = "#E9EBDD",
                                     linetype = "dotted"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "#E9EBDD"),
    axis.line = element_line(color = colores["secundario"]),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0,
                              size = rel(1.5),
                              face = "bold",
                              colour = "#F9F4F3"),
    plot.subtitle = element_text(hjust = 0,
                                 size = rel(1),
                                 face = "bold",
                                 colour = colores["gris_claro"]),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.text=element_text(colour =colores["principal"], face = "bold"),
    strip.background = element_rect(fill=colores["secundario"]))
}
